(**************************************************************************)
(*                                BELENIOS                                *)
(*                                                                        *)
(*  Copyright © 2012-2021 Inria                                           *)
(*                                                                        *)
(*  This program is free software: you can redistribute it and/or modify  *)
(*  it under the terms of the GNU Affero General Public License as        *)
(*  published by the Free Software Foundation, either version 3 of the    *)
(*  License, or (at your option) any later version, with the additional   *)
(*  exemption that compiling, linking, and/or using OpenSSL is allowed.   *)
(*                                                                        *)
(*  This program is distributed in the hope that it will be useful, but   *)
(*  WITHOUT ANY WARRANTY; without even the implied warranty of            *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU     *)
(*  Affero General Public License for more details.                       *)
(*                                                                        *)
(*  You should have received a copy of the GNU Affero General Public      *)
(*  License along with this program.  If not, see                         *)
(*  <http://www.gnu.org/licenses/>.                                       *)
(**************************************************************************)

open Lwt.Syntax
open Belenios_core.Common
open Belenios_core.Serializable_builtin_t
open Belenios_core.Serializable_j
open Web_serializable_builtin_t
open Web_serializable_j
open Belenios_api.Serializable_j
open Web_common
open Api_generic

let with_administrator token metadata f =
  let@ token = Option.unwrap unauthorized token in
  match lookup_token token, metadata.e_owner with
  | Some a, Some o when Accounts.check_account a o -> f a
  | _ -> unauthorized

let find_trustee_id uuid token =
  let* x = Web_persist.get_decryption_tokens uuid in
  match x with
  | None -> Lwt.return (int_of_string_opt token)
  | Some tokens ->
     let rec find i = function
       | [] -> None
       | t :: ts -> if t = token then Some i else find (i+1) ts
     in
     Lwt.return (find 1 tokens)

let find_trustee_private_key uuid trustee_id =
  let* x = Web_persist.get_private_keys uuid in
  match x with
  | None -> Lwt.return_none
  | Some keys ->
     (* there is one Pedersen trustee *)
     let* trustees = Web_persist.get_trustees uuid in
     let open Belenios_core.Serializable_j in
     let trustees = trustees_of_string Yojson.Safe.read_json trustees in
     let rec loop i ts =
       match ts with
       | [] -> Lwt.return_none (* an error, actually *)
       | `Single _ :: ts -> loop (i - 1) ts
       | `Pedersen _ :: _ -> Lwt.return_some (List.nth keys i)
     in
     loop (trustee_id - 1) trustees

let with_tally_trustee token uuid f =
  let@ token = Option.unwrap unauthorized token in
  let* x = find_trustee_id uuid token in
  match x with
  | Some trustee_id ->
     let* tally_trustee_private_key = find_trustee_private_key uuid trustee_id in
     f {tally_trustee_private_key}
  | None -> unauthorized

let get_election_status uuid =
  let* s = Web_persist.get_election_state uuid in
  let* d = Web_persist.get_election_dates uuid in
  let status_state =
    match s with
    | `EncryptedTally _ -> `EncryptedTally
    | (`Open | `Closed | `Shuffling | `Tallied | `Archived) as x -> x
  in
  let status_auto_archive_date =
    match status_state with
    | `Tallied ->
       let t = Option.value d.e_tally ~default:default_tally_date in
       Some (unixfloat_of_datetime @@ datetime_add t (day days_to_archive))
    | _ -> None
  in
  let status_auto_delete_date =
    match status_state with
    | `Open | `Closed | `Shuffling | `EncryptedTally ->
       let t = Option.value d.e_finalization ~default:default_validation_date in
       unixfloat_of_datetime @@ datetime_add t (day days_to_delete)
    | `Tallied ->
       let t = Option.value d.e_tally ~default:default_tally_date in
       unixfloat_of_datetime @@ datetime_add t (day (days_to_archive + days_to_delete))
    | `Archived ->
       let t = Option.value d.e_archive ~default:default_archive_date in
       unixfloat_of_datetime @@ datetime_add t (day days_to_delete)
  in
  let* postpone = Web_persist.get_election_result_hidden uuid in
  Lwt.return {
      status_state;
      status_auto_archive_date;
      status_auto_delete_date;
      status_postpone_date = Option.map unixfloat_of_datetime postpone;
    }

let get_election_automatic_dates uuid =
  let* d = Web_persist.get_election_dates uuid in
  Lwt.return
    {
      auto_date_open = Option.map unixfloat_of_datetime d.e_auto_open;
      auto_date_close = Option.map unixfloat_of_datetime d.e_auto_close;
    }

let set_election_state uuid state =
  let* allowed =
    let* state = Web_persist.get_election_state uuid in
    match state with
    | `Open | `Closed -> Lwt.return_true
    | _ -> Lwt.return_false
  in
  if allowed then (
    let* () =
      Web_persist.set_election_state uuid
        (state : [`Open | `Closed] :> Web_serializable_t.election_state)
    in
    let* dates = Web_persist.get_election_dates uuid in
    let* () =
      Web_persist.set_election_dates uuid
        {dates with e_auto_open = None; e_auto_close = None}
    in
    Lwt.return_true
  ) else (
    Lwt.return_false
  )

let open_election uuid = set_election_state uuid `Open
let close_election uuid = set_election_state uuid `Closed

let set_election_auto_dates uuid d =
  let e_auto_open = Option.map datetime_of_unixfloat d.auto_date_open in
  let e_auto_close = Option.map datetime_of_unixfloat d.auto_date_close in
  let* dates = Web_persist.get_election_dates uuid in
  Web_persist.set_election_dates uuid {dates with e_auto_open; e_auto_close}

let perform_server_side_decryption election metadata tally =
  let module W = (val election : Site_common_sig.ELECTION_LWT) in
  let uuid = W.election.e_uuid in
  let tally = encrypted_tally_of_string W.G.read tally in
  let decrypt i =
    let* x = Web_persist.get_private_key uuid in
    match x with
    | Some sk ->
       let* pd = W.E.compute_factor tally sk in
       let pd = string_of_partial_decryption W.G.write pd in
       Web_persist.add_partial_decryption uuid (i, pd)
    | None ->
       Lwt.fail (Error "missing server private key")
  in
  Option.value metadata.e_trustees ~default:["server"]
  |> List.mapi (fun i t -> i, t)
  |> Lwt_list.iter_s
       (fun (i, t) ->
         if t = "server" then decrypt (i + 1) else Lwt.return_unit
       )

let get_partial_decryptions uuid metadata =
  let@ () = fun cont ->
    let* state = Web_persist.get_election_state uuid in
    match state with
    | `EncryptedTally _ -> cont ()
    | _ -> Lwt.fail @@ Error "not in state EncryptedTally"
  in
  let open Belenios_core.Serializable_j in
  let* pds = Web_persist.get_partial_decryptions uuid in
  let* trustees = Web_persist.get_trustees uuid in
  let trustees = trustees_of_string Yojson.Safe.read_json trustees in
  let threshold, npks =
    let rec loop trustees threshold npks =
      match trustees with
      | [] -> threshold, npks
      | `Single _ :: ts -> loop ts threshold (npks + 1)
      | `Pedersen t :: ts ->
         match threshold with
         | Some _ -> raise @@ Error "unsupported: two Pedersen"
         | None -> loop ts (Some t.t_threshold) (npks + Array.length t.t_verification_keys)
    in
    loop trustees None 0
  in
  let trustees =
    let rec loop i ts =
      if i <= npks then
        match ts with
        | t :: ts -> (Some t, i) :: loop (i + 1) ts
        | [] -> (None, i) :: loop (i + 1) ts
      else []
    in
    match metadata.e_trustees with
    | None -> loop 1 []
    | Some ts -> loop 1 ts
  in
  let rec seq i j = if i >= j then [] else i :: seq (i + 1) j in
  let* trustee_tokens =
    match threshold with
    | None -> Lwt.return @@ List.map string_of_int (seq 1 (npks + 1))
    | Some _ ->
       let* x = Web_persist.get_decryption_tokens uuid in
       match x with
       | Some x -> Lwt.return x
       | None ->
          match metadata.e_trustees with
          | None -> failwith "missing trustees in get_tokens_decrypt"
          | Some ts ->
             let* ts = Lwt_list.map_s (fun _ -> generate_token ()) ts in
             let* () = Web_persist.set_decryption_tokens uuid ts in
             Lwt.return ts
  in
  Lwt.return {
      partial_decryptions_trustees =
        begin
          List.combine trustees trustee_tokens
          |> List.map
               (fun ((name, id), token) ->
                 {
                   trustee_pd_address = Option.value name ~default:"";
                   trustee_pd_token = token;
                   trustee_pd_done = List.mem_assoc id pds;
                 }
               )
        end;
      partial_decryptions_threshold = threshold;
    }


let transition_to_encrypted_tally election metadata tally =
  let module W = (val election : Site_common_sig.ELECTION_LWT) in
  let uuid = W.election.e_uuid in
  let* () = perform_server_side_decryption election metadata tally in
  Web_persist.set_election_state uuid @@ `EncryptedTally (0, 0, "")

let compute_encrypted_tally election metadata =
  let module W = (val election : Site_common_sig.ELECTION_LWT) in
  let uuid = W.election.e_uuid in
  let* state = Web_persist.get_election_state uuid in
  match state with
  | `Closed ->
     let* tally = Web_persist.compute_encrypted_tally election in
     if Belenios.Election.has_nh_questions W.election then (
       let* () = Web_persist.set_election_state uuid `Shuffling in
       Lwt.return_true
     ) else (
       let* () = transition_to_encrypted_tally election metadata tally in
       Lwt.return_true
     )
  | _ -> Lwt.return_false

let finish_shuffling election metadata =
  let module W = (val election : Site_common_sig.ELECTION_LWT) in
  let uuid = W.election.e_uuid in
  let* state = Web_persist.get_election_state uuid in
  match state with
  | `Shuffling ->
     begin
       let* x = Web_persist.compute_encrypted_tally_after_shuffling election in
       match x with
       | None -> Lwt.fail (Error "compute_encrypted_tally_after_shuffling")
       | Some tally ->
          let* () = transition_to_encrypted_tally election metadata tally in
          Lwt.return_true
     end
  | _ -> Lwt.return_false

let release_tally election =
  let module W = (val election : Site_common_sig.ELECTION_LWT) in
  let uuid = W.election.e_uuid in
  let@ () = fun cont ->
    let* state = Web_persist.get_election_state uuid in
    match state with
    | `EncryptedTally _ -> cont ()
    | _ -> Lwt.return @@ Stdlib.Error `Forbidden
  in
  let* ntallied =
    let* hashes = Web_persist.get_ballot_hashes uuid in
    let weights = List.map snd hashes in
    let open Weight in
    Lwt.return @@ List.fold_left ( + ) zero weights
  in
  let@ et = fun cont ->
    let* x = read_file ~uuid (string_of_election_file ESETally) in
    match x with
    | Some [et] ->
       begin
         match Option.wrap (encrypted_tally_of_string W.G.read) et with
         | None -> Lwt.fail (Error "release_tally: encrypted_tally_of_string")
         | Some x -> cont x
       end
    | _ -> Lwt.fail (Error "release_tally")
  in
  let open Belenios_core.Serializable_j in
  let* trustees = Web_persist.get_trustees uuid in
  let trustees = trustees_of_string W.G.read trustees in
  let* pds = Web_persist.get_partial_decryptions uuid in
  let pds = List.map snd pds in
  let pds = List.map (partial_decryption_of_string W.G.read) pds in
  let* shuffles, shufflers =
    let* x = Web_persist.get_shuffles uuid in
    match x with
    | None -> Lwt.return (None, None)
    | Some s ->
       let s = List.map (shuffle_of_string W.G.read) s in
       let* x = Web_persist.get_shuffle_hashes uuid in
       match x with
       | None -> Lwt.return (Some s, None)
       | Some x ->
          let x =
            x
            |> List.map (fun x -> if x.sh_hash = "" then [] else [x.sh_name])
            |> List.flatten
          in
          assert (List.length s = List.length x);
          Lwt.return (Some s, Some x)
  in
  match W.E.compute_result ?shuffles ?shufflers ntallied et pds trustees with
  | Ok result ->
     let* () =
       let result =
         string_of_election_result
           W.write_result (write_encrypted_tally W.G.write)
           (write_partial_decryption W.G.write) (write_shuffle W.G.write)
           result
       in
       write_file ~uuid (string_of_election_file ESResult) [result]
     in
     let* () = Web_persist.remove_audit_cache uuid in
     let* () = Web_persist.set_election_state uuid `Tallied in
     let* dates = Web_persist.get_election_dates uuid in
     let* () = Web_persist.set_election_dates uuid {dates with e_tally = Some (now ())} in
     let* () = cleanup_file (uuid /// "decryption_tokens.json") in
     let* () = cleanup_file (uuid /// "shuffles.jsons") in
     let* () = Web_persist.clear_shuffle_token uuid in
     Lwt.return @@ Ok ()
  | Error e -> Lwt.return @@ Stdlib.Error (`CombinationError e)

let delete_sensitive_data uuid =
  let* () = cleanup_file (uuid /// "state.json") in
  let* () = cleanup_file (uuid /// "decryption_tokens.json") in
  let* () = cleanup_file (uuid /// "partial_decryptions.json") in
  let* () = cleanup_file (uuid /// "extended_records.jsons") in
  let* () = cleanup_file (uuid /// "credential_mappings.jsons") in
  let* () = rmdir (uuid /// "ballots") in
  let* () = cleanup_file (uuid /// "ballots_index.json") in
  let* () = cleanup_file (uuid /// "private_key.json") in
  let* () = cleanup_file (uuid /// "private_keys.jsons") in
  Lwt.return_unit

let archive_election uuid =
  let* () = delete_sensitive_data uuid in
  let* dates = Web_persist.get_election_dates uuid in
  Web_persist.set_election_dates uuid {dates with e_archive = Some (now ())}

let delete_election election metadata =
  let module W = (val election : Site_common_sig.ELECTION_LWT) in
  let uuid = W.election.e_uuid in
  let* () = delete_sensitive_data uuid in
  let de_template =
    {
      t_description = "";
      t_name = W.election.e_name;
      t_questions = Array.map Belenios_core.Question.erase_question W.election.e_questions;
      t_administrator = None;
      t_credential_authority = None;
    }
  in
  let de_owner = Option.value metadata.e_owner ~default:(`Id []) in
  let* dates = Web_persist.get_election_dates uuid in
  let de_date =
    match dates.e_tally with
    | Some x -> x
    | None ->
       match dates.e_finalization with
       | Some x -> x
       | None ->
          match dates.e_creation with
          | Some x -> x
          | None -> default_validation_date
  in
  let de_authentication_method = match metadata.e_auth_config with
    | Some [{auth_system = "cas"; auth_config; _}] ->
       let server = List.assoc "server" auth_config in
       `CAS server
    | Some [{auth_system = "password"; _}] -> `Password
    | _ -> `Unknown
  in
  let de_credential_method = match metadata.e_cred_authority with
    | Some "server" -> `Automatic
    | _ -> `Manual
  in
  let* de_trustees =
    let open Belenios_core.Serializable_j in
    let* trustees = Web_persist.get_trustees uuid in
    trustees_of_string Yojson.Safe.read_json trustees
    |> List.map
         (function
          | `Single _ -> `Single
          | `Pedersen t -> `Pedersen (t.t_threshold, Array.length t.t_verification_keys)
         )
    |> Lwt.return
  in
  let* voters = Web_persist.get_voters uuid in
  let* ballots = Web_persist.get_ballot_hashes uuid in
  let* result = Web_persist.get_election_result uuid in
  let de_nb_voters, de_has_weights =
    match voters with
    | None -> 0, false
    | Some voters ->
       List.length voters,
       List.exists
         (fun x ->
           let _, _, weight = split_identity_opt x in
           weight <> None
         ) voters
  in
  let de = {
      de_uuid = uuid;
      de_template;
      de_owner;
      de_nb_voters;
      de_nb_ballots = List.length ballots;
      de_date;
      de_tallied = result <> None;
      de_authentication_method;
      de_credential_method;
      de_trustees;
      de_has_weights;
    }
  in
  let* () = write_file ~uuid "deleted.json" [string_of_deleted_election de] in
  let files_to_delete = [
      "election.json";
      "ballots.jsons";
      "dates.json";
      "encrypted_tally.json";
      "metadata.json";
      "passwords.csv";
      "public_creds.txt";
      "trustees.json";
      "records";
      "result.json";
      "hide_result";
      "shuffle_token";
      "shuffles.jsons";
      "voters.txt";
      "archive.zip";
      "audit_cache.json";
    ]
  in
  let* () =
    Lwt_list.iter_p
      (fun x ->
        cleanup_file (uuid /// x)
      ) files_to_delete
  in
  Web_persist.clear_elections_by_owner_cache ()

let find_user_id uuid user =
  let db = Lwt_io.lines_of_file (uuid /// "voters.txt") in
  let* db = Lwt_stream.to_list db in
  let rec loop = function
    | [] -> None
    | id :: xs ->
       let _, login, _ = split_identity id in
       if String.lowercase_ascii login = user then Some id else loop xs
  in
  let show_weight =
    List.exists
      (fun x ->
        let _, _, weight = split_identity_opt x in
        weight <> None
      ) db
  in
  Lwt.return (loop db, show_weight)

let load_password_db uuid =
  let db = uuid /// "passwords.csv" in
  Lwt_preemptive.detach Csv.load db

let rec replace_password username ((salt, hashed) as p) = function
  | [] -> []
  | ((username' :: _ :: _ :: rest) as x) :: xs ->
     if username = String.lowercase_ascii username' then
       (username' :: salt :: hashed :: rest) :: xs
     else
       x :: (replace_password username p xs)
  | x :: xs -> x :: (replace_password username p xs)

let regenpwd election metadata user =
  let user = String.lowercase_ascii user in
  let module W = (val election : Site_common_sig.ELECTION_LWT) in
  let uuid = W.election.e_uuid in
  let title = W.election.e_name in
  let* x = find_user_id uuid user in
  match x with
  | Some id, show_weight ->
     let langs = get_languages metadata.e_languages in
     let* db = load_password_db uuid in
     let* email, x =
       Mails_voter.generate_password_email metadata langs title uuid
       id show_weight
     in
     let* () = Mails_voter.submit_bulk_emails [email] in
     let db = replace_password user x db in
     let* () = Api_drafts.dump_passwords uuid db in
     Lwt.return_true
  | _ -> Lwt.return_false

let set_postpone_date uuid date =
  let@ date = fun cont ->
    match date with
    | None -> cont None
    | Some t ->
       let t = datetime_of_unixfloat t in
       let max = datetime_add (now ()) (day days_to_publish_result) in
       if datetime_compare t max > 0 then
         Lwt.return_false
       else
         cont (Some t)
  in
  let* () = Web_persist.set_election_result_hidden uuid date in
  Lwt.return_true

let get_shuffles uuid metadata =
  let@ () = fun cont ->
    let* state = Web_persist.get_election_state uuid in
    match state with
    | `Shuffling -> cont ()
    | _ -> Lwt.fail @@ Error "not in state Shuffling"
  in
  let* hashes = Web_persist.get_shuffle_hashes uuid in
  let hashes = Option.value hashes ~default:[] in
  let* token = Web_persist.get_shuffle_token uuid in
  Lwt.return {
      shuffles_shufflers =
        begin
          (match metadata.e_trustees with None -> ["server"] | Some ts -> ts)
          |> List.map
               (fun t ->
                 {
                   shuffler_address = t;
                   shuffler_fingerprint =
                     hashes
                     |> List.find_opt (fun x -> x.sh_trustee = t)
                     |> Option.map (fun x -> x.sh_hash);
                   shuffler_token =
                     Option.bind token
                       (fun x -> if x.tk_trustee = t then Some x.tk_token else None);
                 }
               )
        end;
    }

let extract_names trustees =
  let open Belenios_core.Serializable_t in
  trustees
  |> List.map
       (function
        | `Pedersen x ->
           x.t_verification_keys
           |> Array.to_list
           |> List.map (fun x -> x.trustee_name)
        | `Single x -> [x.trustee_name]
       )
  |> List.flatten

let get_trustee_names uuid =
  let open Belenios_core.Serializable_j in
  let* trustees = Web_persist.get_trustees uuid in
  let trustees = trustees_of_string Yojson.Safe.read_json trustees in
  Lwt.return (extract_names trustees)

let get_trustee_name uuid metadata trustee =
  match metadata.e_trustees with
  | None -> Lwt.return_none
  | Some xs ->
     let* names = get_trustee_names uuid in
     Lwt.return (List.assoc trustee (List.combine xs names))

let skip_shuffler uuid metadata trustee =
  let* sh_name = get_trustee_name uuid metadata trustee in
  let* () = Web_persist.clear_shuffle_token uuid in
  let sh = {sh_trustee = trustee; sh_hash = ""; sh_name} in
  let* () = Web_persist.add_shuffle_hash uuid sh in
  Lwt.return_unit

let select_shuffler uuid metadata trustee =
  let* name = get_trustee_name uuid metadata trustee in
  let* () = Web_persist.clear_shuffle_token uuid in
  let* _ = Web_persist.gen_shuffle_token uuid trustee name in
  Lwt.return_unit

let split_voting_record =
  let rex = Pcre.regexp "\"(.*)(\\..*)?\" \".*:(.*)\"" in
  fun x ->
  let s = Pcre.exec ~rex x in
  {
    vr_date = unixfloat_of_datetime @@ raw_datetime_of_string @@ Pcre.get_substring s 1;
    vr_username = Pcre.get_substring s 3;
  }

let get_records uuid =
  let* x = read_file ~uuid (string_of_election_file ESRecords) in
  let x = Option.value x ~default:[] in
  Lwt.return @@ List.map split_voting_record x

let dispatch_election ~token ~ifmatch endpoint method_ body uuid raw metadata =
  match endpoint with
  | [] ->
     begin
       let get () =
         let* x = get_election_status uuid in
         Lwt.return @@ string_of_election_status x
       in
       match method_ with
       | `GET -> handle_get get
       | `POST ->
          begin
            let@ () = handle_ifmatch ifmatch get in
            let@ _ = with_administrator token metadata in
            let@ request = body.run admin_request_of_string in
            match request with
            | (`Open | `Close) as x ->
               let doit =
                 match x with
                 | `Open -> open_election
                 | `Close -> close_election
               in
               let* b = doit uuid in
               if b then ok else forbidden
            | (`ComputeEncryptedTally | `FinishShuffling) as x ->
               let@ () = handle_generic_error in
               let doit =
                 match x with
                 | `ComputeEncryptedTally -> compute_encrypted_tally
                 | `FinishShuffling -> finish_shuffling
               in
               let module W = Belenios.Election.Make (struct let raw_election = raw end) (LwtRandom) () in
               let* b = doit (module W) metadata in
               if b then ok else forbidden
            | `ReleaseTally ->
               begin
                 let@ () = handle_generic_error in
                 let module W = Belenios.Election.Make (struct let raw_election = raw end) (LwtRandom) () in
                 let* b = release_tally (module W) in
                 match b with
                 | Ok () -> ok
                 | Error `Forbidden -> forbidden
                 | Error (`CombinationError e) ->
                    let msg =
                      Printf.sprintf "combination error: %s"
                        (Belenios.Trustees.string_of_combination_error e)
                    in
                    Lwt.fail @@ Error msg
               end
            | `Archive ->
               let@ () = handle_generic_error in
               let* () = archive_election uuid in
               ok
            | `RegeneratePassword user ->
               let@ () = handle_generic_error in
               let module W = Belenios.Election.Make (struct let raw_election = raw end) (LwtRandom) () in
               let* b = regenpwd (module W) metadata user in
               if b then ok else not_found
            | `SetPostponeDate date ->
               let@ () = handle_generic_error in
               let* b = set_postpone_date uuid date in
               if b then ok else bad_request
          end
       | `DELETE ->
          let@ () = handle_ifmatch ifmatch get in
          let@ _ = with_administrator token metadata in
          let@ () = handle_generic_error in
          let module W = Belenios.Election.Make (struct let raw_election = raw end) (LwtRandom) () in
          let* () = delete_election (module W) metadata in
          ok
       | _ -> method_not_allowed
     end
  | ["election"] ->
     begin
       match method_ with
       | `GET -> Lwt.return (200, raw)
       | _ -> method_not_allowed
     end
  | ["automatic-dates"] ->
     begin
       let get () =
         let* x = get_election_automatic_dates uuid in
         Lwt.return @@ string_of_election_auto_dates x
       in
       match method_ with
       | `GET -> handle_get get
       | `PUT ->
          let@ _ = with_administrator token metadata in
          let@ () = handle_ifmatch ifmatch get in
          let@ d = body.run election_auto_dates_of_string in
          let@ () = handle_generic_error in
          let* () = set_election_auto_dates uuid d in
          ok
       | _ -> method_not_allowed
     end
  | ["voters"] ->
     begin
       let@ _ = with_administrator token metadata in
       match method_ with
       | `GET ->
          let@ () = handle_generic_error in
          let* x = read_file ~uuid (string_of_election_file ESVoters) in
          let x = Option.value x ~default:[] in
          Lwt.return (200, string_of_voter_list x)
       | _ -> method_not_allowed
     end
  | ["records"] ->
     begin
       let@ _ = with_administrator token metadata in
       match method_ with
       | `GET ->
          let@ () = handle_generic_error in
          let* x = get_records uuid in
          Lwt.return (200, string_of_records x)
       | _ -> method_not_allowed
     end
  | ["partial-decryptions"] ->
     begin
       match method_ with
       | `GET ->
          let@ _ = with_administrator token metadata in
          let@ () = handle_generic_error in
          let* x = get_partial_decryptions uuid metadata in
          Lwt.return (200, string_of_partial_decryptions x)
       | _ -> method_not_allowed
     end
  | ["tally-trustee"] ->
     begin
       match method_ with
       | `GET ->
          let@ x = with_tally_trustee token uuid in
          let@ () = handle_generic_error in
          Lwt.return (200, string_of_tally_trustee x)
       | _ -> method_not_allowed
     end
  | ["shuffles"] ->
     begin
       match method_ with
       | `GET ->
          let@ _ = with_administrator token metadata in
          let@ () = handle_generic_error in
          let* x = get_shuffles uuid metadata in
          Lwt.return (200, string_of_shuffles x)
       | _ -> method_not_allowed
     end
  | ["shuffles"; shuffler] ->
     begin
       match method_ with
       | `POST ->
          begin
            let@ _ = with_administrator token metadata in
            let@ request = body.run shuffler_request_of_string in
            let@ () = handle_generic_error in
            match request with
            | `Skip -> let* () = skip_shuffler uuid metadata shuffler in ok
            | `Select -> let* () = select_shuffler uuid metadata shuffler in ok
          end
       | _ -> method_not_allowed
     end
  | _ -> not_found

let dispatch ~token ~ifmatch endpoint method_ body =
  match endpoint with
  | [] ->
     begin
       let@ token = Option.unwrap unauthorized token in
       let@ account = Option.unwrap unauthorized (lookup_token token) in
       match method_ with
       | `GET ->
          let* elections = Web_persist.get_elections_by_owner account.account_id in
          let elections =
            List.fold_left
              (fun accu (kind, summary_uuid, date, summary_name) ->
                let summary_date = unixfloat_of_datetime date in
                match kind with
                | `Draft -> accu
                | (`Validated | `Tallied | `Archived) as x ->
                   let summary_kind = Some x in
                   {summary_uuid; summary_name; summary_date; summary_kind} :: accu
              ) [] elections
          in
          Lwt.return (200, string_of_summary_list elections)
       | _ -> method_not_allowed
     end
  | uuid :: endpoint ->
     let@ uuid = Option.unwrap bad_request (Option.wrap uuid_of_raw_string uuid) in
     let* raw = Web_persist.get_raw_election uuid in
     let@ raw = Option.unwrap not_found raw in
     let* metadata = Web_persist.get_election_metadata uuid in
     dispatch_election ~token ~ifmatch endpoint method_ body uuid raw metadata
