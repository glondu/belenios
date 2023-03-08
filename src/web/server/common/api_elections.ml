(**************************************************************************)
(*                                BELENIOS                                *)
(*                                                                        *)
(*  Copyright © 2012-2023 Inria                                           *)
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
open Belenios_core.Serializable_j
open Web_serializable_j
open Belenios_api.Serializable_j
open Web_common
open Api_generic

let with_administrator token metadata f =
  let@ token = Option.unwrap unauthorized token in
  match lookup_token token with
  | Some a when Accounts.check a metadata.e_owners -> f a
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
  let* keys = Web_persist.get_private_keys uuid in
  let&* keys in
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
  let* status_state = Web_persist.get_election_state uuid in
  let* d = Web_persist.get_election_dates uuid in
  let status_auto_archive_date =
    match status_state with
    | `Tallied ->
       let t = Option.value d.e_tally ~default:default_tally_date in
       Some (Datetime.to_unixfloat @@ Period.add t (Period.day days_to_archive))
    | _ -> None
  in
  let status_auto_delete_date =
    match status_state with
    | `Open | `Closed | `Shuffling | `EncryptedTally ->
       let t = Option.value d.e_finalization ~default:default_validation_date in
       Datetime.to_unixfloat @@ Period.add t (Period.day days_to_delete)
    | `Tallied ->
       let t = Option.value d.e_tally ~default:default_tally_date in
       Datetime.to_unixfloat @@ Period.add t (Period.day (days_to_archive + days_to_delete))
    | `Archived ->
       let t = Option.value d.e_archive ~default:default_archive_date in
       Datetime.to_unixfloat @@ Period.add t (Period.day days_to_delete)
  in
  let* postpone = Web_persist.get_election_result_hidden uuid in
  Lwt.return {
      status_state;
      status_auto_archive_date;
      status_auto_delete_date;
      status_postpone_date = Option.map Datetime.to_unixfloat postpone;
    }

let get_partial_decryptions uuid metadata =
  let@ () = fun cont ->
    let* state = Web_persist.get_election_state uuid in
    match state with
    | `EncryptedTally -> cont ()
    | _ -> Lwt.fail @@ Error `NotInExpectedState
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
         | Some _ -> raise @@ Error (`Unsupported "two Pedersens")
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
             let ts = List.map (fun _ -> generate_token ()) ts in
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
                   trustee_pd_done = List.exists (fun x -> x.owned_owner = id) pds;
                 }
               )
        end;
      partial_decryptions_threshold = threshold;
    }

let set_postpone_date uuid date =
  let@ date = fun cont ->
    match date with
    | None -> cont None
    | Some t ->
       let t = Datetime.from_unixfloat t in
       let max = Period.add (Datetime.now ()) (Period.day days_to_publish_result) in
       if Datetime.compare t max > 0 then
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
    | _ -> Lwt.fail @@ Error `NotInExpectedState
  in
  let* shuffles = Web_persist.get_shuffles uuid in
  let shuffles = Option.value shuffles ~default:[] in
  let* skipped = Web_persist.get_skipped_shufflers uuid in
  let skipped = Option.value skipped ~default:[] in
  let* token = Web_persist.get_shuffle_token uuid in
  Lwt.return {
      shuffles_shufflers =
        begin
          (match metadata.e_trustees with None -> ["server"] | Some ts -> ts)
          |> List.mapi
               (fun i t ->
                 let trustee_id = i + 1 in
                 {
                   shuffler_address = t;
                   shuffler_fingerprint =
                     List.find_map
                       (fun (_, o, _) ->
                         if o.owned_owner = trustee_id then
                           Some (Hash.to_b64 o.owned_payload)
                         else if List.mem t skipped then
                           Some ""
                         else None
                       ) shuffles;
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
  |> List.mapi (fun i x -> i + 1, x)

let get_trustee_names uuid =
  let open Belenios_core.Serializable_j in
  let* trustees = Web_persist.get_trustees uuid in
  let trustees = trustees_of_string Yojson.Safe.read_json trustees in
  Lwt.return (extract_names trustees)

let get_trustee_name uuid metadata trustee =
  match metadata.e_trustees with
  | None -> Lwt.return (1, None)
  | Some xs ->
     let* names = get_trustee_names uuid in
     Lwt.return (List.assoc trustee (List.combine xs names))

let skip_shuffler uuid trustee =
  let* x = Web_persist.get_shuffle_token uuid in
  let* () =
    match x with
    | Some x when x.tk_trustee = trustee ->
       Web_persist.clear_shuffle_token uuid
    | None -> Lwt.return_unit
    | _ -> Lwt.fail @@ Error `NotInExpectedState
  in
  let* x = Web_persist.get_skipped_shufflers uuid in
  let x = Option.value x ~default:[] in
  if List.mem trustee x then
    Lwt.fail @@ Error `NotInExpectedState
  else
    Web_persist.set_skipped_shufflers uuid (trustee :: x)

let select_shuffler uuid metadata trustee =
  let* trustee_id, name = get_trustee_name uuid metadata trustee in
  let* () = Web_persist.clear_shuffle_token uuid in
  let* _ = Web_persist.gen_shuffle_token uuid trustee trustee_id name in
  Lwt.return_unit

let split_voting_record =
  let rex = Pcre.regexp "\"(.*)(\\..*)?\" \".*:(.*)\"" in
  fun x ->
  let s = Pcre.exec ~rex x in
  {
    vr_date = Datetime.to_unixfloat @@ Datetime.wrap @@ Pcre.get_substring s 1;
    vr_username = Pcre.get_substring s 3;
  }

let get_records uuid =
  let* x = Web_persist.get_records uuid in
  let x = Option.value x ~default:[] in
  Lwt.return @@ List.map split_voting_record x

let cast_ballot send_confirmation election ~rawballot ~user ~precast_data =
  let module W = (val election : Site_common_sig.ELECTION) in
  let uuid = W.election.e_uuid in
  let* voters = Web_persist.get_voters uuid in
  let* email, login, weight =
    let x = SMap.find_opt (String.lowercase_ascii user.user_name) voters.voter_map in
    match x with
    | Some x -> Lwt.return @@ Voter.get x
    | None -> fail UnauthorizedVoter
  in
  let show_weight = voters.has_explicit_weights in
  let oweight = if show_weight then Some weight else None in
  let user_s = string_of_user user in
  let* state = Web_persist.get_election_state uuid in
  let voting_open = state = `Open in
  let* () = if not voting_open then fail ElectionClosed else Lwt.return_unit in
  let* r = Web_persist.cast_ballot election ~rawballot ~user:user_s ~weight (Datetime.now ()) ~precast_data in
  match r with
  | Ok (hash, revote) ->
     let* success = send_confirmation uuid revote login email oweight hash in
     let () =
       if revote then
         Printf.ksprintf Ocsigen_messages.accesslog
           "Someone revoted in election %s" (Uuid.unwrap uuid)
     in
     Lwt.return (user, hash, revote, weight, success)
  | Error e ->
     fail (CastError e)

let direct_voter_auth =
  ref (fun _ _ -> assert false) (* initialized in Web_main *)

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
                 | `Open -> Web_persist.open_election
                 | `Close -> Web_persist.close_election
               in
               let* b = doit uuid in
               if b then ok else forbidden
            | (`ComputeEncryptedTally | `FinishShuffling) as x ->
               let@ () = handle_generic_error in
               let doit =
                 match x with
                 | `ComputeEncryptedTally -> Web_persist.compute_encrypted_tally
                 | `FinishShuffling -> Web_persist.finish_shuffling
               in
               let module W = Belenios.Election.Make (struct let raw_election = raw end) (Random) () in
               let* b = doit (module W) in
               if b then ok else forbidden
            | `ReleaseTally ->
               let@ () = handle_generic_error in
               let* () = Web_persist.release_tally uuid in
               ok
            | `Archive ->
               let@ () = handle_generic_error in
               let* () = Web_persist.archive_election uuid in
               ok
            | `RegeneratePassword user ->
               let@ () = handle_generic_error in
               let module W = Belenios.Election.Make (struct let raw_election = raw end) (Random) () in
               let* b = Web_persist.regen_password (module W) metadata user in
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
          let* () = Web_persist.delete_election uuid in
          ok
       | _ -> method_not_allowed
     end
  | ["election"] ->
     begin
       match method_ with
       | `GET -> Lwt.return (200, raw)
       | _ -> method_not_allowed
     end
  | ["trustees"] ->
     begin
       let get () = Web_persist.get_trustees uuid in
       match method_ with
       | `GET -> handle_get get
       | _ -> method_not_allowed
     end
  | ["automatic-dates"] ->
     begin
       let get () =
         let* x = Web_persist.get_election_automatic_dates uuid in
         Lwt.return @@ string_of_election_auto_dates x
       in
       match method_ with
       | `GET -> handle_get get
       | `PUT ->
          let@ _ = with_administrator token metadata in
          let@ () = handle_ifmatch ifmatch get in
          let@ d = body.run election_auto_dates_of_string in
          let@ () = handle_generic_error in
          let* () = Web_persist.set_election_automatic_dates uuid d in
          ok
       | _ -> method_not_allowed
     end
  | ["voters"] ->
     begin
       let@ _ = with_administrator token metadata in
       match method_ with
       | `GET ->
          let@ () = handle_generic_error in
          let* x = Web_persist.get_voters_file uuid in
          begin
            match x with
            | None -> not_found
            | Some x -> Lwt.return (200, x)
          end
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
            | `Skip -> let* () = skip_shuffler uuid shuffler in ok
            | `Select -> let* () = select_shuffler uuid metadata shuffler in ok
          end
       | _ -> method_not_allowed
     end
  | ["ballots"] ->
     begin
       match method_ with
       | `POST ->
          begin
            let@ token = Option.unwrap unauthorized token in
            let@ user = fun cont ->
              Lwt.catch
                (fun () ->
                  let json =
                    match Base64.decode token with
                    | Ok x -> Yojson.Safe.from_string x
                    | Error (`Msg x) -> failwith x
                  in
                  let* x = !direct_voter_auth uuid json in
                  cont x
                )
                (fun _ -> unauthorized)
            in
            let@ rawballot = body.run Fun.id in
            let@ () = handle_generic_error in
            let send_confirmation _ _ _ _ _ _ = Lwt.return_true in
            let module W = Belenios.Election.Make (struct let raw_election = raw end) (Random) () in
            let* x = Web_persist.precast_ballot (module W) ~rawballot in
            match x with
            | Error _ -> bad_request
            | Ok precast_data ->
               let* _ = cast_ballot send_confirmation (module W) ~rawballot ~user ~precast_data in
               ok
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
          let* elections = Web_persist.get_elections_by_owner account.id in
          let elections =
            List.fold_left
              (fun accu (kind, summary_uuid, date, summary_name) ->
                let summary_date = Datetime.to_unixfloat date in
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
     let@ uuid = Option.unwrap bad_request (Option.wrap Uuid.wrap uuid) in
     let* raw = Web_persist.get_raw_election uuid in
     let@ raw = Option.unwrap not_found raw in
     let* metadata = Web_persist.get_election_metadata uuid in
     dispatch_election ~token ~ifmatch endpoint method_ body uuid raw metadata
