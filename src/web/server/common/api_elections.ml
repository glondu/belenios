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
open Belenios
open Belenios_server_core
open Belenios_api.Serializable_j
open Web_common
open Api_generic

let with_administrator token metadata f =
  let@ token = Option.unwrap unauthorized token in
  match lookup_token token with
  | Some a when Accounts.check a metadata.e_owners -> f a
  | _ -> unauthorized

let find_trustee_id s uuid token =
  let* x = Spool.get s uuid Spool.decryption_tokens in
  match x with
  | None -> Lwt.return (int_of_string_opt token)
  | Some tokens ->
      let rec find i = function
        | [] -> None
        | t :: ts -> if t = token then Some i else find (i + 1) ts
      in
      Lwt.return (find 1 tokens)

let find_trustee_private_key s uuid trustee_id =
  let* keys = Spool.get s uuid Spool.private_keys in
  let&* keys = keys in
  (* there is one Pedersen trustee *)
  let* trustees = Public_archive.get_trustees s uuid in
  let trustees =
    trustees_of_string Yojson.Safe.read_json Yojson.Safe.read_json trustees
  in
  let rec loop i ts =
    match ts with
    | [] -> Lwt.return_none (* an error, actually *)
    | `Single _ :: ts -> loop (i - 1) ts
    | `Pedersen _ :: _ -> Lwt.return_some (List.nth keys i)
  in
  loop (trustee_id - 1) trustees

let with_tally_trustee token s uuid f =
  let@ token = Option.unwrap unauthorized token in
  let* x = find_trustee_id s uuid token in
  match x with Some trustee_id -> f trustee_id | None -> unauthorized

let get_election_status s uuid =
  let* status_state = Web_persist.get_election_state s uuid in
  let* d = Web_persist.get_election_dates s uuid in
  let* status_authentication =
    let* m = Web_persist.get_election_metadata s uuid in
    Lwt.return @@ Api_drafts.authentication_of_auth_config m.e_auth_config
  in
  let status_auto_archive_date =
    match status_state with
    | `Tallied ->
        let t = Option.value d.e_tally ~default:Defaults.tally_date in
        Some
          (Datetime.to_unixfloat
          @@ Period.add t (Period.day Defaults.days_to_archive))
    | _ -> None
  in
  let status_auto_delete_date =
    match status_state with
    | `Open | `Closed | `Shuffling | `EncryptedTally ->
        let t =
          Option.value d.e_finalization ~default:Defaults.validation_date
        in
        Datetime.to_unixfloat
        @@ Period.add t (Period.day Defaults.days_to_delete)
    | `Tallied ->
        let t = Option.value d.e_tally ~default:Defaults.tally_date in
        Datetime.to_unixfloat
        @@ Period.add t (Period.day Defaults.(days_to_archive + days_to_delete))
    | `Archived ->
        let t = Option.value d.e_archive ~default:Defaults.archive_date in
        Datetime.to_unixfloat
        @@ Period.add t (Period.day Defaults.days_to_delete)
  in
  Lwt.return
    {
      status_state :> state;
      status_authentication;
      status_auto_archive_date;
      status_auto_delete_date;
    }

let get_partial_decryptions s uuid metadata =
  let@ () =
   fun cont ->
    let* state = Web_persist.get_election_state s uuid in
    match state with
    | `EncryptedTally -> cont ()
    | _ -> Lwt.fail @@ Error `NotInExpectedState
  in
  let* pds = Public_archive.get_partial_decryptions s uuid in
  let* trustees = Public_archive.get_trustees s uuid in
  let trustees =
    trustees_of_string Yojson.Safe.read_json Yojson.Safe.read_json trustees
  in
  let threshold, npks =
    let rec loop trustees threshold npks =
      match trustees with
      | [] -> (threshold, npks)
      | `Single _ :: ts -> loop ts threshold (npks + 1)
      | `Pedersen t :: ts -> (
          match threshold with
          | Some _ -> raise @@ Error (`Unsupported "two Pedersens")
          | None ->
              loop ts (Some t.t_threshold)
                (npks + Array.length t.t_verification_keys))
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
    match metadata.e_trustees with None -> loop 1 [] | Some ts -> loop 1 ts
  in
  let rec seq i j = if i >= j then [] else i :: seq (i + 1) j in
  let* trustee_tokens =
    match threshold with
    | None -> Lwt.return @@ List.map string_of_int (seq 1 (npks + 1))
    | Some _ -> (
        let* x = Spool.get s uuid Spool.decryption_tokens in
        match x with
        | Some x -> Lwt.return x
        | None -> (
            match metadata.e_trustees with
            | None -> failwith "missing trustees in get_tokens_decrypt"
            | Some ts ->
                let ts = List.map (fun _ -> generate_token ()) ts in
                let* () = Spool.create s uuid Spool.decryption_tokens ts in
                Lwt.return ts))
  in
  Lwt.return
    {
      partial_decryptions_trustees =
        List.combine trustees trustee_tokens
        |> List.map (fun ((name, id), token) ->
               {
                 trustee_pd_address = Option.value name ~default:"";
                 trustee_pd_token = token;
                 trustee_pd_done = List.exists (fun x -> x.owned_owner = id) pds;
               });
      partial_decryptions_threshold = threshold;
    }

let get_shuffles s uuid metadata =
  let@ () =
   fun cont ->
    let* state = Web_persist.get_election_state s uuid in
    match state with
    | `Shuffling -> cont ()
    | _ -> Lwt.fail @@ Error `NotInExpectedState
  in
  let* shuffles = Public_archive.get_shuffles s uuid in
  let shuffles = Option.value shuffles ~default:[] in
  let* skipped = Spool.get s uuid Spool.skipped_shufflers in
  let skipped = Option.value skipped ~default:[] in
  let* token = Web_persist.get_shuffle_token s uuid in
  Lwt.return
    {
      shuffles_shufflers =
        (match metadata.e_trustees with None -> [ "server" ] | Some ts -> ts)
        |> List.mapi (fun i t ->
               let trustee_id = i + 1 in
               {
                 shuffler_address = t;
                 shuffler_fingerprint =
                   List.find_map
                     (fun (_, o, _) ->
                       if o.owned_owner = trustee_id then
                         Some (Hash.to_b64 o.owned_payload)
                       else if List.mem t skipped then Some ""
                       else None)
                     shuffles;
                 shuffler_token =
                   Option.bind token (fun x ->
                       if x.tk_trustee = t then Some x.tk_token else None);
               });
    }

let extract_names trustees =
  trustees
  |> List.map (function
       | `Pedersen x ->
           x.t_verification_keys |> Array.to_list
           |> List.map (fun (x : _ trustee_public_key) -> x.trustee_name)
       | `Single (x : _ trustee_public_key) -> [ x.trustee_name ])
  |> List.flatten
  |> List.mapi (fun i x -> (i + 1, x))

let get_trustee_names s uuid =
  let* trustees = Public_archive.get_trustees s uuid in
  let trustees =
    trustees_of_string Yojson.Safe.read_json Yojson.Safe.read_json trustees
  in
  Lwt.return (extract_names trustees)

let get_trustee_name s uuid metadata trustee =
  match metadata.e_trustees with
  | None -> Lwt.return (1, None)
  | Some xs ->
      let* names = get_trustee_names s uuid in
      Lwt.return (List.assoc trustee (List.combine xs names))

let skip_shuffler s uuid trustee =
  let* x = Web_persist.get_shuffle_token s uuid in
  let* () =
    match x with
    | Some x when x.tk_trustee = trustee -> Spool.del s uuid Spool.shuffle_token
    | None -> Lwt.return_unit
    | _ -> Lwt.fail @@ Error `NotInExpectedState
  in
  let@ current, set =
   fun cont ->
    let* x = Spool.update s uuid Spool.skipped_shufflers in
    match x with
    | None -> cont ([], Spool.create s uuid Spool.skipped_shufflers)
    | Some x -> cont x
  in
  if List.mem trustee current then Lwt.fail @@ Error `NotInExpectedState
  else set (trustee :: current)

let select_shuffler s uuid metadata trustee =
  let* trustee_id, name = get_trustee_name s uuid metadata trustee in
  let* () = Spool.del s uuid Spool.shuffle_token in
  let* _ = Web_persist.gen_shuffle_token s uuid trustee trustee_id name in
  Lwt.return_unit

let post_partial_decryption s uuid election ~trustee_id ~partial_decryption =
  let* pds = Public_archive.get_partial_decryptions s uuid in
  let@ () =
   fun cont ->
    if List.exists (fun x -> x.owned_owner = trustee_id) pds then
      Lwt.return @@ Stdlib.Error `AlreadyDone
    else cont ()
  in
  let module W = (val election : Election.ELECTION) in
  let* pks =
    let* trustees = Public_archive.get_trustees s uuid in
    trustees
    |> trustees_of_string W.(sread G.of_string) W.(sread G.Zq.of_string)
    |> List.map (function
         | `Single x -> [ x ]
         | `Pedersen t -> Array.to_list t.t_verification_keys)
    |> List.flatten |> Array.of_list |> Lwt.return
  in
  let pk = pks.(trustee_id - 1).trustee_public_key in
  let pd =
    partial_decryption_of_string
      W.(sread G.of_string)
      W.(sread G.Zq.of_string)
      partial_decryption
  in
  let* et =
    let* x = Public_archive.get_latest_encrypted_tally s uuid in
    match x with
    | None -> assert false
    | Some x -> Lwt.return @@ encrypted_tally_of_string W.(sread G.of_string) x
  in
  if
    string_of_partial_decryption
      W.(swrite G.to_string)
      W.(swrite G.Zq.to_string)
      pd
    = partial_decryption
    && W.E.check_factor et pk pd
  then
    let pd = (trustee_id, partial_decryption) in
    let* () = Web_persist.add_partial_decryption s uuid pd in
    Lwt.return @@ Ok ()
  else Lwt.return @@ Stdlib.Error `Invalid

let post_shuffle s uuid election ~token ~shuffle =
  let* expected_token = Web_persist.get_shuffle_token s uuid in
  match expected_token with
  | Some x when token = x.tk_token ->
      Lwt.catch
        (fun () ->
          let* y =
            Web_persist.append_to_shuffles s election x.tk_trustee_id shuffle
          in
          match y with
          | Some _ ->
              let* () = Spool.del s uuid Spool.shuffle_token in
              let* () = Spool.del s uuid Spool.audit_cache in
              Lwt.return @@ Ok ()
          | None -> Lwt.return @@ Stdlib.Error `Failure)
        (fun e -> Lwt.return @@ Stdlib.Error (`Invalid e))
  | _ -> Lwt.return @@ Stdlib.Error `Forbidden

let split_voting_record =
  let rex = Re.Pcre.regexp "\"(.*)(\\..*)?\" \".*:(.*)\"" in
  fun x ->
    let s = Re.Pcre.exec ~rex x in
    {
      vr_date =
        Datetime.to_unixfloat @@ Datetime.wrap @@ Re.Pcre.get_substring s 1;
      vr_username = Re.Pcre.get_substring s 3;
    }

let get_records s uuid =
  let* x = Web_persist.get_records s uuid in
  let x = Option.value x ~default:[] in
  Lwt.return @@ List.map split_voting_record x

let cast_ballot send_confirmation s uuid election ~ballot ~user ~precast_data =
  let module W = (val election : Election.ELECTION) in
  let* email, login, weight =
    let* x = Web_persist.get_voter s uuid user.user_name in
    match x with
    | Some x -> Lwt.return @@ Voter.get x
    | None -> fail `UnauthorizedVoter
  in
  let* show_weight = Web_persist.get_has_explicit_weights s uuid in
  let oweight = if show_weight then Some weight else None in
  let user_s = string_of_user user in
  let* state = Web_persist.get_election_state s uuid in
  let voting_open = state = `Open in
  let* () = if not voting_open then fail `ElectionClosed else Lwt.return_unit in
  let* r =
    Web_persist.cast_ballot s uuid ~ballot ~user:user_s ~weight
      (Datetime.now ()) ~precast_data
  in
  match r with
  | Ok (hash, revote) ->
      let confirmation : confirmation =
        {
          user = login;
          recipient = email;
          hash;
          revote;
          weight = oweight;
          email = false;
        }
      in
      let* email = send_confirmation s uuid confirmation in
      let () =
        if revote then
          Printf.ksprintf Ocsigen_messages.accesslog
            "Someone revoted in election %s" (Uuid.unwrap uuid)
      in
      Lwt.return { confirmation with email }
  | Error e -> fail e

let direct_voter_auth = ref (fun _ _ _ -> assert false)
(* initialized in Web_main *)

let state_module = ref None
(* initialized in Web_main *)

let dispatch_election ~token ~ifmatch endpoint method_ body s uuid raw metadata
    =
  match endpoint with
  | [] -> (
      let get () =
        let* x = get_election_status s uuid in
        Lwt.return @@ string_of_election_status x
      in
      match method_ with
      | `GET -> handle_get get
      | `POST -> (
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
              let* b = doit s uuid in
              if b then ok else forbidden
          | (`ComputeEncryptedTally | `FinishShuffling) as x ->
              let@ () = handle_generic_error in
              let doit =
                match x with
                | `ComputeEncryptedTally -> Web_persist.compute_encrypted_tally
                | `FinishShuffling -> Web_persist.finish_shuffling
              in
              let* b = doit s uuid in
              if b then ok else forbidden
          | `ReleaseTally ->
              let@ () = handle_generic_error in
              let* () = Web_persist.release_tally s uuid in
              ok
          | `Archive ->
              let@ () = handle_generic_error in
              let* () = Web_persist.archive_election s uuid in
              ok
          | `RegeneratePassword user ->
              let@ () = handle_generic_error in
              let* b = Web_persist.regen_password s uuid metadata user in
              if b then ok else not_found)
      | `DELETE ->
          let@ () = handle_ifmatch ifmatch get in
          let@ _ = with_administrator token metadata in
          let@ () = handle_generic_error in
          let* () = Web_persist.delete_election s uuid in
          ok
      | _ -> method_not_allowed)
  | [ "audit-cache" ] -> (
      let get () =
        let* x = Web_persist.get_audit_cache s uuid in
        Lwt.return @@ string_of_audit_cache x
      in
      match method_ with `GET -> handle_get get | _ -> method_not_allowed)
  | [ "election" ] -> (
      match method_ with `GET -> return_json 200 raw | _ -> method_not_allowed)
  | [ "archive" ] -> (
      match method_ with
      | `GET ->
          let module S = (val s) in
          let* path = S.get_as_file (Election (uuid, Public_archive)) in
          Lwt.return @@ `Bel path
      | _ -> method_not_allowed)
  | [ "trustees" ] -> (
      let get () = Public_archive.get_trustees s uuid in
      match method_ with `GET -> handle_get get | _ -> method_not_allowed)
  | [ "automatic-dates" ] -> (
      let get () =
        let* x = Web_persist.get_election_automatic_dates s uuid in
        Lwt.return @@ string_of_election_auto_dates x
      in
      match method_ with
      | `GET -> handle_get get
      | `PUT ->
          let@ _ = with_administrator token metadata in
          let@ () = handle_ifmatch ifmatch get in
          let@ d = body.run election_auto_dates_of_string in
          let@ () = handle_generic_error in
          let* () = Web_persist.set_election_automatic_dates s uuid d in
          ok
      | _ -> method_not_allowed)
  | [ "voters" ] -> (
      let@ _ = with_administrator token metadata in
      match method_ with
      | `GET ->
          let@ () = handle_generic_error in
          let module S = (val s) in
          let* x = Web_persist.get_all_voters s uuid in
          return_json 200 (string_of_voter_list x)
      | _ -> method_not_allowed)
  | [ "records" ] -> (
      let@ _ = with_administrator token metadata in
      match method_ with
      | `GET ->
          let@ () = handle_generic_error in
          let* x = get_records s uuid in
          return_json 200 (string_of_records x)
      | _ -> method_not_allowed)
  | [ "partial-decryptions" ] -> (
      match method_ with
      | `GET ->
          let@ _ = with_administrator token metadata in
          let@ () = handle_generic_error in
          let* x = get_partial_decryptions s uuid metadata in
          return_json 200 (string_of_partial_decryptions x)
      | _ -> method_not_allowed)
  | [ "trustee" ] -> (
      let* state = Web_persist.get_election_state s uuid in
      match state with
      | `EncryptedTally -> (
          match method_ with
          | `GET ->
              let@ trustee_id = with_tally_trustee token s uuid in
              let@ () = handle_generic_error in
              let* tally_trustee_private_key =
                find_trustee_private_key s uuid trustee_id
              in
              return_json 200
                (string_of_tally_trustee { tally_trustee_private_key })
          | `POST -> (
              let@ trustee_id = with_tally_trustee token s uuid in
              let@ () = handle_generic_error in
              let@ partial_decryption = body.run Fun.id in
              let module W = (val Election.of_string (module Random) raw) in
              let* x =
                post_partial_decryption s uuid
                  (module W)
                  ~trustee_id ~partial_decryption
              in
              match x with
              | Ok () -> ok
              | Error `AlreadyDone -> conflict
              | Error `Invalid -> bad_request)
          | _ -> method_not_allowed)
      | `Shuffling -> (
          match method_ with
          | `POST -> (
              let@ token = Option.unwrap unauthorized token in
              let@ () = handle_generic_error in
              let@ shuffle = body.run Fun.id in
              let election = Election.of_string (module Random) raw in
              let* x = post_shuffle s uuid election ~token ~shuffle in
              match x with
              | Ok () -> ok
              | Error `Forbidden -> forbidden
              | Error _ -> bad_request)
          | _ -> method_not_allowed)
      | _ -> precondition_failed)
  | [ "nh-ciphertexts" ] -> (
      match method_ with
      | `GET ->
          let@ () = handle_generic_error in
          Lwt.catch
            (fun () ->
              let* x = Public_archive.get_nh_ciphertexts s uuid in
              return_json 200 x)
            (function Election_not_found _ -> not_found | e -> Lwt.reraise e)
      | _ -> method_not_allowed)
  | [ "encrypted-tally" ] -> (
      match method_ with
      | `GET ->
          let@ () = handle_generic_error in
          Lwt.catch
            (fun () ->
              let* x = Public_archive.get_latest_encrypted_tally s uuid in
              match x with None -> not_found | Some x -> return_json 200 x)
            (function Election_not_found _ -> not_found | e -> Lwt.reraise e)
      | _ -> method_not_allowed)
  | [ "shuffles" ] -> (
      match method_ with
      | `GET ->
          let@ _ = with_administrator token metadata in
          let@ () = handle_generic_error in
          let* x = get_shuffles s uuid metadata in
          return_json 200 (string_of_shuffles x)
      | _ -> method_not_allowed)
  | [ "shuffles"; shuffler ] -> (
      match method_ with
      | `POST -> (
          let@ _ = with_administrator token metadata in
          let@ request = body.run shuffler_request_of_string in
          let@ () = handle_generic_error in
          match request with
          | `Skip ->
              let* () = skip_shuffler s uuid shuffler in
              ok
          | `Select ->
              let* () = select_shuffler s uuid metadata shuffler in
              ok)
      | _ -> method_not_allowed)
  | [ "ballots" ] -> (
      match method_ with
      | `GET ->
          let@ () = handle_generic_error in
          let* x = Public_archive.get_ballot_hashes s uuid in
          return_json 200 (string_of_ballots_with_weights x)
      | `POST -> (
          let@ () = handle_generic_error in
          let@ state_module cont =
            match !state_module with
            | None -> failwith "anomaly: state_module not initialized"
            | Some x -> cont x
          in
          let module State = (val state_module : Web_auth_sig.STATE) in
          let@ ballot = body.run Fun.id in
          let@ precast_data cont =
            Lwt.try_bind
              (fun () -> Web_persist.precast_ballot s uuid ~ballot)
              (function
                | Ok x -> cont x | Error e -> raise @@ Error (`CastError e))
              (function
                | Election_not_found _ -> not_found | e -> Lwt.reraise e)
          in
          match token with
          | None ->
              let* state = State.create s uuid { ballot; precast_data } in
              let json =
                match state with
                | None -> `Assoc []
                | Some state -> `Assoc [ ("state", `String state) ]
              in
              return_json 401 (Yojson.Safe.to_string json)
          | Some token ->
              let@ user cont =
                Lwt.catch
                  (fun () ->
                    let json =
                      match Base64.decode token with
                      | Ok x -> Yojson.Safe.from_string x
                      | Error (`Msg x) -> failwith x
                    in
                    let* x = !direct_voter_auth s uuid json in
                    cont x)
                  (fun _ -> unauthorized)
              in
              let send_confirmation _ _ _ = Lwt.return_false in
              let* _ =
                let election = Election.of_string (module Random) raw in
                cast_ballot send_confirmation s uuid election ~ballot ~user
                  ~precast_data
              in
              ok)
      | _ -> method_not_allowed)
  | [ "objects"; hash ] -> (
      match method_ with
      | `GET -> (
          let@ () = handle_generic_error in
          let* x = Public_archive.get_data s uuid (Hash.of_hex hash) in
          match x with Some x -> return_json 200 x | None -> not_found)
      | _ -> method_not_allowed)
  | [ "last-event" ] -> (
      match method_ with
      | `GET -> (
          let@ () = handle_generic_error in
          let* x = Spool.get s uuid Spool.last_event in
          match x with
          | Some x -> return_json 200 (string_of_last_event x)
          | None -> not_found)
      | _ -> method_not_allowed)
  | [ "roots" ] -> (
      match method_ with
      | `GET ->
          let@ () = handle_generic_error in
          let* x = Public_archive.get_roots s uuid in
          return_json 200 (string_of_roots x)
      | _ -> method_not_allowed)
  | _ -> not_found

let dispatch s ~token ~ifmatch endpoint method_ body =
  match endpoint with
  | [] -> (
      let@ token = Option.unwrap unauthorized token in
      let@ account = Option.unwrap unauthorized (lookup_token token) in
      match method_ with
      | `GET ->
          let* elections = Storage.get_elections_by_owner account.id in
          let elections =
            List.fold_left
              (fun accu ({ state; _ } as x) ->
                match state with
                | `Draft -> accu
                | `Open | `Closed | `Shuffling | `EncryptedTally | `Tallied
                | `Archived ->
                    x :: accu)
              [] elections
          in
          return_json 200 (string_of_summary_list elections)
      | _ -> method_not_allowed)
  | uuid :: endpoint ->
      let@ uuid = Option.unwrap bad_request (Option.wrap Uuid.wrap uuid) in
      let* raw = Public_archive.get_election s uuid in
      let@ raw = Option.unwrap not_found raw in
      let* metadata = Web_persist.get_election_metadata s uuid in
      dispatch_election ~token ~ifmatch endpoint method_ body s uuid raw
        metadata
