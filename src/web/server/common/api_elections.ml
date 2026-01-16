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
open Belenios_storage_api
open Belenios_server_core
open Belenios_web_api
open Web_common
open Api_generic

let with_administrator token metadata f =
  let@ token = Option.unwrap unauthorized token in
  match lookup_token token with
  | Some (a, _) when Accounts.check a metadata.e_owners -> f a
  | _ -> unauthorized

let find_trustee_id s uuid token =
  let* x = Storage.get s (Election (uuid, State_state)) in
  match Lopt.get_value x with
  | Some (Some (`Decryption tokens)) ->
      let rec find i = function
        | [] -> None
        | t :: ts -> if t = token then Some i else find (i + 1) ts
      in
      Lwt.return (find 1 tokens)
  | _ -> Lwt.return (int_of_string_opt token)

let find_trustee_private_key s uuid trustee_id =
  let* keys = Storage.get s (Election (uuid, Private_keys)) in
  let&* keys = Lopt.get_value keys in
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
  let* status_authentication, status_sealed =
    let* m = Web_persist.get_election_metadata s uuid in
    Lwt.return
      ( Api_drafts.authentication_of_auth_config m.e_auth_config,
        m.e_sealed = Some true )
  in
  let status_auto_archive_date =
    match status_state with
    | `Tallied ->
        let t = Option.value d.e_date_tally ~default:d.e_date_creation in
        Some (t +. (86400. *. Defaults.days_to_archive))
    | _ -> None
  in
  let status_auto_delete_date =
    match status_state with
    | `Draft ->
        let t = d.e_date_creation in
        t +. (86400. *. Defaults.days_to_delete)
    | `Open | `Closed | `Shuffling | `EncryptedTally ->
        let t = Option.value d.e_date_finalization ~default:d.e_date_creation in
        t +. (86400. *. Defaults.days_to_delete)
    | `Tallied ->
        let t = Option.value d.e_date_tally ~default:d.e_date_creation in
        t +. (86400. *. Defaults.(days_to_archive +. days_to_delete))
    | `Archived ->
        let t = Option.value d.e_date_archive ~default:d.e_date_creation in
        t +. (86400. *. Defaults.days_to_delete)
  in
  Lwt.return
    {
      status_state :> state;
      status_authentication;
      status_auto_archive_date;
      status_auto_delete_date;
      status_sealed;
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
        let@ x, set = Storage.update s (Election (uuid, State_state)) in
        match Lopt.get_value x with
        | Some (Some (`Decryption x)) -> Lwt.return x
        | _ -> (
            match metadata.e_trustees with
            | None -> failwith "missing trustees in get_tokens_decrypt"
            | Some ts ->
                let ts = List.map (fun _ -> generate_token ()) ts in
                let* () = set Value (Some (`Decryption ts)) in
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
  let* skipped, token =
    let* x = Storage.get s (Election (uuid, State_state)) in
    match Lopt.get_value x with
    | Some (Some (`Shuffle { skipped; token })) -> Lwt.return (skipped, token)
    | _ -> Lwt.return ([], None)
  in
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
  let@ x, set = Storage.update s (Election (uuid, State_state)) in
  let current, set =
    let ok : Belenios_storage_api.shuffle_token option -> _ = function
      | None -> true
      | Some t when t.tk_trustee = trustee -> true
      | _ -> false
    in
    match Lopt.get_value x with
    | None -> ([], Storage.set s (Election (uuid, State_state)) Value)
    | Some (Some (`Shuffle { skipped; token })) when ok token ->
        (skipped, set Value)
    | _ -> raise @@ Error `NotInExpectedState
  in
  if List.mem trustee current then Lwt.fail @@ Error `NotInExpectedState
  else set (Some (`Shuffle { skipped = trustee :: current; token = None }))

let select_shuffler s uuid metadata tk_trustee =
  let* tk_trustee_id, tk_name = get_trustee_name s uuid metadata tk_trustee in
  let@ x, set = Storage.update s (Election (uuid, State_state)) in
  let* skipped, set =
    match Lopt.get_value x with
    | Some (Some (`Shuffle { skipped; _ })) -> Lwt.return (skipped, set Value)
    | _ -> Lwt.return ([], set Value)
  in
  let tk_token = generate_token () in
  let t : Belenios_storage_api.shuffle_token =
    { tk_trustee; tk_token; tk_trustee_id; tk_name }
  in
  set (Some (`Shuffle { skipped; token = Some t }))

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
  let@ x, set = Storage.update s (Election (uuid, State_state)) in
  match Lopt.get_value x with
  | Some (Some (`Shuffle { skipped; token = Some x })) when token = x.tk_token
    ->
      Lwt.catch
        (fun () ->
          let* y =
            Web_persist.append_to_shuffles s election x.tk_trustee_id shuffle
          in
          match y with
          | Some _ ->
              let* () = set Value (Some (`Shuffle { skipped; token = None })) in
              let* () = Storage.del s (Election (uuid, Audit_cache)) in
              Lwt.return @@ Ok ()
          | None -> Lwt.return @@ Stdlib.Error `Failure)
        (fun e -> Lwt.return @@ Stdlib.Error (`Invalid e))
  | _ -> Lwt.return @@ Stdlib.Error `Forbidden

let get_records s uuid =
  let* x = Web_persist.get_records s uuid in
  Option.value ~default:[] x
  |> List.map (fun (vr_username, vr_date) -> { vr_date; vr_username })
  |> Lwt.return

let cast_ballot send_confirmation s uuid election ~ballot ~user ~precast_data =
  let { user; name; timestamp } : Web_auth_sig.timestamped_user = user in
  let module W = (val election : Election.ELECTION) in
  let* recipient, weight =
    let* x = Web_persist.get_voter s uuid user.user_name in
    match x with
    | Some x -> Lwt.return Voter.(get_recipient x, get_weight x)
    | None -> fail `UnauthorizedVoter
  in
  let* show_weight = Web_persist.get_has_explicit_weights s uuid in
  let oweight = if show_weight then Some weight else None in
  let user_s = string_of_user user in
  let* state = Web_persist.get_election_state s uuid in
  let* voting_open =
    match state with
    | `Open -> Lwt.return_true
    | `Closed -> (
        let* dates = Web_persist.get_election_dates s uuid in
        match
          (dates.e_date_auto_close, dates.e_date_grace_period, timestamp)
        with
        | Some close, Some grace, Some t when t <= close ->
            let now = Unix.gettimeofday () in
            Lwt.return (now -. t <= grace)
        | _ -> Lwt.return_false)
    | _ -> Lwt.return_false
  in
  let* () = if not voting_open then fail `ElectionClosed else Lwt.return_unit in
  let* r =
    Web_persist.cast_ballot s uuid ~ballot ~user:user_s ~weight
      (Unix.gettimeofday ()) ~precast_data
  in
  match r with
  | Ok (hash, revote) ->
      let confirmation : confirmation =
        { recipient; name; hash; revote; weight = oweight; email = false }
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

let dispatch_election ~token ~ifmatch endpoint method_ body s uuid metadata =
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
          let@ account = with_administrator token metadata in
          let@ request = body.run admin_request_of_string in
          match request with
          | (`Open | `Close) as x ->
              let@ () =
               fun cont ->
                if metadata.e_sealed = Some true then forbidden else cont ()
              in
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
              let* () = Storage.archive_election s uuid in
              ok
          | `RegeneratePassword user ->
              let@ () = handle_generic_error in
              let* b =
                Web_persist.regen_password s uuid ~admin_id:account.id user
              in
              if b then ok else not_found
          | `Seal seal ->
              let@ () = handle_generic_error in
              let* () = Web_persist.seal_election s uuid seal in
              ok)
      | _ -> method_not_allowed)
  | [ "audit-cache" ] -> (
      match method_ with
      | `GET ->
          let* x = Web_persist.get_audit_cache s uuid in
          let@ x = Option.unwrap not_found x in
          return_json 200 @@ string_of_audit_cache x
      | _ -> method_not_allowed)
  | [ "sealing-log" ] -> (
      let@ _ = with_administrator token metadata in
      match method_ with
      | `GET ->
          let* path =
            Storage.get_unixfilename s (Election (uuid, Sealing_log))
          in
          Lwt.return @@ `Sealing_log path
      | _ -> method_not_allowed)
  | [ "voters" ] -> (
      let@ _ = with_administrator token metadata in
      match method_ with
      | `GET ->
          let@ () = handle_generic_error in
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
              let* raw = Public_archive.get_election s uuid in
              let@ raw = Option.unwrap not_found raw in
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
              let* raw = Public_archive.get_election s uuid in
              let@ raw = Option.unwrap not_found raw in
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
          let* raw = Public_archive.get_election s uuid in
          let@ raw = Option.unwrap not_found raw in
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
              let lang =
                let sp = Eliom_common.get_sp () in
                let@ lang =
                  Option.bind
                    (Ocsigen_request.header sp.sp_request.request_info
                       (Ocsigen_header.Name.of_string "Accept-Language"))
                in
                Language.of_string_opt lang
              in
              let* state =
                State.create_election s uuid { lang; ballot; precast_data }
              in
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
                cast_ballot send_confirmation s uuid election ~ballot
                  ~user:{ user; name = None; timestamp = None }
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
  | [ "archive-header" ] -> (
      match method_ with
      | `GET -> (
          let@ () = handle_generic_error in
          let* x = Storage.get s (Election (uuid, Archive_header)) in
          match Lopt.get_string x with
          | Some x -> return_json 200 x
          | None -> not_found)
      | _ -> method_not_allowed)
  | [ "last-event" ] -> (
      match method_ with
      | `GET -> (
          let@ () = handle_generic_error in
          let* x = Storage.get s (Election (uuid, Last_event)) in
          match Lopt.get_string x with
          | Some x -> return_json 200 x
          | None -> not_found)
      | _ -> method_not_allowed)
  | [ "roots" ] -> (
      match method_ with
      | `GET ->
          let@ () = handle_generic_error in
          let* x = Public_archive.get_roots s uuid in
          return_json 200 (string_of_roots x)
      | _ -> method_not_allowed)
  | [ "logo" ] -> (
      let set_logo e_logo =
        match metadata.e_sealed with
        | Some true -> forbidden
        | _ ->
            let@ x, set = Storage.update s (Election (uuid, Metadata)) in
            let* () =
              match Lopt.get_value x with
              | Some x -> set Value { x with e_logo }
              | None -> (
                  let@ x, set = Storage.update s (Election (uuid, Draft)) in
                  match Lopt.get_value x with
                  | Some (Draft (v, x)) ->
                      let se_metadata = { x.se_metadata with e_logo } in
                      set Value (Draft (v, { x with se_metadata }))
                  | None -> Lwt.return_unit)
            in
            ok
      in
      match method_ with
      | `GET ->
          let@ () = handle_generic_error in
          let@ logo cont =
            match metadata.e_logo with None -> not_found | Some x -> cont x
          in
          let@ content cont =
            match Base64.decode logo with
            | Ok x -> cont x
            | Error _ -> not_found
          in
          return_generic { mime = "image/png"; content }
      | `PUT ->
          let@ _ = with_administrator token metadata in
          let@ () = handle_generic_error in
          let@ logo = body.run Fun.id in
          if String.length logo <= 10240 then
            match Base64.encode logo with
            | Ok x -> set_logo (Some x)
            | Error _ -> bad_request
          else request_entity_too_large
      | `DELETE ->
          let@ _ = with_administrator token metadata in
          let@ () = handle_generic_error in
          set_logo None
      | _ -> method_not_allowed)
  | _ -> not_found

let dispatch s ~token ~ifmatch endpoint method_ body =
  match endpoint with
  | [] -> (
      let@ token = Option.unwrap unauthorized token in
      let@ account, _ = Option.unwrap unauthorized (lookup_token token) in
      let get () =
        let* elections = Storage.get_elections_by_owner account.id in
        Lwt.return @@ string_of_summary_list elections
      in
      match method_ with
      | `GET -> handle_get get
      | `POST -> (
          let@ () = handle_ifmatch ifmatch get in
          let@ draft = body.run draft_of_string in
          let@ () = handle_generic_error in
          let* uuid = Api_drafts.post_drafts account s draft in
          match uuid with
          | Some uuid -> return_json 200 (string_of_uuid uuid)
          | None -> forbidden)
      | _ -> method_not_allowed)
  | [ uuid; "election" ] -> (
      let@ uuid = Option.unwrap bad_request (Option.wrap Uuid.wrap uuid) in
      let* raw = Public_archive.get_election s uuid in
      match raw with
      | Some raw -> (
          match method_ with
          | `GET -> return_json 200 raw
          | _ -> method_not_allowed)
      | None -> (
          let* se = Storage.get s (Election (uuid, Draft)) in
          match Lopt.get_value se with
          | None -> not_found
          | Some se -> (
              let get () =
                let (Draft (v, se)) = se in
                let version = se.se_version in
                let group = se.se_group in
                let module G = (val Group.of_string ~version group : GROUP) in
                let public_key = G.to_string G.g in
                Lwt.return
                @@ Election.make_raw_election ~version
                     (Template (v, se.se_questions))
                     ~uuid ~group ~public_key
              in
              match method_ with
              | `GET -> handle_get get
              | _ -> method_not_allowed)))
  | [ uuid; "trustees" ] -> (
      let@ uuid = Option.unwrap bad_request (Option.wrap Uuid.wrap uuid) in
      match method_ with
      | `GET ->
          Lwt.try_bind
            (fun () -> Public_archive.get_trustees s uuid)
            (fun raw -> return_json 200 raw)
            (fun _ ->
              let* se = Storage.get s (Election (uuid, Draft)) in
              match Lopt.get_value se with
              | None -> not_found
              | Some (Draft (_, se)) ->
                  let@ trustees cont =
                    match se.se_trustees with
                    | `Basic x ->
                        let ts = x.dbp_trustees in
                        cont
                        @@ List.map
                             (fun { st_public_key; st_name; _ } ->
                               let pk =
                                 trustee_public_key_of_string
                                   Yojson.Safe.read_json Yojson.Safe.read_json
                                   st_public_key
                               in
                               let pk = { pk with trustee_name = st_name } in
                               `Single pk)
                             ts
                    | `Threshold x -> (
                        let ts = x.dtp_trustees in
                        match x.dtp_parameters with
                        | None -> precondition_failed
                        | Some tp ->
                            let tp =
                              threshold_parameters_of_string
                                Yojson.Safe.read_json Yojson.Safe.read_json tp
                            in
                            let named =
                              List.combine
                                (Array.to_list tp.t_verification_keys)
                                ts
                              |> List.map
                                   (fun ((k : _ trustee_public_key), t) ->
                                     { k with trustee_name = t.stt_name })
                              |> Array.of_list
                            in
                            let tp = { tp with t_verification_keys = named } in
                            cont [ `Pedersen tp ])
                  in
                  trustees
                  |> string_of_trustees Yojson.Safe.write_json
                       Yojson.Safe.write_json
                  |> return_json 200)
      | _ -> method_not_allowed)
  | [ uuid; "automatic-dates" ] -> (
      let@ uuid = Option.unwrap bad_request (Option.wrap Uuid.wrap uuid) in
      let get () =
        let* x = Web_persist.get_election_automatic_dates s uuid in
        Lwt.return @@ string_of_election_auto_dates x
      in
      match method_ with
      | `GET -> handle_get get
      | `PUT ->
          let@ metadata cont =
            let* draft = Storage.get s (Election (uuid, Draft)) in
            match Lopt.get_value draft with
            | Some (Draft (_, se)) -> cont se.se_metadata
            | None ->
                let* raw = Public_archive.get_election s uuid in
                let@ _ = Option.unwrap not_found raw in
                let* metadata = Web_persist.get_election_metadata s uuid in
                cont metadata
          in
          let@ _ = with_administrator token metadata in
          let@ () =
           fun cont ->
            if metadata.e_sealed = Some true then forbidden else cont ()
          in
          let@ () = handle_ifmatch ifmatch get in
          let@ d = body.run election_auto_dates_of_string in
          let@ () = handle_generic_error in
          let* () = Web_persist.set_election_automatic_dates s uuid d in
          ok
      | _ -> method_not_allowed)
  | uuid :: "draft" :: endpoint ->
      let@ uuid = Option.unwrap bad_request (Option.wrap Uuid.wrap uuid) in
      let@ se, set = Storage.update s (Election (uuid, Draft)) in
      let set ?(billing = false) ((Draft (_, se) : draft_election) as x) =
        let* () =
          match (billing, se.se_metadata.e_billing_request) with
          | true, _ | _, None -> Lwt.return_unit
          | false, Some id ->
              se.se_metadata <- { se.se_metadata with e_billing_request = None };
              Billing.remove ~id
        in
        set Value x
      in
      let@ se = Option.unwrap not_found (Lopt.get_value se) in
      Api_drafts.dispatch_draft ~token ~ifmatch endpoint method_ body s uuid
        (se, set)
  | [ uuid ] when method_ = `DELETE ->
      let@ uuid = Option.unwrap bad_request (Option.wrap Uuid.wrap uuid) in
      let@ metadata cont =
        let* draft = Storage.get s (Election (uuid, Draft)) in
        match Lopt.get_value draft with
        | Some (Draft (_, se)) -> cont se.se_metadata
        | None ->
            let* raw = Public_archive.get_election s uuid in
            let@ _ = Option.unwrap not_found raw in
            let* metadata = Web_persist.get_election_metadata s uuid in
            cont metadata
      in
      let@ _ = with_administrator token metadata in
      let@ () = handle_generic_error in
      let* () = Storage.delete_election s uuid in
      ok
  | uuid :: endpoint ->
      let@ uuid = Option.unwrap bad_request (Option.wrap Uuid.wrap uuid) in
      let* metadata = Web_persist.get_election_metadata s uuid in
      dispatch_election ~token ~ifmatch endpoint method_ body s uuid metadata
