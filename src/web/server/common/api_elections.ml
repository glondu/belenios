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

let with_administrator token (metadata : metadata) f =
  let@ token = Option.unwrap unauthorized token in
  match lookup_token token with
  | Some (a, _) when Accounts.check a metadata.owners -> f a
  | _ -> unauthorized

let find_trustee_id s token =
  let* x = Storage.E.get s State_state in
  match Lopt.get_value x with
  | Some (Some `Decryption) -> (
      let* metadata = Storage.E.get s Metadata in
      match Lopt.get_value metadata with
      | None -> Lwt.return_none
      | Some metadata -> (
          match metadata.trustees with
          | None -> Lwt.return_none
          | Some trustees ->
              let rec find i = function
                | [] -> None
                | None :: ts -> find (i + 1) ts
                | Some (t : external_trustee) :: ts ->
                    if t.token = token then Some i else find (i + 1) ts
              in
              Lwt.return (find 1 trustees)))
  | _ -> Lwt.return_none

let find_trustee_private_key s (type a b) (w : (a, b) group_witness) trustee_id
    =
  let* keys = Storage.E.get s (Private_keys w) in
  let&* keys = Lopt.get_value keys in
  (* there is one Pedersen trustee *)
  let* trustees = Public_archive.get_trustees s in
  let trustees = !*(trustees_of_yojson Fun.id Fun.id) trustees in
  let rec loop i ts =
    match ts with
    | [] -> Lwt.return_none (* an error, actually *)
    | `Single _ :: ts -> loop (i - 1) ts
    | `Pedersen (p : _ threshold_parameters) :: _ ->
        let algorithm = p.context.algorithm in
        let private_key = List.nth keys i in
        Lwt.return_some
          ({ algorithm; private_key } : (a, b) tally_trustee_content)
  in
  loop (trustee_id - 1) trustees

let with_tally_trustee token s f =
  let@ token = Option.unwrap unauthorized token in
  let* x = find_trustee_id s token in
  match x with Some trustee_id -> f trustee_id | None -> unauthorized

let get_election_status s =
  let* status_state = Web_persist.get_election_state s in
  let* d = Web_persist.get_election_dates s in
  let* status_authentication, status_sealed =
    let* m = Web_persist.get_election_metadata s in
    Lwt.return
      ( Api_drafts.authentication_of_auth_config m.auth_config,
        m.sealed = Some true )
  in
  let status_auto_archive_date =
    match status_state with
    | `Tallied ->
        let t = Option.value d.tally ~default:d.creation in
        Some (t +. (86400. *. Defaults.days_to_archive))
    | _ -> None
  in
  let status_auto_delete_date =
    match status_state with
    | `Draft ->
        let t = d.creation in
        t +. (86400. *. Defaults.days_to_delete)
    | `Open | `Closed | `Shuffling | `EncryptedTally ->
        let t = Option.value d.finalization ~default:d.creation in
        t +. (86400. *. Defaults.days_to_delete)
    | `Tallied ->
        let t = Option.value d.tally ~default:d.creation in
        t +. (86400. *. Defaults.(days_to_archive +. days_to_delete))
    | `Archived ->
        let t = Option.value d.archive ~default:d.creation in
        t +. (86400. *. Defaults.days_to_delete)
  in
  Lwt.return
    {
      state = (status_state :> state);
      authentication = status_authentication;
      auto_archive_date = status_auto_archive_date;
      auto_delete_date = status_auto_delete_date;
      sealed = status_sealed;
    }

let get_partial_decryptions s (metadata : metadata) =
  let@ () =
   fun cont ->
    let* state = Web_persist.get_election_state s in
    match state with
    | `EncryptedTally -> cont ()
    | _ -> Lwt.fail @@ Error `NotInExpectedState
  in
  let* pds = Public_archive.get_partial_decryptions s in
  let* trustees = Public_archive.get_trustees s in
  let trustees = !*(trustees_of_yojson Fun.id Fun.id) trustees in
  let threshold, npks =
    let rec loop (trustees : _ trustee_kind list) threshold npks =
      match trustees with
      | [] -> (threshold, npks)
      | `Single _ :: ts -> loop ts threshold (npks + 1)
      | `Pedersen t :: ts -> (
          match threshold with
          | Some _ -> raise @@ Error (`Unsupported "two Pedersens")
          | None ->
              loop ts (Some t.context.threshold)
                (npks + Array.length t.verification_keys))
    in
    loop trustees None 0
  in
  let trustees =
    let rec loop i ts =
      if i <= npks then
        match ts with
        | t :: ts -> (t, i) :: loop (i + 1) ts
        | [] -> (None, i) :: loop (i + 1) ts
      else []
    in
    match metadata.trustees with None -> loop 1 [] | Some ts -> loop 1 ts
  in
  Lwt.return
    ({
       trustees =
         trustees
         |> List.filter_map (fun (trustee, index) ->
             match trustee with
             | None -> None
             | Some (t : external_trustee) ->
                 Some
                   ({
                      address = t.id;
                      token = t.token;
                      done_ = List.exists (fun x -> x.owner = index) pds;
                    }
                     : trustee_pd));
       threshold;
     }
      : partial_decryptions)

let get_shuffles s (metadata : metadata) =
  let@ () =
   fun cont ->
    let* state = Web_persist.get_election_state s in
    match state with
    | `Shuffling -> cont ()
    | _ -> Lwt.fail @@ Error `NotInExpectedState
  in
  let* shuffles = Public_archive.get_shuffles s in
  let shuffles = Option.value shuffles ~default:[] in
  let* skipped, token =
    let* x = Storage.E.get s State_state in
    match Lopt.get_value x with
    | Some (Some (`Shuffle { skipped; token; _ })) -> Lwt.return (skipped, token)
    | _ -> Lwt.return ([], None)
  in
  Lwt.return
    ({
       shufflers =
         (match metadata.trustees with None -> [ None ] | Some ts -> ts)
         |> List.mapi (fun i (trustee : external_trustee option) ->
             let trustee_id = i + 1 in
             let trustee_with_selected =
               match trustee with
               | None -> None
               | Some trustee ->
                   let selected =
                     match token with
                     | None -> false
                     | Some token -> token.trustee = trustee
                   in
                   Some (trustee, selected)
             in
             ({
                trustee = trustee_with_selected;
                fingerprint =
                  List.find_map
                    (fun (_, o, _) ->
                      if o.owner = trustee_id then Some (Some o.payload)
                      else if
                        List.exists
                          (fun x ->
                            match trustee with
                            | Some t when t.id = x -> true
                            | _ -> false)
                          skipped
                      then Some None
                      else None)
                    shuffles;
              }
               : shuffler));
     }
      : shuffles)

let extract_names trustees =
  trustees
  |> List.map (function
    | `Pedersen x ->
        x.verification_keys |> Array.to_list
        |> List.map (fun (x : _ threshold_verification_key) ->
            x.message.message.name)
    | `Single (x : _ basic_parameters) ->
        [ x.verification_key.message.message.name ])
  |> List.flatten
  |> List.mapi (fun i x -> (i + 1, x))

let get_trustee_names s =
  let* trustees = Public_archive.get_trustees s in
  let trustees = !*(trustees_of_yojson Fun.id Fun.id) trustees in
  Lwt.return (extract_names trustees)

let get_trustee_by_id s (metadata : metadata) id =
  match metadata.trustees with
  | None -> Lwt.return_none
  | Some xs ->
      let rec find = function
        | [] -> Lwt.return_none
        | (x, (index, name)) :: xs -> (
            match x with
            | None -> find xs
            | Some (x : external_trustee) ->
                if x.id = id then Lwt.return_some (x, index, name) else find xs)
      in
      let* names = get_trustee_names s in
      find (List.combine xs names)

let skip_shuffler s trustee =
  let@ x, set = Storage.E.update s State_state in
  let current, set =
    let ok : Belenios_storage_api.shuffle_token option -> _ = function
      | None -> true
      | Some t when t.trustee.id = trustee -> true
      | _ -> false
    in
    match Lopt.get_value x with
    | None -> ([], Storage.E.set s State_state Value)
    | Some (Some (`Shuffle { skipped; token })) when ok token ->
        (skipped, set Value)
    | _ -> raise @@ Error `NotInExpectedState
  in
  if List.mem trustee current then Lwt.fail @@ Error `NotInExpectedState
  else set (Some (`Shuffle { skipped = trustee :: current; token = None }))

let select_shuffler s metadata trustee =
  let@ trustee, trustee_id, name =
   fun cont ->
    let* x = get_trustee_by_id s metadata trustee in
    match x with None -> failwith __FUNCTION__ | Some x -> cont x
  in
  let@ x, set = Storage.E.update s State_state in
  let* skipped, set =
    match Lopt.get_value x with
    | Some (Some (`Shuffle { skipped; _ })) -> Lwt.return (skipped, set Value)
    | _ -> Lwt.return ([], set Value)
  in
  let t : Belenios_storage_api.shuffle_token =
    { trustee; trustee_id; name = Option.value ~default:"N/A" name }
  in
  set (Some (`Shuffle { skipped; token = Some t }))

let post_partial_decryption s (election : Election.t) ~trustee_id
    ~partial_decryption =
  let* pds = Public_archive.get_partial_decryptions s in
  let@ () =
   fun cont ->
    if List.exists (fun x -> x.owner = trustee_id) pds then
      Lwt.return @@ Stdlib.Error `AlreadyDone
    else cont ()
  in
  let module W = (val election) in
  let* pks =
    let* trustees = Public_archive.get_trustees s in
    trustees
    |> !*[%witness_of_yojson (W.G.witness : _ trustees)]
    |> List.map (function
      | `Single (x : _ basic_parameters) ->
          [
            ( x.cert.message.verification,
              x.verification_key.message.message.public_key );
          ]
      | `Pedersen (t : _ threshold_parameters) ->
          List.combine (Array.to_list t.certs)
            (Array.to_list t.verification_keys)
          |> List.map (fun (x, (y : _ threshold_verification_key)) ->
              (x.message.verification, y.message.message.public_key)))
    |> List.flatten |> Array.of_list |> Lwt.return
  in
  let vk, pvk = pks.(trustee_id - 1) in
  let pd =
    !*[%witness_of_yojson (W.G.witness : _ partial_decryption)]
      partial_decryption
  in
  let* et =
    let* x = Public_archive.get_latest_encrypted_tally s in
    match x with
    | None -> assert false
    | Some x -> Lwt.return @@ !*(encrypted_tally_of_yojson !$W.(G.of_string)) x
  in
  if
    !+[%yojson_of_witness (W.G.witness : _ partial_decryption)] pd
    = partial_decryption
    && W.E.check_factor et ~vk ~pvk pd
  then
    let pd = (trustee_id, partial_decryption) in
    let* () = Web_persist.add_partial_decryption s pd in
    Lwt.return @@ Ok ()
  else Lwt.return @@ Stdlib.Error `Invalid

let post_shuffle s (election : Election.t) ~token ~shuffle =
  let@ x, set = Storage.E.update s State_state in
  match Lopt.get_value x with
  | Some (Some (`Shuffle { skipped; token = Some x }))
    when token = x.trustee.token ->
      Lwt.catch
        (fun () ->
          let module W = (val election) in
          let shuffle =
            !*[%witness_of_yojson (W.G.witness : _ shuffle)] shuffle
          in
          let@ vk cont =
            let* trustees = Public_archive.get_trustees s in
            let trustees =
              !*[%witness_of_yojson (W.G.witness : _ trustees)] trustees
            in
            let keys =
              trustees
              |> List.map (function
                | `Single (t : _ basic_parameters) ->
                    [| t.cert.message.verification |]
                | `Pedersen (t : _ threshold_parameters) ->
                    t.certs |> Array.map (fun cert -> cert.message.verification))
              |> Array.concat
            in
            cont @@ keys.(x.trustee_id - 1)
          in
          let* y =
            Web_persist.append_to_shuffles s (module W) x.trustee_id ~vk shuffle
          in
          match y with
          | Some _ ->
              let* () = set Value (Some (`Shuffle { skipped; token = None })) in
              let* () = Storage.E.del s Audit_cache in
              Lwt.return @@ Ok ()
          | None -> Lwt.return @@ Stdlib.Error `Failure)
        (fun e -> Lwt.return @@ Stdlib.Error (`Invalid e))
  | _ -> Lwt.return @@ Stdlib.Error `Forbidden

let get_records s =
  let* x = Web_persist.get_records s in
  Option.value ~default:[] x
  |> List.map (fun (username, date) -> ({ date; username } : voting_record))
  |> Lwt.return

let cast_ballot send_confirmation s (election : Election.t) ~ballot ~user
    ~precast_data =
  let { user; name; timestamp } : Web_auth_sig.timestamped_user = user in
  let module W = (val election) in
  let* recipient, weight =
    let* x = Web_persist.get_voter s user.name in
    match x with
    | Some x -> Lwt.return Voter.(get_recipient x, get_weight x)
    | None -> fail `UnauthorizedVoter
  in
  let* show_weight = Web_persist.get_has_explicit_weights s in
  let oweight = if show_weight then Some weight else None in
  let user_s = string_of_user user in
  let* state = Web_persist.get_election_state s in
  let* voting_open =
    match state with
    | `Open -> Lwt.return_true
    | `Closed -> (
        let* dates = Web_persist.get_election_dates s in
        match (dates.auto_close, dates.grace_period, timestamp) with
        | Some close, Some grace, Some t when t <= close ->
            let now = Unix.gettimeofday () in
            Lwt.return (now -. t <= grace)
        | _ -> Lwt.return_false)
    | _ -> Lwt.return_false
  in
  let* () = if not voting_open then fail `ElectionClosed else Lwt.return_unit in
  let* r =
    Web_persist.cast_ballot s ~ballot ~user:user_s ~weight
      (Unix.gettimeofday ()) ~precast_data
  in
  match r with
  | Ok (hash, revote) ->
      let confirmation : confirmation =
        { recipient; name; hash; revote; weight = oweight; email = false }
      in
      let* email = send_confirmation s confirmation in
      let () =
        if revote then
          let uuid = Storage.E.get_uuid s in
          Printf.ksprintf Ocsigen_messages.accesslog
            "Someone revoted in election %s" (Uuid.to_string uuid)
      in
      Lwt.return { confirmation with email }
  | Error e -> fail e

let state_module = ref None
(* initialized in Web_main *)

let dispatch_election ~token ~ifmatch endpoint method_ body s
    (metadata : metadata) =
  match endpoint with
  | [] -> (
      let get () =
        let* x = get_election_status s in
        Lwt.return @@ yojson_of_election_status x
      in
      match method_ with
      | `GET -> handle_get get
      | `POST -> (
          let@ () = handle_ifmatch ifmatch get in
          let@ _ = with_administrator token metadata in
          let@ request = body.run !*admin_request_of_yojson in
          match request with
          | (`Open | `Close) as x ->
              let@ () =
               fun cont ->
                if metadata.sealed = Some true then forbidden else cont ()
              in
              let doit =
                match x with
                | `Open -> Web_persist.open_election
                | `Close -> Web_persist.close_election
              in
              let* b = doit s in
              if b then ok else forbidden
          | (`ComputeEncryptedTally | `FinishShuffling) as x ->
              let@ () = handle_generic_error in
              let doit =
                match x with
                | `ComputeEncryptedTally -> Web_persist.compute_encrypted_tally
                | `FinishShuffling -> Web_persist.finish_shuffling
              in
              let* b = doit s in
              if b then ok else forbidden
          | `ReleaseTally ->
              let@ () = handle_generic_error in
              let* () = Web_persist.release_tally s in
              ok
          | `Archive ->
              let@ () = handle_generic_error in
              let* () = Storage.E.archive_election s in
              ok
          | `Seal seal ->
              let@ () = handle_generic_error in
              let* () = Web_persist.seal_election s seal in
              ok)
      | _ -> method_not_allowed)
  | [ "audit-cache" ] -> (
      match method_ with
      | `GET ->
          let* x = Web_persist.get_audit_cache s in
          let@ x = Option.unwrap not_found x in
          return_json 200 @@ !+yojson_of_audit_cache x
      | _ -> method_not_allowed)
  | [ "sealing-log" ] -> (
      let@ _ = with_administrator token metadata in
      match method_ with
      | `GET ->
          let* path = Storage.E.get_unixfilename s Sealing_log in
          Lwt.return @@ `Sealing_log path
      | _ -> method_not_allowed)
  | [ "voters" ] -> (
      let@ _ = with_administrator token metadata in
      match method_ with
      | `GET ->
          let@ () = handle_generic_error in
          let* x = Web_persist.get_all_voters s in
          return_json 200 (!+yojson_of_voter_list x)
      | _ -> method_not_allowed)
  | [ "records" ] -> (
      let@ _ = with_administrator token metadata in
      match method_ with
      | `GET ->
          let@ () = handle_generic_error in
          let* x = get_records s in
          return_json 200 (!+yojson_of_records x)
      | _ -> method_not_allowed)
  | [ "partial-decryptions" ] -> (
      match method_ with
      | `GET ->
          let@ _ = with_administrator token metadata in
          let@ () = handle_generic_error in
          let* x = get_partial_decryptions s metadata in
          return_json 200 (!+yojson_of_partial_decryptions x)
      | _ -> method_not_allowed)
  | [ "trustee" ] -> (
      let* state = Web_persist.get_election_state s in
      match state with
      | `EncryptedTally -> (
          match method_ with
          | `GET ->
              let@ trustee_id = with_tally_trustee token s in
              let@ () = handle_generic_error in
              let* election = Public_archive.get_election s in
              let@ election =
                Option.unwrap not_found (Lopt.get_value election)
              in
              let module W = (val election) in
              let* private_key =
                find_trustee_private_key s W.G.witness trustee_id
              in
              return_json 200
                (!+[%yojson_of_witness (W.G.witness : _ tally_trustee)]
                   private_key)
          | `POST -> (
              let@ trustee_id = with_tally_trustee token s in
              let@ () = handle_generic_error in
              let@ partial_decryption = body.run Fun.id in
              let* election = Public_archive.get_election s in
              let@ election =
                Option.unwrap not_found (Lopt.get_value election)
              in
              let module W = (val election) in
              let* x =
                post_partial_decryption s
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
              let* election = Public_archive.get_election s in
              let@ election =
                Option.unwrap not_found (Lopt.get_value election)
              in
              let* x = post_shuffle s election ~token ~shuffle in
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
              let* x = Public_archive.get_nh_ciphertexts s in
              return_json 200 x)
            (function Election_not_found _ -> not_found | e -> Lwt.reraise e)
      | _ -> method_not_allowed)
  | [ "encrypted-tally" ] -> (
      match method_ with
      | `GET ->
          let@ () = handle_generic_error in
          Lwt.catch
            (fun () ->
              let* x = Public_archive.get_latest_encrypted_tally s in
              match x with None -> not_found | Some x -> return_json 200 x)
            (function Election_not_found _ -> not_found | e -> Lwt.reraise e)
      | _ -> method_not_allowed)
  | [ "shuffles" ] -> (
      match method_ with
      | `GET ->
          let@ _ = with_administrator token metadata in
          let@ () = handle_generic_error in
          let* x = get_shuffles s metadata in
          return_json 200 (!+yojson_of_shuffles x)
      | _ -> method_not_allowed)
  | [ "shuffles"; shuffler ] -> (
      match method_ with
      | `POST -> (
          let@ _ = with_administrator token metadata in
          let@ request = body.run !*shuffler_request_of_yojson in
          let@ () = handle_generic_error in
          match request with
          | `Skip ->
              let* () = skip_shuffler s shuffler in
              ok
          | `Select ->
              let* () = select_shuffler s metadata shuffler in
              ok)
      | _ -> method_not_allowed)
  | [ "ballots" ] -> (
      match method_ with
      | `GET ->
          let@ () = handle_generic_error in
          let* x = Public_archive.get_ballot_hashes s in
          return_json 200 (!+yojson_of_ballots_with_weights x)
      | `POST ->
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
              (fun () -> Web_persist.precast_ballot s ~ballot)
              (function
                | Ok x -> cont x | Error e -> raise @@ Error (`CastError e))
              (function
                | Election_not_found _ -> not_found | e -> Lwt.reraise e)
          in
          let lang =
            let sp = Eliom_common.get_sp () in
            let@ lang =
              Option.bind
                (Ocsigen_request.header sp.sp_request.request_info
                   (Ocsigen_header.Name.of_string "Accept-Language"))
            in
            Language.of_string_opt lang
          in
          let* state = State.create_election s { lang; ballot; precast_data } in
          let json =
            match state with
            | None -> `Assoc []
            | Some state -> `Assoc [ ("state", `String state) ]
          in
          return_json 401 (Json.to_string json)
      | _ -> method_not_allowed)
  | [ "objects"; hash ] -> (
      match method_ with
      | `GET -> (
          let@ () = handle_generic_error in
          let* x = Public_archive.get_data s (Hash.of_hex hash) in
          match x with Some x -> return_json 200 x | None -> not_found)
      | _ -> method_not_allowed)
  | [ "archive-header" ] -> (
      match method_ with
      | `GET -> (
          let@ () = handle_generic_error in
          let* x = Storage.E.get s Archive_header in
          match Lopt.get_string x with
          | Some x -> return_json 200 x
          | None -> not_found)
      | _ -> method_not_allowed)
  | [ "last-event" ] -> (
      match method_ with
      | `GET -> (
          let@ () = handle_generic_error in
          let* x = Storage.E.get s Last_event in
          match Lopt.get_string x with
          | Some x -> return_json 200 x
          | None -> not_found)
      | _ -> method_not_allowed)
  | [ "roots" ] -> (
      match method_ with
      | `GET ->
          let@ () = handle_generic_error in
          let* x = Public_archive.get_roots s in
          return_json 200 (!+yojson_of_roots x)
      | _ -> method_not_allowed)
  | [ "archive.zip" ] -> (
      match method_ with
      | `GET ->
          let@ _ = with_administrator token metadata in
          Lwt.try_bind
            (fun () -> Storage.E.get_unixfilename s Confidential_archive)
            (fun x ->
              return_generic { mime = "application/zip"; content = Path x })
            (function Not_found -> not_found | e -> Lwt.reraise e)
      | _ -> method_not_allowed)
  | [ "logo" ] -> (
      let set_logo logo =
        match metadata.sealed with
        | Some true -> forbidden
        | _ ->
            let@ x, set = Storage.E.update s Metadata in
            let* () =
              match Lopt.get_value x with
              | Some x -> set Value { x with logo }
              | None -> (
                  let@ x, set = Storage.E.update s Draft in
                  match Lopt.get_value x with
                  | Some (W (w, Draft (v, x))) ->
                      let metadata = { x.metadata with logo } in
                      set Value (W (w, Draft (v, { x with metadata })))
                  | None -> Lwt.return_unit)
            in
            ok
      in
      match method_ with
      | `GET ->
          let@ () = handle_generic_error in
          let@ logo cont =
            match metadata.logo with None -> not_found | Some x -> cont x
          in
          let@ content cont =
            match Base64.decode logo with
            | Ok x -> cont (String x)
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

let dispatch ~token ~ifmatch endpoint method_ body =
  match endpoint with
  | [] -> (
      let@ token = Option.unwrap unauthorized token in
      let@ account, _ = Option.unwrap unauthorized (lookup_token token) in
      let get () =
        let* elections = Storage.get_elections_by_owner account.id in
        Lwt.return @@ yojson_of_summary_list elections
      in
      match method_ with
      | `GET -> handle_get get
      | `POST -> (
          let@ () = handle_ifmatch ifmatch get in
          let@ draft = body.run !*draft_of_yojson in
          let@ () = handle_generic_error in
          let* uuid = Api_drafts.post_drafts account draft in
          match uuid with
          | Some uuid -> return_json 200 (!+yojson_of_uuid uuid)
          | None -> forbidden)
      | _ -> method_not_allowed)
  | [ uuid; "election" ] -> (
      let@ uuid = Option.unwrap bad_request (Option.wrap Uuid.of_string uuid) in
      let@ s = Storage.E.with_transaction uuid in
      let* election = Public_archive.get_election s in
      match Lopt.get_string election with
      | Some election -> (
          match method_ with
          | `GET -> return_json 200 election
          | _ -> method_not_allowed)
      | None -> (
          let* se = Storage.E.get s Draft in
          match Lopt.get_value se with
          | None -> not_found
          | Some se -> (
              let get () =
                let (W (_, Draft (v, se))) = se in
                let version = se.version in
                let group = se.group in
                let module G = (val Group.of_string ~version group : GROUP) in
                let public_key = G.to_string G.g in
                Election.make_raw_election ~version
                  (Template (v, se.questions))
                  ~uuid ~group ~public_key
                |> Lwt.return
              in
              match method_ with
              | `GET -> handle_get get
              | _ -> method_not_allowed)))
  | [ uuid; "trustees" ] -> (
      let@ uuid = Option.unwrap bad_request (Option.wrap Uuid.of_string uuid) in
      let@ s = Storage.E.with_transaction uuid in
      match method_ with
      | `GET ->
          Lwt.try_bind
            (fun () -> Public_archive.get_trustees s)
            (fun raw -> return_json 200 raw)
            (fun _ ->
              let* se = Storage.E.get s Draft in
              match Lopt.get_value se with
              | None -> not_found
              | Some (W (w, Draft (_, se))) ->
                  let@ trustees cont =
                    match se.trustees with
                    | `Basic x -> (
                        let ts = x.trustees in
                        try
                          cont
                          @@ List.map
                               (fun (x : _ draft_basic_trustee) ->
                                 match x.parameters with
                                 | None -> raise Exit
                                 | Some x -> `Single x)
                               ts
                        with Exit -> precondition_failed)
                    | `Threshold x -> (
                        match x.parameters with
                        | None -> precondition_failed
                        | Some tp -> cont [ `Pedersen tp ])
                  in
                  trustees
                  |> !+[%yojson_of_witness (w : _ trustees)]
                  |> return_json 200)
      | _ -> method_not_allowed)
  | [ uuid; "automatic-dates" ] -> (
      let@ uuid = Option.unwrap bad_request (Option.wrap Uuid.of_string uuid) in
      let@ s = Storage.E.with_transaction uuid in
      let get () =
        let* x = Web_persist.get_election_automatic_dates s in
        Lwt.return @@ yojson_of_election_auto_dates x
      in
      match method_ with
      | `GET -> handle_get get
      | `PUT ->
          let@ metadata cont =
            let* draft = Storage.E.get s Draft in
            match Lopt.get_value draft with
            | Some (W (_, Draft (_, se))) -> cont se.metadata
            | None ->
                let* raw = Public_archive.get_election s in
                let@ _ = Option.unwrap not_found (Lopt.get_string raw) in
                let* metadata = Web_persist.get_election_metadata s in
                cont metadata
          in
          let@ _ = with_administrator token metadata in
          let@ () =
           fun cont ->
            if metadata.sealed = Some true then forbidden else cont ()
          in
          let@ () = handle_ifmatch ifmatch get in
          let@ d = body.run !*election_auto_dates_of_yojson in
          let@ () = handle_generic_error in
          let* () = Web_persist.set_election_automatic_dates s d in
          ok
      | _ -> method_not_allowed)
  | uuid :: "draft" :: endpoint ->
      let@ uuid = Option.unwrap bad_request (Option.wrap Uuid.of_string uuid) in
      let@ s = Storage.E.with_transaction uuid in
      let@ se, set = Storage.E.update s Draft in
      let@ se = Option.unwrap not_found (Lopt.get_value se) in
      let set ?(billing = false)
          ((W (_, Draft (_, se)) : wrapped_draft_election) as x) =
        let* () =
          match (billing, se.metadata.billing_request) with
          | true, _ | _, None -> Lwt.return_unit
          | false, Some id ->
              se.metadata <- { se.metadata with billing_request = None };
              Billing.remove ~id
        in
        set Value x
      in
      Api_drafts.dispatch_draft ~token ~ifmatch endpoint method_ body s uuid
        (se, set)
  | [ uuid ] when method_ = `DELETE ->
      let@ uuid = Option.unwrap bad_request (Option.wrap Uuid.of_string uuid) in
      let@ s = Storage.E.with_transaction uuid in
      let@ metadata cont =
        let* draft = Storage.E.get s Draft in
        match Lopt.get_value draft with
        | Some (W (_, Draft (_, se))) -> cont se.metadata
        | None ->
            let* election = Public_archive.get_election s in
            let@ _ = Option.unwrap not_found (Lopt.get_string election) in
            let* metadata = Web_persist.get_election_metadata s in
            cont metadata
      in
      let@ _ = with_administrator token metadata in
      let@ () = handle_generic_error in
      let* () = Storage.E.delete_election s in
      ok
  | uuid :: endpoint ->
      let@ uuid = Option.unwrap bad_request (Option.wrap Uuid.of_string uuid) in
      let@ s = Storage.E.with_transaction uuid in
      let* metadata = Web_persist.get_election_metadata s in
      dispatch_election ~token ~ifmatch endpoint method_ body s metadata
