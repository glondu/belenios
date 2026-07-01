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

open Lwt
open Lwt.Syntax
open Belenios
open Belenios_web_api
open Belenios_storage_api
open Belenios_server_core
open Web_common

let get_election_dates s =
  let* x = Storage.E.get s Dates in
  Lwt.return
    (Option.value ~default:Belenios_storage_api.default_election_dates
       (Lopt.get_value x))

let update_election_dates s cont =
  let@ x, set = Storage.E.update s Dates in
  cont
    ( Option.value ~default:Belenios_storage_api.default_election_dates
        (Lopt.get_value x),
      set Value )

let empty_metadata =
  {
    owners = [];
    auth_config = None;
    cred_authority = None;
    cred_authority_info = None;
    trustees = None;
    languages = None;
    contact = None;
    booth_version = None;
    billing_request = None;
    sealed = None;
    logo = None;
  }

let get_election_metadata s =
  let* x = Storage.E.get s Metadata in
  match Lopt.get_value x with
  | Some x -> Lwt.return x
  | None -> (
      let* x = Storage.E.get s Draft in
      match Lopt.get_value x with
      | Some (W (_, Draft (_, x))) -> Lwt.return x.metadata
      | None -> Lwt.return empty_metadata)

let seal_election s seal =
  let@ x, set = Storage.E.update s Metadata in
  match Lopt.get_value x with
  | None -> raise Not_found
  | Some metadata -> (
      let* x = Storage.E.get s Dates in
      match Lopt.get_value x with
      | None -> raise Not_found
      | Some dates ->
          let op, sealed =
            if seal then
              ( `Seal
                  {
                    date_open = dates.auto_open;
                    date_close = dates.auto_close;
                    date_publish = dates.publish;
                  },
                Some true )
            else (`Unseal, None)
          in
          let date = Unix.gettimeofday () in
          let* b = Storage.E.append_sealing s { date; op } in
          let* () = Storage.E.del s Audit_cache in
          if b then set Value { metadata with sealed }
          else failwith "sealing error")

let append_to_shuffles s (type a b) (election : (a, b) Election.u) owner
    ~(vk : a) (shuffle : (a, b) shuffle) =
  let module W = (val election) in
  let@ last cont =
    let* x = Storage.E.get s Last_event in
    match Lopt.get_value x with None -> assert false | Some x -> cont x
  in
  let shuffle_s = !+[%yojson_of_witness (W.G.witness : _ shuffle)] shuffle in
  let shuffle_h = Hash.hash_string shuffle_s in
  let* last_nh = Public_archive.get_nh_ciphertexts s in
  let last_nh = !*(nh_ciphertexts_of_yojson !$W.G.of_string) last_nh in
  if
    !+[%yojson_of_witness (W.G.witness : _ shuffle)] shuffle = shuffle_s
    && W.E.check_shuffle ~vk last_nh shuffle
  then
    let owned = { owner; payload = shuffle_h } in
    let owned_s = !+(yojson_of_owned yojson_of_hash) owned in
    let* x =
      Storage.E.append s ~last
        [
          Data shuffle_s;
          Data owned_s;
          Event (`Shuffle, Some (Hash.hash_string owned_s));
        ]
    in
    match x with
    | true -> return_some shuffle_h
    | false -> Lwt.fail @@ Failure "race condition in append_to_shuffles"
  else return_none

let make_result_transaction yojson_of_result result =
  let payload = !+(yojson_of_election_result yojson_of_result) result in
  [ Data payload; Event (`Result, Some (Hash.hash_string payload)) ]

let internal_release_tally ~force s set_state =
  let@ election =
    let uuid = Storage.E.get_uuid s in
    Public_archive.with_election s ~fallback:(fun () ->
        Lwt.fail (Election_not_found (uuid, "internal_release_tally")))
  in
  let module W = (val election) in
  let@ last cont =
    let* x = Storage.E.get s Last_event in
    match Lopt.get_value x with None -> assert false | Some x -> cont x
  in
  let* metadata = get_election_metadata s in
  let trustees_with_ids =
    Option.value metadata.trustees ~default:[ None ]
    |> List.mapi (fun i x -> (i + 1, x))
  in
  let* pds = Public_archive.get_partial_decryptions s W.G.witness in
  let@ () =
   fun cont ->
    if force then cont ()
    else if
      (* check whether all trustees have done their job *)
      List.for_all
        (fun (i, x) -> x = None || List.exists (fun x -> x.owner = i) pds)
        trustees_with_ids
    then cont ()
    else Lwt.return_false
  in
  let* tally =
    let* x = Public_archive.get_latest_encrypted_tally s in
    match x with
    | None -> assert false
    | Some x -> Lwt.return @@ !*(encrypted_tally_of_yojson !$W.G.of_string) x
  in
  let* sized =
    let* x = Public_archive.get_sized_encrypted_tally s in
    match x with
    | None -> assert false
    | Some x ->
        let x = !*(sized_encrypted_tally_of_yojson hash_of_yojson) x in
        Lwt.return { x with encrypted_tally = tally }
  in
  let* trustees =
    let* x = Public_archive.get_trustees s W.G.witness in
    match Lopt.get_value x with None -> assert false | Some x -> Lwt.return x
  in
  let* pds, transactions =
    let pds = List.rev pds in
    let decrypt owner =
      let* x = Storage.E.get s Server_seed in
      match Lopt.get_value x with
      | Some seed ->
          let module P = Pki.Make (W.G) in
          let module T = (val Trustees.get_by_version W.version) in
          let module KG = T.MakeBasic (W.G) in
          let sk = P.derive_sk seed in
          let pdk = KG.derive seed in
          let pd = W.E.compute_factor tally ~sk ~pdk in
          let owned = { owner; payload = pd } in
          let pd =
            !+[%yojson_of_witness (W.G.witness : _ partial_decryption)] pd
          in
          let payload =
            { owner; payload = Hash.hash_string pd }
            |> !+(yojson_of_owned yojson_of_hash)
          in
          let transaction =
            [
              Data pd;
              Data payload;
              Event (`PartialDecryption, Some (Hash.hash_string payload));
            ]
          in
          Lwt.return (owned, transaction)
      | _ -> assert false
    in
    Lwt_list.fold_left_s
      (fun ((pds, transactions) as accu) (i, t) ->
        if t = None then
          if List.exists (fun x -> x.owner = i) pds then Lwt.return accu
          else
            let* pd, transaction = decrypt i in
            Lwt.return (pd :: pds, transaction :: transactions)
        else Lwt.return accu)
      (pds, []) trustees_with_ids
  in
  match W.E.compute_result sized pds trustees with
  | Ok result ->
      let result_transaction =
        make_result_transaction W.yojson_of_result result
      in
      let@ () =
       fun cont ->
        let* x =
          List.rev (result_transaction :: transactions)
          |> List.flatten |> Storage.E.append s ~last
        in
        match x with
        | true -> cont ()
        | false ->
            Lwt.fail @@ Failure "race condition in internal_release_tally"
      in
      let* () = Storage.E.del s Audit_cache in
      let* () = set_state `Tallied in
      let@ dates, set_dates = update_election_dates s in
      let* () = set_dates { dates with tally = Some (Unix.gettimeofday ()) } in
      let* () = Storage.E.set s State_state Value None in
      Lwt.return_true
  | Error e -> Lwt.fail @@ Failure (Trustees.string_of_combination_error e)

let raw_get_election_state ?(update = true) ?(ignore_errors = true) s return =
  let@ () =
   fun cont ->
    if Storage.readonly.get () then
      let* x = Storage.E.get s State in
      match Lopt.get_value x with
      | Some x -> return (x, fun _ -> raise Readonly_storage)
      | None -> (
          let* x = Storage.E.get s Draft in
          match Lopt.get_value x with
          | Some _ -> return (`Draft, fun _ -> raise Readonly_storage)
          | None -> return (`Archived, fun _ -> raise Readonly_storage))
    else cont ()
  in
  let@ state, set_state =
   fun cont ->
    let@ x, set = Storage.E.update s State in
    match Lopt.get_value x with
    | Some x -> cont (x, set Value)
    | None -> (
        let* x = Storage.E.get s Draft in
        match Lopt.get_value x with
        | Some _ ->
            return
              (`Draft, fun _ -> Lwt.fail_with "cannot get out of Draft this way")
        | None ->
            return
              (`Archived, fun _ -> Lwt.fail_with "cannot get out of Archived"))
  in
  let now = Unix.gettimeofday () in
  let* dates = get_election_dates s in
  let past = function None -> false | Some t -> t < now in
  let@ () =
   fun cont ->
    match state with
    | `EncryptedTally when update -> (
        match dates.publish with
        | Some _ when not (past dates.publish) -> cont ()
        | _ ->
            let@ () =
             fun cont2 ->
              if ignore_errors then Lwt.catch cont2 (fun _ -> cont ())
              else cont2 ()
            in
            let* b = internal_release_tally ~force:false s set_state in
            return ((if b then `Tallied else state), set_state))
    | _ -> cont ()
  in
  let new_state =
    match state with `Closed when past dates.auto_open -> `Open | x -> x
  in
  let new_state =
    match new_state with `Open when past dates.auto_close -> `Closed | x -> x
  in
  assert (new_state <> `Archived);
  let* () =
    if update && new_state <> state then set_state new_state else return_unit
  in
  return (new_state, set_state)

let update_election_state s cont = raw_get_election_state s cont

let get_election_state s =
  let@ x, _ = update_election_state s in
  Lwt.return x

let release_tally s =
  let@ state, set_state = update_election_state s in
  match state with
  | `EncryptedTally ->
      let* b = internal_release_tally ~force:true s set_state in
      assert b;
      set_state `Tallied
  | _ -> Lwt.fail @@ Failure "election not in EncryptedTally state"

let add_partial_decryption s (owner, pd) =
  let payload =
    { owner; payload = Hash.hash_string pd }
    |> !+(yojson_of_owned yojson_of_hash)
  in
  let* x =
    Storage.E.append s
      [
        Data pd;
        Data payload;
        Event (`PartialDecryption, Some (Hash.hash_string payload));
      ]
  in
  match x with
  | true -> Lwt.return_unit
  | false -> Lwt.fail @@ Failure "race condition in add_partial_decryption"

let get_all_voters s =
  let* x = Storage.E.get s Voters in
  match Lopt.get_value x with None -> Lwt.return [] | Some x -> Lwt.return x

let dummy_voters_config =
  {
    has_explicit_weights = false;
    username_or_address = `Username;
    nb_voters = 0;
  }

let get_voters_config s =
  let* x = Storage.E.get s Voters_config in
  match Lopt.get_value x with
  | None -> Lwt.return dummy_voters_config
  | Some x -> Lwt.return x

let get_has_explicit_weights s =
  let* { has_explicit_weights; _ } = get_voters_config s in
  Lwt.return has_explicit_weights

let get_username_or_address s =
  let* { username_or_address; _ } = get_voters_config s in
  Lwt.return username_or_address

let get_voter s id =
  let* x = Storage.E.get s (Voter id) in
  let&* x = Lopt.get_value x in
  Lwt.return_some x

let get_credential_user s cred =
  let* x = Storage.E.get s (Credential_user cred) in
  match Lopt.get_value x with
  | Some x -> Lwt.return_some x
  | None ->
      let uuid = Storage.E.get_uuid s in
      Lwt.fail
        (Failure
           (Printf.sprintf "could not find credential record of %s/%s"
              (Uuid.to_string uuid) cred))

let add_ballot s (election : Election.t) last ballot =
  let module W = (val election) in
  let hash = Hash.hash_string ballot in
  let* x =
    Storage.E.append s ~last [ Data ballot; Event (`Ballot, Some hash) ]
  in
  match x with
  | true ->
      let () = Public_archive.clear_ballot_cache (Storage.E.get_uuid s) in
      return hash
  | false -> Lwt.fail @@ Failure "race condition in add_ballot"

let get_credential_weight s credential =
  let* x = Storage.E.get s (Credential_weight credential) in
  match Lopt.get_value x with
  | None -> Lwt.return Weight.one
  | Some x -> Lwt.return x

let raw_compute_encrypted_tally s (election : Election.t) =
  let module W = (val election) in
  let module GMap = Map.Make (W.G) in
  let@ last cont =
    let* x = Storage.E.get s Last_event in
    match Lopt.get_value x with None -> assert false | Some x -> cont x
  in
  let* ballots =
    Public_archive.fold_on_ballots s
      (fun _ b accu ->
        let ballot = !*W.ballot_of_yojson b in
        match W.get_credential ballot with
        | None -> assert false
        | Some credential ->
            if GMap.mem credential accu then Lwt.return accu
            else Lwt.return @@ GMap.add credential ballot accu)
      GMap.empty
  in
  let* ballots =
    Lwt_list.fold_left_s
      (fun accu (credential, ballot) ->
        let* weight = get_credential_weight s (W.G.to_string credential) in
        Lwt.return @@ ((weight, ballot) :: accu))
      [] (GMap.bindings ballots)
  in
  let tally = W.E.process_ballots ballots in
  let tally_s = !+(yojson_of_encrypted_tally !&W.G.to_string) tally in
  let payload =
    {
      num_tallied = List.length ballots;
      total_weight =
        List.fold_left
          (fun accu (w, _) -> Weight.(accu + w))
          Weight.zero ballots;
      encrypted_tally = Hash.hash_string tally_s;
    }
    |> !+(yojson_of_sized_encrypted_tally yojson_of_hash)
  in
  let* x =
    Storage.E.append s ~last
      [
        Event (`EndBallots, None);
        Data tally_s;
        Data payload;
        Event (`EncryptedTally, Some (Hash.hash_string payload));
      ]
  in
  match x with
  | true -> Storage.E.del s Audit_cache
  | false -> Lwt.fail @@ Failure "race condition in raw_compute_encrypted_tally"

let get_credential_record s credential =
  let* credential_mapping = Storage.E.get s (Credential_mapping credential) in
  let&* { ballot = cr_ballot; _ } = Lopt.get_value credential_mapping in
  let* cr_username = get_credential_user s credential in
  let* cr_weight = get_credential_weight s credential in
  return_some { cr_ballot; cr_weight; cr_username }

type precast_data = {
  credential : string;
  credential_record : credential_record;
}

let precast_ballot s ~ballot =
  let@ election =
    let uuid = Storage.E.get_uuid s in
    Public_archive.with_election s ~fallback:(fun () ->
        Lwt.fail (Election_not_found (uuid, "precast_ballot")))
  in
  let module W = (val election) in
  let@ () =
   fun cont ->
    let hash = Hash.hash_string ballot in
    let* x = Public_archive.get_data s hash in
    match x with
    | None -> cont ()
    | Some _ -> Lwt.return @@ Error `DuplicateBallot
  in
  let@ rc cont =
    match W.E.check_rawballot ballot with
    | Error _ as x -> Lwt.return x
    | Ok rc -> cont rc
  in
  let credential = rc.rc_credential in
  let@ credential_record cont =
    let* x = get_credential_record s credential in
    match x with
    | None -> Lwt.return @@ Error `InvalidCredential
    | Some cr -> cont cr
  in
  if rc.rc_check () then Lwt.return @@ Ok { credential; credential_record }
  else Lwt.return @@ Error `InvalidBallot

let do_cast_ballot s (election : Election.t) ~ballot ~user ~weight date
    ~precast_data =
  let module W = (val election) in
  let@ last cont =
    let* x = Storage.E.get s Last_event in
    match Lopt.get_value x with None -> assert false | Some x -> cont x
  in
  let get_username user =
    match String.index_opt user ':' with
    | None -> user
    | Some i -> String.sub user (i + 1) (String.length user - i - 1)
  in
  let get_user_record user =
    let* x = Storage.E.get s (Extended_record user) in
    let&* { credential; _ } = Lopt.get_value x in
    return_some credential
  in
  let@ x cont =
    let { credential; credential_record = cr } = precast_data in
    let@ () =
     fun cont2 ->
      if Weight.compare cr.cr_weight weight <> 0 then cont @@ Error `WrongWeight
      else cont2 ()
    in
    let@ () =
     fun cont2 ->
      match cr.cr_username with
      | Some username when get_username user <> username ->
          cont @@ Error `WrongUsername
      | Some _ -> cont2 ()
      | None -> (
          let* x = get_user_record user in
          match (x, cr.cr_ballot) with
          | None, None -> cont2 ()
          | None, Some _ -> cont @@ Error `UsedCredential
          | Some _, None -> cont @@ Error `RevoteNotAllowed
          | Some credential', _ when credential' = credential -> cont2 ()
          | Some _, _ -> cont @@ Error `WrongCredential)
    in
    let* x = get_credential_record s credential in
    match x with
    | None -> assert false
    | Some cr' when cr'.cr_ballot = cr.cr_ballot ->
        cont @@ Ok (credential, cr.cr_ballot)
    | Some _ -> cont @@ Error `ExpiredBallot
  in
  match x with
  | Error _ as x -> return x
  | Ok (credential, old) ->
      let@ hash, revote =
       fun cont ->
        match old with
        | None ->
            let* h = add_ballot s election last ballot in
            cont (h, false)
        | Some _ ->
            if !Web_config.deny_revote then return @@ Error `RevoteNotAllowed
            else
              let* h = add_ballot s election last ballot in
              cont (h, true)
      in
      let* () =
        hash |> Hash.to_b64
        |> (fun ballot -> { ballot = Some ballot; credential })
        |> Storage.E.set s (Credential_mapping credential) Value
      in
      let* () =
        { username = user; date; credential }
        |> Storage.E.set s (Extended_record user) Value
      in
      return (Ok (hash, revote))

let cast_ballot s ~ballot ~user ~weight date ~precast_data =
  let@ election =
    let uuid = Storage.E.get_uuid s in
    Public_archive.with_election s ~fallback:(fun () ->
        Lwt.fail (Election_not_found (uuid, "cast_ballot")))
  in
  do_cast_ballot s election ~ballot ~user ~weight date ~precast_data

let compute_audit_cache s =
  let* election = Public_archive.get_election s in
  match Lopt.get_value election with
  | None -> Lwt.return_none
  | Some election ->
      let module W = (val election) in
      let* voters = get_all_voters s in
      let voters_hash = Hash.hash_string (Voter.list_to_string voters) in
      let* shuffles =
        let* x = Public_archive.get_shuffles s in
        let&* x = x in
        Lwt.return_some (List.map (fun (_, x, _) -> x) x)
      in
      let* encrypted_tally =
        let* x = Public_archive.get_sized_encrypted_tally s in
        let&* x = x in
        let x = !*(sized_encrypted_tally_of_yojson hash_of_yojson) x in
        Lwt.return_some x.encrypted_tally
      in
      let@ trustees cont =
        let* x = Public_archive.get_trustees s W.G.witness in
        match Lopt.get_value x with None -> Lwt.return_none | Some x -> cont x
      in
      let* checksums =
        let* public_credentials =
          Public_archive.get_public_creds s W.G.witness
        in
        let* final =
          let* roots = Public_archive.get_roots s in
          let&* _ = roots.result in
          let* last_event = Storage.E.get s Last_event in
          Lwt.return
          @@ Option.map
               (fun (x : last_event) -> x.hash)
               (Lopt.get_value last_event)
        in
        Election.compute_checksums
          (module W)
          trustees public_credentials ~shuffles ~encrypted_tally ~final
        |> Lwt.return
      in
      let* sealing_log =
        let* x = Storage.E.get s Sealing_log in
        match Lopt.get_value x with
        | None -> Lwt.return_none
        | Some x -> Lwt.return_some @@ Hash.hash_string x
      in
      return_some { voters_hash; checksums; threshold = None; sealing_log }

let get_audit_cache s =
  let* cache = Storage.E.get s Audit_cache in
  match Lopt.get_value cache with
  | Some x -> return_some x
  | None -> (
      let* cache = compute_audit_cache s in
      match cache with
      | None -> return_none
      | Some cache ->
          let* () = Storage.E.set s Audit_cache Value cache in
          return_some cache)

let send_credentials s ~admin_id (Draft (_, se)) private_creds =
  let@ private_creds cont =
    match Lopt.get_value private_creds with
    | None -> Lwt.return_unit
    | Some x -> cont x
  in
  let@ () =
   fun cont -> if se.pending_credentials then cont () else Lwt.return_unit
  in
  let voter_map =
    List.fold_left
      (fun accu (v : draft_voter) ->
        let login = Voter.get v.id in
        let weight = Voter.get_weight v.id in
        let recipient =
          match v.id with
          | _, { address; _ } -> Option.value ~default:login address
        in
        SMap.add login (recipient, weight) accu)
      SMap.empty se.voters
  in
  let* metadata = Mails_voter.get_metadata s ~admin_id in
  let send = Mails_voter.generate_credential_email metadata in
  let* jobs =
    Lwt_list.fold_left_s
      (fun jobs (login, credential) ->
        match SMap.find_opt login voter_map with
        | None -> Lwt.return jobs
        | Some (recipient, weight) ->
            let* job = send ~recipient ~login ~weight credential in
            Lwt.return (job :: jobs))
      [] private_creds
  in
  let* () = Mails_voter_bulk.submit_bulk_emails jobs in
  se.pending_credentials <- false;
  Lwt.return_unit

let validate_on_credential_server ~uuid ~(info : cred_authority_info) ~token
    ~metadata () =
  let prefix =
    Printf.sprintf "validate_on_credential_server[%s]" (Uuid.to_string uuid)
  in
  let body =
    `Validate { uuid; token; metadata }
    |> !+Belenios_web_api.yojson_of_credentials_request
    |> Cohttp_lwt.Body.of_string
  in
  Lwt.catch
    (fun () ->
      let* x, body =
        Cohttp_lwt_unix.Client.post ~body (Uri.of_string info.server)
      in
      let* () = Cohttp_lwt.Body.drain_body body in
      let code = Cohttp.Code.code_of_status x.status in
      let msg = Printf.sprintf "%s: %d" prefix code in
      Ocsigen_messages.warning msg;
      Lwt.return_unit)
    (fun e ->
      let msg = Printf.sprintf "%s: %s" prefix (Printexc.to_string e) in
      Ocsigen_messages.errlog msg;
      Lwt.return_unit)

let validate_election ~admin_id storage
    ((W (w, Draft (v, se)), set) :
      wrapped_draft_election updatable_with_billing) s =
  let uuid = Storage.E.get_uuid storage in
  let open Belenios_web_api in
  let questions =
    let x = se.questions in
    let administrator =
      match x.administrator with None -> se.administrator | x -> x
    in
    let credential_authority =
      match x.credential_authority with
      | None -> se.metadata.cred_authority
      | x -> x
    in
    { x with administrator; credential_authority }
  in
  (* convenience tests *)
  let validation_error x = raise (Api_generic.Error (`ValidationError x)) in
  let () =
    if questions.name = "" then validation_error `NoTitle;
    if questions.questions = [||] then validation_error `NoQuestions;
    (match questions.administrator with
    | None | Some "" -> validation_error `NoAdministrator
    | _ -> ());
    match questions.credential_authority with
    | None | Some "" -> validation_error `NoCredentialAuthority
    | _ -> ()
  in
  let@ _check_cas_server (cont : unit -> _) =
    match se.metadata.auth_config with
    | Some [ { auth_system = "cas"; auth_config; _ } ] -> (
        match List.assoc_opt "server" auth_config with
        | None -> validation_error `BadAuthentication
        | Some url ->
            let url =
              if String.ends_with ~suffix:"/" url then url else url ^ "/"
            in
            let try_validate suffix fail =
              Lwt.catch
                (fun () ->
                  let* x, body =
                    Cohttp_lwt_unix.Client.get (Uri.of_string (url ^ suffix))
                  in
                  let* () = Cohttp_lwt.Body.drain_body body in
                  match Cohttp.Code.code_of_status x.status with
                  | 200 -> cont ()
                  | _ -> fail ())
                (fun _ -> fail ())
            in
            let@ () = try_validate "serviceValidate" in
            let@ () = try_validate "validate" in
            validation_error `BadAuthentication)
    | _ -> cont ()
  in
  (* check status *)
  let () =
    if s.num_voters = 0 then validation_error `NoVoters;
    if not s.credentials_ready then validation_error `MissingPublicCredentials;
    if not s.trustees_ready then validation_error `TrusteesNotReady;
    if not s.nh_and_weights_compatible then
      validation_error `WeightsAreIncompatibleWithNH;
    match s.restricted_mode_error with
    | None -> ()
    | Some e -> validation_error (`RestrictedMode e)
  in
  (* billing *)
  let* () =
    match (!Web_config.billing, se.metadata.billing_request) with
    | None, _ -> Lwt.return_unit
    | Some (url, callback), None ->
        let* id = Billing.create ~admin_id ~uuid ~nb_voters:s.num_voters in
        let metadata = { se.metadata with billing_request = Some id } in
        let se = { se with metadata } in
        let* () =
          set ~billing:true (W (w, Draft (v, se)) : wrapped_draft_election)
        in
        validation_error (`MissingBilling { url; id; callback })
    | Some (url, callback), Some id ->
        let* b = Billing.check ~url ~id in
        if b then Lwt.return_unit
        else validation_error (`MissingBilling { url; id; callback })
  in
  (* send private credentials *)
  let* () =
    match se.metadata.cred_authority_info with
    | None ->
        let* private_creds = Storage.E.get storage Private_creds in
        send_credentials storage ~admin_id (Draft (v, se)) private_creds
    | Some info ->
        let* metadata = Mails_voter.get_metadata storage ~admin_id in
        Lwt.async
          (validate_on_credential_server ~uuid ~info ~token:se.public_creds
             ~metadata);
        Lwt.return_unit
  in
  (* the validation itself *)
  let* x = Storage.E.validate_election storage in
  match x with Ok () -> Lwt.return_unit | Error e -> validation_error e

let create_draft s se = Storage.E.set s Draft Value se
let transition_to_encrypted_tally set_state = set_state `EncryptedTally

let compute_encrypted_tally s =
  let@ state, set_state = update_election_state s in
  match state with
  | `Closed ->
      let@ election =
        Public_archive.with_election s ~fallback:(fun () ->
            let uuid = Storage.E.get_uuid s in
            Lwt.fail (Election_not_found (uuid, "compute_encrypted_tally")))
      in
      let module W = (val election) in
      let@ sk cont =
        let* x = Storage.E.get s Server_seed in
        match Lopt.get_value x with
        | None -> Lwt.return_false
        | Some seed ->
            let module P = Pki.Make (W.G) in
            cont @@ P.derive_sk seed
      in
      let vk = W.G.(g **~ sk) in
      let* () = raw_compute_encrypted_tally s election in
      if W.has_nh_questions then
        let* () = set_state `Shuffling in
        (* perform server-side shuffle *)
        let* cc = Public_archive.get_nh_ciphertexts s in
        let cc = !*(nh_ciphertexts_of_yojson !$W.G.of_string) cc in
        let shuffle = W.E.shuffle_ciphertexts ~sk cc in
        let* x = append_to_shuffles s (module W) 1 ~vk shuffle in
        match x with
        | None -> Lwt.fail (Failure "server-side shuffle failed")
        | Some _ -> Lwt.return_true
      else
        let* () = transition_to_encrypted_tally set_state in
        Lwt.return_true
  | _ -> Lwt.return_false

let finish_shuffling s =
  let@ state, set_state = update_election_state s in
  match state with
  | `Shuffling ->
      let@ () =
       fun cont ->
        let* x = Storage.E.append s [ Event (`EndShuffles, None) ] in
        match x with
        | true -> cont ()
        | false -> Lwt.fail @@ Failure "race condition in finish_shuffling"
      in
      let* () = Storage.E.set s State_state Value None in
      let* () = transition_to_encrypted_tally set_state in
      Lwt.return_true
  | _ -> Lwt.return_false

let set_election_state s state =
  let@ state', set_state = update_election_state s in
  let* allowed =
    match state' with
    | `Open | `Closed -> Lwt.return_some set_state
    | _ -> Lwt.return_none
  in
  match allowed with
  | Some set_state ->
      let* () = set_state (state : [ `Open | `Closed ] :> election_state) in
      let@ dates, set_dates = update_election_dates s in
      let* () = set_dates { dates with auto_open = None; auto_close = None } in
      Lwt.return_true
  | None -> Lwt.return_false

let open_election s = set_election_state s `Open
let close_election s = set_election_state s `Closed

let get_election_automatic_dates s =
  let open Belenios_web_api in
  let* d = get_election_dates s in
  Lwt.return
    {
      open_ = d.auto_open;
      close = d.auto_close;
      publish = d.publish;
      grace_period = d.grace_period;
    }

let set_election_automatic_dates s d =
  let open Belenios_web_api in
  let auto_open = d.open_ in
  let auto_close = d.close in
  let publish = d.publish in
  let grace_period = d.grace_period in
  let@ dates, set = update_election_dates s in
  set { dates with auto_open; auto_close; publish; grace_period }

let get_draft_public_credentials s (type a b) (w : (a, b) group_witness) =
  let* x = Storage.E.get s (Public_creds w) in
  let&* x = Lopt.get_value x in
  x
  |> List.map (fun (x : _ public_credential_with_id) -> x.credential)
  |> Lwt.return_some

let get_records s =
  let* x = Storage.E.get s Records in
  let&* x = Lopt.get_value x in
  Lwt.return_some x

type credentials_status = [ `None | `Pending of int | `Done ]

let pending_generations = ref SMap.empty

let generate_credentials_on_server_async uuid (Draft (_, se)) =
  let uuid_s = Uuid.to_string uuid in
  match SMap.find_opt uuid_s !pending_generations with
  | Some _ -> ()
  | None ->
      let voters = List.map (fun (v : draft_voter) -> v.id) se.voters in
      let module G = (val Belenios.Group.of_string ~version:se.version se.group)
      in
      let module Cred =
        Credential.Make
          (G)
          (struct
            type 'a t = 'a Lwt.t

            let return = Lwt.return
            let bind = Lwt.bind
            let pause = Lwt.pause
            let uuid = uuid
          end) in
      let t, p = Cred.generate_sub (List.length voters) in
      pending_generations := SMap.add uuid_s p !pending_generations;
      Lwt.async (fun () ->
          let* x = t in
          let Credential.{ private_creds; public_with_ids; _ } =
            Cred.merge_sub voters x
          in
          let@ s = Storage.E.with_transaction uuid in
          let@ se, set = Storage.E.update s Draft in
          match Lopt.get_value se with
          | Some (W (w, Draft (v, se))) ->
              let* () = private_creds |> Storage.E.set s Private_creds Value in
              let* () =
                Storage.E.set s (Public_creds G.witness) Value public_with_ids
              in
              se.public_creds_received <- true;
              se.pending_credentials <- true;
              let* () = set Value (W (w, Draft (v, se))) in
              pending_generations := SMap.remove uuid_s !pending_generations;
              Lwt.return_unit
          | None -> Lwt.return_unit)

let get_credentials_status uuid (Draft (_, se)) =
  match SMap.find_opt (Uuid.to_string uuid) !pending_generations with
  | Some p -> `Pending (p ())
  | None -> if se.public_creds_received then `Done else `None
