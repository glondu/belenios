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
open Belenios_storage_api
open Belenios_server_core
open Web_common

let get_setup_data s uuid =
  let* x =
    let* x = Public_archive.get_roots s uuid in
    let&* x = x.roots_setup_data in
    Public_archive.get_data s uuid x
  in
  match x with
  | None -> assert false
  | Some x -> Lwt.return (setup_data_of_string x)

let get_election_dates s uuid =
  let* x = Storage.get s (Election (uuid, Dates)) in
  Lwt.return
    (Option.value ~default:Belenios_storage_api.default_election_dates
       (Lopt.get_value x))

let update_election_dates s uuid cont =
  let@ x, set = Storage.update s (Election (uuid, Dates)) in
  cont
    ( Option.value ~default:Belenios_storage_api.default_election_dates
        (Lopt.get_value x),
      set Value )

let empty_metadata =
  {
    e_owners = [];
    e_auth_config = None;
    e_cred_authority = None;
    e_trustees = None;
    e_languages = None;
    e_contact = None;
    e_booth_version = None;
    e_billing_request = None;
    e_sealed = None;
  }

let get_election_metadata s uuid =
  let* x = Storage.get s (Election (uuid, Metadata)) in
  match Lopt.get_value x with
  | Some x -> Lwt.return x
  | None -> (
      let* x = Storage.get s (Election (uuid, Draft)) in
      match Lopt.get_value x with
      | Some (Draft (_, x)) -> Lwt.return x.se_metadata
      | None -> Lwt.return empty_metadata)

let seal_election s uuid seal =
  let@ x, set = Storage.update s (Election (uuid, Metadata)) in
  match Lopt.get_value x with
  | None -> raise Not_found
  | Some metadata -> (
      let* x = Storage.get s (Election (uuid, Dates)) in
      match Lopt.get_value x with
      | None -> raise Not_found
      | Some dates ->
          let op, e_sealed =
            if seal then
              ( `Seal
                  {
                    date_open = dates.e_date_auto_open;
                    date_close = dates.e_date_auto_close;
                    date_publish = dates.e_date_publish;
                  },
                Some true )
            else (`Unseal, None)
          in
          let date = Unix.gettimeofday () in
          let* b = Storage.append_sealing s uuid { date; op } in
          let* () = Storage.del s (Election (uuid, Audit_cache)) in
          if b then set Value { metadata with e_sealed }
          else failwith "sealing error")

let append_to_shuffles s election owned_owner shuffle_s =
  let module W = (val election : Site_common_sig.ELECTION) in
  let uuid = W.uuid in
  let@ last cont =
    let* x = Storage.get s (Election (uuid, Last_event)) in
    match Lopt.get_value x with None -> assert false | Some x -> cont x
  in
  let shuffle =
    shuffle_of_string W.(sread G.of_string) W.(sread G.Zq.of_string) shuffle_s
  in
  let shuffle_h = Hash.hash_string shuffle_s in
  let* last_nh = Public_archive.get_nh_ciphertexts s uuid in
  let last_nh = nh_ciphertexts_of_string W.(sread G.of_string) last_nh in
  if
    string_of_shuffle W.(swrite G.to_string) W.(swrite G.Zq.to_string) shuffle
    = shuffle_s
    && W.E.check_shuffle last_nh shuffle
  then
    let owned = { owned_owner; owned_payload = shuffle_h } in
    let owned_s = string_of_owned write_hash owned in
    let* x =
      Storage.append s uuid ~last
        [
          Data shuffle_s;
          Data owned_s;
          Event (`Shuffle, Some (Hash.hash_string owned_s));
        ]
    in
    match x with
    | true -> return_some @@ sha256_b64 shuffle_s
    | false -> Lwt.fail @@ Failure "race condition in append_to_shuffles"
  else return_none

let make_result_transaction write_result result =
  let payload = string_of_election_result write_result result in
  [ Data payload; Event (`Result, Some (Hash.hash_string payload)) ]

let internal_release_tally ~force s uuid set_state =
  let@ last cont =
    let* x = Storage.get s (Election (uuid, Last_event)) in
    match Lopt.get_value x with None -> assert false | Some x -> cont x
  in
  let* metadata = get_election_metadata s uuid in
  let trustees_with_ids =
    Option.value metadata.e_trustees ~default:[ "server" ]
    |> List.mapi (fun i x -> (i + 1, x))
  in
  let* pds = Public_archive.get_partial_decryptions s uuid in
  let@ () =
   fun cont ->
    if force then cont ()
    else if
      (* check whether all trustees have done their job *)
      List.for_all
        (fun (i, x) ->
          x = "server" || List.exists (fun x -> x.owned_owner = i) pds)
        trustees_with_ids
    then cont ()
    else Lwt.return_false
  in
  let@ election =
    Public_archive.with_election s uuid ~fallback:(fun () ->
        Lwt.fail (Election_not_found (uuid, "internal_release_tally")))
  in
  let module W = (val election) in
  let* tally =
    let* x = Public_archive.get_latest_encrypted_tally s uuid in
    match x with
    | None -> assert false
    | Some x -> Lwt.return @@ encrypted_tally_of_string W.(sread G.of_string) x
  in
  let* sized =
    let* x = Public_archive.get_sized_encrypted_tally s uuid in
    match x with
    | None -> assert false
    | Some x ->
        let x = sized_encrypted_tally_of_string read_hash x in
        Lwt.return { x with sized_encrypted_tally = tally }
  in
  let* trustees =
    let* x = Public_archive.get_trustees s uuid in
    Lwt.return
    @@ trustees_of_string W.(sread G.of_string) W.(sread G.Zq.of_string) x
  in
  let* pds, transactions =
    let pds =
      List.rev_map
        (fun x ->
          let owned_payload =
            partial_decryption_of_string
              W.(sread G.of_string)
              W.(sread G.Zq.of_string)
              x.owned_payload
          in
          { x with owned_payload })
        pds
    in
    let decrypt owned_owner =
      let* x = Storage.get s (Election (uuid, Private_key)) in
      match Lopt.get_value x with
      | Some (`String sk) ->
          let sk = W.G.Zq.of_string sk in
          let pd = W.E.compute_factor tally sk in
          let owned = { owned_owner; owned_payload = pd } in
          let pd =
            string_of_partial_decryption
              W.(swrite G.to_string)
              W.(swrite G.Zq.to_string)
              pd
          in
          let payload =
            { owned_owner; owned_payload = Hash.hash_string pd }
            |> string_of_owned write_hash
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
        if t = "server" then
          if List.exists (fun x -> x.owned_owner = i) pds then Lwt.return accu
          else
            let* pd, transaction = decrypt i in
            Lwt.return (pd :: pds, transaction :: transactions)
        else Lwt.return accu)
      (pds, []) trustees_with_ids
  in
  match W.E.compute_result sized pds trustees with
  | Ok result ->
      let result_transaction = make_result_transaction W.write_result result in
      let@ () =
       fun cont ->
        let* x =
          List.rev (result_transaction :: transactions)
          |> List.flatten
          |> Storage.append s uuid ~last
        in
        match x with
        | true -> cont ()
        | false ->
            Lwt.fail @@ Failure "race condition in internal_release_tally"
      in
      let* () = Storage.del s (Election (uuid, Audit_cache)) in
      let* () = set_state `Tallied in
      let@ dates, set_dates = update_election_dates s uuid in
      let* () =
        set_dates { dates with e_date_tally = Some (Unix.gettimeofday ()) }
      in
      let* () = Storage.set s (Election (uuid, State_state)) Value None in
      Lwt.return_true
  | Error e -> Lwt.fail @@ Failure (Trustees.string_of_combination_error e)

let raw_get_election_state ?(update = true) ?(ignore_errors = true) s uuid
    return =
  let@ state, set_state =
   fun cont ->
    let@ x, set = Storage.update s (Election (uuid, State)) in
    match Lopt.get_value x with
    | Some x -> cont (x, set Value)
    | None -> (
        let* x = Storage.get s (Election (uuid, Draft)) in
        match Lopt.get_value x with
        | Some _ ->
            return
              (`Draft, fun _ -> Lwt.fail_with "cannot get out of Draft this way")
        | None ->
            return
              (`Archived, fun _ -> Lwt.fail_with "cannot get out of Archived"))
  in
  let now = Unix.gettimeofday () in
  let* dates = get_election_dates s uuid in
  let past = function None -> false | Some t -> t < now in
  let@ () =
   fun cont ->
    match state with
    | `EncryptedTally when update -> (
        match dates.e_date_publish with
        | Some _ when not (past dates.e_date_publish) -> cont ()
        | _ ->
            let@ () =
             fun cont2 ->
              if ignore_errors then Lwt.catch cont2 (fun _ -> cont ())
              else cont2 ()
            in
            let* b = internal_release_tally ~force:false s uuid set_state in
            return ((if b then `Tallied else state), set_state))
    | _ -> cont ()
  in
  let new_state =
    match state with
    | `Closed when past dates.e_date_auto_open -> `Open
    | x -> x
  in
  let new_state =
    match new_state with
    | `Open when past dates.e_date_auto_close -> `Closed
    | x -> x
  in
  assert (new_state <> `Archived);
  let* () =
    if update && new_state <> state then set_state new_state else return_unit
  in
  return (new_state, set_state)

let update_election_state s uuid cont = raw_get_election_state s uuid cont

let get_election_state s uuid =
  let@ x, _ = update_election_state s uuid in
  Lwt.return x

let release_tally s uuid =
  let@ state, set_state = update_election_state s uuid in
  match state with
  | `EncryptedTally ->
      let* b = internal_release_tally ~force:true s uuid set_state in
      assert b;
      set_state `Tallied
  | _ -> Lwt.fail @@ Failure "election not in EncryptedTally state"

let add_partial_decryption s uuid (owned_owner, pd) =
  let payload =
    { owned_owner; owned_payload = Hash.hash_string pd }
    |> string_of_owned write_hash
  in
  let* x =
    Storage.append s uuid
      [
        Data pd;
        Data payload;
        Event (`PartialDecryption, Some (Hash.hash_string payload));
      ]
  in
  match x with
  | true -> Lwt.return_unit
  | false -> Lwt.fail @@ Failure "race condition in add_partial_decryption"

let check_password s uuid ~user ~password =
  let* r = Storage.get s (Election (uuid, Password user)) in
  let&* ({ username; address; _ } as r) = Lopt.get_value r in
  if check_password r password then Lwt.return_some (username, address)
  else Lwt.return_none

let get_all_voters s uuid =
  let* x = Storage.get s (Election (uuid, Voters)) in
  match Lopt.get_value x with None -> Lwt.return [] | Some x -> Lwt.return x

let dummy_voters_config =
  {
    has_explicit_weights = false;
    username_or_address = `Username;
    nb_voters = 0;
  }

let get_voters_config s uuid =
  let* x = Storage.get s (Election (uuid, Voters_config)) in
  match Lopt.get_value x with
  | None -> Lwt.return dummy_voters_config
  | Some x -> Lwt.return x

let get_has_explicit_weights s uuid =
  let* { has_explicit_weights; _ } = get_voters_config s uuid in
  Lwt.return has_explicit_weights

let get_username_or_address s uuid =
  let* { username_or_address; _ } = get_voters_config s uuid in
  Lwt.return username_or_address

let get_voter s uuid id =
  let* x = Storage.get s (Election (uuid, Voter id)) in
  let&* x = Lopt.get_value x in
  Lwt.return_some x

let get_credential_user s uuid cred =
  let* x = Storage.get s (Election (uuid, Credential_user cred)) in
  match Lopt.get_value x with
  | Some x -> Lwt.return_some x
  | None ->
      Lwt.fail
        (Failure
           (Printf.sprintf "could not find credential record of %s/%s"
              (Uuid.unwrap uuid) cred))

let add_ballot s election last ballot =
  let module W = (val election : Site_common_sig.ELECTION) in
  let uuid = W.uuid in
  let hash = Hash.hash_string ballot in
  let* x =
    Storage.append s uuid ~last [ Data ballot; Event (`Ballot, Some hash) ]
  in
  match x with
  | true ->
      let () = Public_archive.clear_ballot_cache uuid in
      return hash
  | false -> Lwt.fail @@ Failure "race condition in add_ballot"

let get_credential_weight s uuid credential =
  let* x = Storage.get s (Election (uuid, Credential_weight credential)) in
  match Lopt.get_value x with
  | None -> Lwt.return Weight.one
  | Some x -> Lwt.return x

let raw_compute_encrypted_tally s election =
  let module W = (val election : Site_common_sig.ELECTION) in
  let module GMap = Map.Make (W.G) in
  let uuid = W.uuid in
  let@ last cont =
    let* x = Storage.get s (Election (uuid, Last_event)) in
    match Lopt.get_value x with None -> assert false | Some x -> cont x
  in
  let* ballots =
    Public_archive.fold_on_ballots s uuid
      (fun _ b accu ->
        let ballot = W.read_ballot ++ b in
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
        let* weight = get_credential_weight s uuid (W.G.to_string credential) in
        Lwt.return @@ ((weight, ballot) :: accu))
      [] (GMap.bindings ballots)
  in
  let tally = W.E.process_ballots ballots in
  let tally_s = string_of_encrypted_tally W.(swrite G.to_string) tally in
  let payload =
    {
      sized_num_tallied = List.length ballots;
      sized_total_weight =
        List.fold_left
          (fun accu (w, _) -> Weight.(accu + w))
          Weight.zero ballots;
      sized_encrypted_tally = Hash.hash_string tally_s;
    }
    |> string_of_sized_encrypted_tally write_hash
  in
  let* x =
    Storage.append s uuid ~last
      [
        Event (`EndBallots, None);
        Data tally_s;
        Data payload;
        Event (`EncryptedTally, Some (Hash.hash_string payload));
      ]
  in
  match x with
  | true -> Storage.del s (Election (uuid, Audit_cache))
  | false -> Lwt.fail @@ Failure "race condition in raw_compute_encrypted_tally"

let get_credential_record s uuid credential =
  let* credential_mapping =
    Storage.get s (Election (uuid, Credential_mapping credential))
  in
  let&* { c_ballot = cr_ballot; _ } = Lopt.get_value credential_mapping in
  let* cr_username = get_credential_user s uuid credential in
  let* cr_weight = get_credential_weight s uuid credential in
  return_some { cr_ballot; cr_weight; cr_username }

type precast_data = {
  credential : string;
  credential_record : credential_record;
}

let precast_ballot s uuid ~ballot =
  let@ election =
    Public_archive.with_election s uuid ~fallback:(fun () ->
        Lwt.fail (Election_not_found (uuid, "precast_ballot")))
  in
  let module W = (val election) in
  let@ () =
   fun cont ->
    let hash = Hash.hash_string ballot in
    let* x = Public_archive.get_data s uuid hash in
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
    let* x = get_credential_record s uuid credential in
    match x with
    | None -> Lwt.return @@ Error `InvalidCredential
    | Some cr -> cont cr
  in
  if rc.rc_check () then Lwt.return @@ Ok { credential; credential_record }
  else Lwt.return @@ Error `InvalidBallot

let do_cast_ballot s election ~ballot ~user ~weight date ~precast_data =
  let module W = (val election : Site_common_sig.ELECTION) in
  let uuid = W.uuid in
  let@ last cont =
    let* x = Storage.get s (Election (uuid, Last_event)) in
    match Lopt.get_value x with None -> assert false | Some x -> cont x
  in
  let get_username user =
    match String.index_opt user ':' with
    | None -> user
    | Some i -> String.sub user (i + 1) (String.length user - i - 1)
  in
  let get_user_record user =
    let* x = Storage.get s (Election (uuid, Extended_record user)) in
    let&* { r_credential; _ } = Lopt.get_value x in
    return_some r_credential
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
    let* x = get_credential_record s uuid credential in
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
        |> (fun ballot -> { c_ballot = Some ballot; c_credential = credential })
        |> Storage.set s (Election (uuid, Credential_mapping credential)) Value
      in
      let* () =
        { r_username = user; r_date = date; r_credential = credential }
        |> Storage.set s (Election (uuid, Extended_record user)) Value
      in
      return (Ok (hash, revote))

let cast_ballot s uuid ~ballot ~user ~weight date ~precast_data =
  let@ election =
    Public_archive.with_election s uuid ~fallback:(fun () ->
        Lwt.fail (Election_not_found (uuid, "cast_ballot")))
  in
  do_cast_ballot s election ~ballot ~user ~weight date ~precast_data

let compute_audit_cache s uuid =
  let* election = Public_archive.get_election s uuid in
  match election with
  | None -> Lwt.return_none
  | Some _ ->
      let* voters = get_all_voters s uuid in
      let cache_voters_hash = Hash.hash_string (Voter.list_to_string voters) in
      let* shuffles =
        let* x = Public_archive.get_shuffles s uuid in
        let&* x = x in
        Lwt.return_some (List.map (fun (_, x, _) -> x) x)
      in
      let* encrypted_tally =
        let* x = Public_archive.get_sized_encrypted_tally s uuid in
        let&* x = x in
        let x = sized_encrypted_tally_of_string read_hash x in
        Lwt.return_some x.sized_encrypted_tally
      in
      let* trustees = Public_archive.get_trustees s uuid in
      let* cache_checksums =
        let* setup_data = get_setup_data s uuid in
        let election = setup_data.setup_election in
        let* public_credentials = Public_archive.get_public_creds s uuid in
        let* final =
          let* roots = Public_archive.get_roots s uuid in
          let&* _ = roots.roots_result in
          let* last_event = Storage.get s (Election (uuid, Last_event)) in
          Lwt.return
          @@ Option.map (fun x -> x.last_hash) (Lopt.get_value last_event)
        in
        Election.compute_checksums ~election ~shuffles ~encrypted_tally
          ~trustees ~public_credentials ~final
        |> Lwt.return
      in
      let* cache_sealing_log =
        let* x = Storage.get s (Election (uuid, Sealing_log)) in
        match Lopt.get_value x with
        | None -> Lwt.return_none
        | Some x -> Lwt.return_some @@ Hash.hash_string x
      in
      return_some
        {
          cache_voters_hash;
          cache_checksums;
          cache_threshold = None;
          cache_sealing_log;
        }

let get_audit_cache s uuid =
  let* cache = Storage.get s (Election (uuid, Audit_cache)) in
  match Lopt.get_value cache with
  | Some x -> return_some x
  | None -> (
      let* cache = compute_audit_cache s uuid in
      match cache with
      | None -> return_none
      | Some cache ->
          let* () = Storage.set s (Election (uuid, Audit_cache)) Value cache in
          return_some cache)

let regen_password s uuid ~admin_id user =
  let user = String.lowercase_ascii user in
  let* show_weight = get_has_explicit_weights s uuid in
  let* x = Storage.get s (Election (uuid, Voter user)) in
  let@ y = Storage.update s (Election (uuid, Password user)) in
  match (Lopt.get_value x, y) with
  | Some id, (r, set) ->
      let@ r cont =
        match Lopt.get_value r with
        | None -> Lwt.return_false
        | Some r -> cont r
      in
      let* email, (salt, hashed) =
        Mails_voter.generate_password_email ~admin_id uuid id show_weight
      in
      let r = { r with salt; hashed } in
      let* () = set Value r in
      let* () = Mails_voter_bulk.submit_bulk_emails [ email ] in
      Lwt.return_true
  | _ -> Lwt.return_false

let send_credentials uuid ~admin_id (Draft (v, se)) private_creds =
  let@ private_creds cont =
    match Lopt.get_value private_creds with
    | None -> Lwt.return_unit
    | Some x -> cont x
  in
  let@ () =
   fun cont -> if se.se_pending_credentials then cont () else Lwt.return_unit
  in
  let voter_map =
    List.fold_left
      (fun accu v ->
        let recipient, login, weight = Voter.get v.sv_id in
        SMap.add login (recipient, weight) accu)
      SMap.empty se.se_voters
  in
  let send = Mails_voter.generate_credential_email uuid (Draft (v, se)) in
  let* jobs =
    Lwt_list.fold_left_s
      (fun jobs (login, credential) ->
        match SMap.find_opt login voter_map with
        | None -> Lwt.return jobs
        | Some (recipient, weight) ->
            let* job = send ~admin_id ~recipient ~login ~weight ~credential in
            Lwt.return (job :: jobs))
      [] private_creds
  in
  let* () = Mails_voter_bulk.submit_bulk_emails jobs in
  se.se_pending_credentials <- false;
  Lwt.return_unit

let validate_election ~admin_id storage uuid
    ((Draft (v, se), set) : _ updatable_with_billing) s =
  let open Belenios_web_api in
  let questions =
    let x = se.se_questions in
    let t_administrator =
      match x.t_administrator with None -> se.se_administrator | x -> x
    in
    let t_credential_authority =
      match x.t_credential_authority with
      | None -> se.se_metadata.e_cred_authority
      | x -> x
    in
    { x with t_administrator; t_credential_authority }
  in
  (* convenience tests *)
  let validation_error x = raise (Api_generic.Error (`ValidationError x)) in
  let () =
    if questions.t_name = "" then validation_error `NoTitle;
    if questions.t_questions = [||] then validation_error `NoQuestions;
    (match questions.t_administrator with
    | None | Some "" -> validation_error `NoAdministrator
    | _ -> ());
    match questions.t_credential_authority with
    | None | Some "" -> validation_error `NoCredentialAuthority
    | _ -> ()
  in
  (* check status *)
  let () =
    if s.num_voters = 0 then validation_error `NoVoters;
    (match s.passwords_ready with
    | Some false -> validation_error `MissingPasswords
    | Some true | None -> ());
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
    match (!Web_config.billing, se.se_metadata.e_billing_request) with
    | None, _ -> Lwt.return_unit
    | Some _, None ->
        let* id = Billing.create ~admin_id ~uuid ~nb_voters:s.num_voters in
        let se_metadata = { se.se_metadata with e_billing_request = Some id } in
        let se = { se with se_metadata } in
        let* () = set ~billing:true (Draft (v, se) : draft_election) in
        validation_error (`MissingBilling id)
    | Some (url, _), Some id ->
        let* b = Billing.check ~url ~id in
        if b then Lwt.return_unit else validation_error (`MissingBilling id)
  in
  (* private credentials *)
  let* private_creds = Storage.get storage (Election (uuid, Private_creds)) in
  (* the validation itself *)
  let* x = Storage.validate_election storage uuid in
  match x with
  | Ok () -> send_credentials uuid ~admin_id (Draft (v, se)) private_creds
  | Error e -> validation_error e

let create_draft s uuid se = Storage.set s (Election (uuid, Draft)) Value se
let transition_to_encrypted_tally set_state = set_state `EncryptedTally

let compute_encrypted_tally s uuid =
  let@ state, set_state = update_election_state s uuid in
  match state with
  | `Closed ->
      let@ election =
        Public_archive.with_election s uuid ~fallback:(fun () ->
            Lwt.fail (Election_not_found (uuid, "compute_encrypted_tally")))
      in
      let module W = (val election) in
      let* () = raw_compute_encrypted_tally s election in
      if W.has_nh_questions then
        let* () = set_state `Shuffling in
        (* perform server-side shuffle *)
        let* cc = Public_archive.get_nh_ciphertexts s uuid in
        let cc = nh_ciphertexts_of_string W.(sread G.of_string) cc in
        let shuffle = W.E.shuffle_ciphertexts cc in
        let shuffle =
          string_of_shuffle
            W.(swrite G.to_string)
            W.(swrite G.Zq.to_string)
            shuffle
        in
        let* x = append_to_shuffles s election 1 shuffle in
        match x with
        | None -> Lwt.fail (Failure "server-side shuffle failed")
        | Some _ -> Lwt.return_true
      else
        let* () = transition_to_encrypted_tally set_state in
        Lwt.return_true
  | _ -> Lwt.return_false

let finish_shuffling s uuid =
  let@ state, set_state = update_election_state s uuid in
  match state with
  | `Shuffling ->
      let@ () =
       fun cont ->
        let* x = Storage.append s uuid [ Event (`EndShuffles, None) ] in
        match x with
        | true -> cont ()
        | false -> Lwt.fail @@ Failure "race condition in finish_shuffling"
      in
      let* () = Storage.set s (Election (uuid, State_state)) Value None in
      let* () = transition_to_encrypted_tally set_state in
      Lwt.return_true
  | _ -> Lwt.return_false

let set_election_state s uuid state =
  let@ state', set_state = update_election_state s uuid in
  let* allowed =
    match state' with
    | `Open | `Closed -> Lwt.return_some set_state
    | _ -> Lwt.return_none
  in
  match allowed with
  | Some set_state ->
      let* () = set_state (state : [ `Open | `Closed ] :> election_state) in
      let@ dates, set_dates = update_election_dates s uuid in
      let* () =
        set_dates
          { dates with e_date_auto_open = None; e_date_auto_close = None }
      in
      Lwt.return_true
  | None -> Lwt.return_false

let open_election s uuid = set_election_state s uuid `Open
let close_election s uuid = set_election_state s uuid `Closed

let get_election_automatic_dates s uuid =
  let open Belenios_web_api in
  let* d = get_election_dates s uuid in
  Lwt.return
    {
      auto_date_open = d.e_date_auto_open;
      auto_date_close = d.e_date_auto_close;
      auto_date_publish = d.e_date_publish;
    }

let set_election_automatic_dates s uuid d =
  let open Belenios_web_api in
  let e_date_auto_open = d.auto_date_open in
  let e_date_auto_close = d.auto_date_close in
  let e_date_publish = d.auto_date_publish in
  let@ dates, set = update_election_dates s uuid in
  set { dates with e_date_auto_open; e_date_auto_close; e_date_publish }

let get_draft_public_credentials s uuid =
  let* x = Storage.get s (Election (uuid, Public_creds)) in
  let&* x = Lopt.get_value x in
  let x =
    x |> List.map strip_public_credential |> string_of_public_credentials
  in
  Lwt.return_some x

let get_records s uuid =
  let* x = Storage.get s (Election (uuid, Records)) in
  let&* x = Lopt.get_value x in
  Lwt.return_some x

type credentials_status = [ `None | `Pending of int | `Done ]

let pending_generations = ref SMap.empty

let generate_credentials_on_server_async uuid (Draft (_, se)) =
  let uuid_s = Uuid.unwrap uuid in
  match SMap.find_opt uuid_s !pending_generations with
  | Some _ -> ()
  | None ->
      let voters = List.map (fun v -> v.sv_id) se.se_voters in
      let module G =
        (val Belenios.Group.of_string ~version:se.se_version se.se_group)
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
          end)
      in
      let t, p = Cred.generate_sub (List.length voters) in
      pending_generations := SMap.add uuid_s p !pending_generations;
      Lwt.async (fun () ->
          let* x = t in
          let Credential.{ private_creds; public_with_ids; _ } =
            Cred.merge_sub voters x
          in
          let@ s = Storage.with_transaction in
          let@ se, set = Storage.update s (Election (uuid, Draft)) in
          match Lopt.get_value se with
          | Some (Draft (v, se)) ->
              let* () =
                private_creds
                |> Storage.set s (Election (uuid, Private_creds)) Value
              in
              let* () =
                Storage.set s
                  (Election (uuid, Public_creds))
                  Value public_with_ids
              in
              se.se_public_creds_received <- true;
              se.se_pending_credentials <- true;
              let* () = set Value (Draft (v, se)) in
              pending_generations := SMap.remove uuid_s !pending_generations;
              Lwt.return_unit
          | None -> Lwt.return_unit)

let get_credentials_status uuid (Draft (_, se)) =
  match SMap.find_opt (Uuid.unwrap uuid) !pending_generations with
  | Some p -> `Pending (p ())
  | None -> if se.se_public_creds_received then `Done else `None
