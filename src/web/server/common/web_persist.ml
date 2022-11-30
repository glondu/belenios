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

open Lwt
open Lwt.Syntax
open Belenios_core
open Signatures
open Belenios
open Serializable_builtin_t
open Serializable_j
open Common
open Web_serializable_j
open Web_common

let elections_by_owner_cache = ref None
let elections_by_owner_mutex = Lwt_mutex.create ()

let clear_elections_by_owner_cache () =
  let@ () = Lwt_mutex.with_lock elections_by_owner_mutex in
  elections_by_owner_cache := None;
  return_unit

let get_draft_election uuid = Spool.get ~uuid Spool.draft
let set_draft_election uuid = Spool.set ~uuid Spool.draft

let get_from_data uuid f =
  let* x = Web_events.get_roots ~uuid in
  match f x with
  | None -> Lwt.return_none
  | Some x -> Web_events.get_data ~uuid x

let get_setup_data uuid =
  let* x =
    let* x = Web_events.get_roots ~uuid in
    let&* x = x.roots_setup_data in
    Web_events.get_data ~uuid x
  in
  match x with
  | None -> assert false
  | Some x -> Lwt.return (setup_data_of_string x)

let get_from_setup_data uuid f =
  let* x = Web_events.get_roots ~uuid in
  match x.roots_setup_data with
  | None -> Lwt.return_none
  | Some x ->
     let* x = Web_events.get_data ~uuid x in
     match x with
     | None -> Lwt.return_none
     | Some x -> Web_events.get_data ~uuid (f (setup_data_of_string x))

let fold_on_event_payload_hashes uuid typ last_event f accu =
  let rec loop e accu =
    let* e = Web_events.get_event ~uuid e in
    match e with
    | None -> assert false
    | Some e ->
       if e.event_typ = typ then (
         match e.event_payload, e.event_parent with
         | Some payload, Some parent ->
            let* accu = f payload accu in
            loop parent accu
         | _ -> assert false
       ) else Lwt.return accu
  in
  loop last_event accu

let fold_on_event_payloads uuid typ last_event f accu =
  fold_on_event_payload_hashes uuid typ last_event
    (fun payload accu ->
      let* x = Web_events.get_data ~uuid payload in
      match x with
      | None -> assert false
      | Some x -> f payload x accu
    ) accu

let get_election_result uuid =
  get_from_data uuid (fun x -> x.roots_result)

let set_election_result_hidden uuid hidden =
  match hidden with
  | None -> Spool.del ~uuid Spool.hide_result
  | Some d -> Spool.set ~uuid Spool.hide_result d

let get_election_result_hidden uuid =
  let* t = Spool.get ~uuid Spool.hide_result in
  let&* t in
  if Datetime.compare (Datetime.now ()) t < 0 then
    return_some t
  else
    let* () = set_election_result_hidden uuid None in
    return_none

let default_dates =
  {
    e_creation = None;
    e_finalization = None;
    e_tally = None;
    e_archive = None;
    e_last_mail = None;
    e_auto_open = None;
    e_auto_close = None;
  }

let get_election_dates uuid = Spool.get_default ~default:default_dates ~uuid Spool.dates

let set_election_dates uuid x =
  let* () = Spool.set ~uuid Spool.dates x in
  clear_elections_by_owner_cache ()

let set_election_state uuid s =
  let* () = match s with
    | `Archived -> Spool.del ~uuid Spool.state
    | _ -> Spool.set ~uuid Spool.state s
  in
  clear_elections_by_owner_cache ()

let get_raw_election uuid =
  get_from_setup_data uuid (fun x -> x.setup_election)

let get_sized_encrypted_tally uuid =
  let* roots = Web_events.get_roots ~uuid in
  match roots.roots_encrypted_tally with
  | None -> Lwt.return_none
  | Some x ->
     let* x = Web_events.get_data ~uuid x in
     match x with
     | None -> assert false
     | Some x -> Lwt.return_some x

let get_nh_ciphertexts election =
  let module W = (val election : Site_common_sig.ELECTION_LWT) in
  let uuid = W.election.e_uuid in
  let* x = Web_events.get_roots ~uuid in
  match x.roots_last_shuffle_event with
  | None ->
     begin
       match x.roots_encrypted_tally with
       | None -> assert false
       | Some x ->
          let* x = Web_events.get_data ~uuid x in
          match x with
          | None -> assert false
          | Some x ->
             let x = sized_encrypted_tally_of_string read_hash x in
             let* x = Web_events.get_data ~uuid x.sized_encrypted_tally in
             match x with
             | None -> assert false
             | Some x ->
                encrypted_tally_of_string W.G.read x
                |> W.E.extract_nh_ciphertexts
                |> string_of_nh_ciphertexts W.G.write
                |> return
     end
  | Some x ->
     let* x = Web_events.get_event ~uuid x in
     match x with
     | None -> assert false
     | Some x ->
        match x.event_payload with
        | None -> assert false
        | Some x ->
           let* x = Web_events.get_data ~uuid x in
           match x with
           | None -> assert false
           | Some x ->
              let x = owned_of_string read_hash x in
              let* x = Web_events.get_data ~uuid x.owned_payload in
              match x with
              | None -> assert false
              | Some x ->
                 let x = shuffle_of_string W.G.read x in
                 return @@ string_of_nh_ciphertexts W.G.write x.shuffle_ciphertexts

let get_latest_encrypted_tally election =
  let module W = (val election : Site_common_sig.ELECTION_LWT) in
  let uuid = W.election.e_uuid in
  let* roots = Web_events.get_roots ~uuid in
  let@ tally = fun cont ->
    match roots.roots_encrypted_tally with
    | None -> return_none
    | Some x ->
       let* x = Web_events.get_data ~uuid x in
       match x with
       | None -> assert false
       | Some x ->
          let x = sized_encrypted_tally_of_string read_hash x in
          let* x = Web_events.get_data ~uuid x.sized_encrypted_tally in
          match x with
          | None -> assert false
          | Some x -> cont @@ encrypted_tally_of_string W.G.read x
  in
  let* nh = get_nh_ciphertexts election in
  let nh = nh_ciphertexts_of_string W.G.read nh in
  let tally = W.E.merge_nh_ciphertexts nh tally in
  return_some @@ string_of_encrypted_tally W.G.write tally

let get_trustees uuid =
  let* x = get_from_setup_data uuid (fun x -> x.setup_trustees) in
  let@ () = fun cont ->
    match x with None -> cont () | Some x -> return x
  in
  let msg =
    Printf.sprintf "missing trustees for election %s"
      (Uuid.unwrap uuid)
  in
  Lwt.fail (Failure msg)

let get_partial_decryptions uuid =
  let* x = Web_events.get_roots ~uuid in
  match x.roots_last_pd_event with
  | None -> Lwt.return []
  | Some x ->
     fold_on_event_payloads uuid `PartialDecryption x
       (fun _ x accu ->
         let x = owned_of_string read_hash x in
         let* pd =
           let* x = Web_events.get_data ~uuid x.owned_payload in
           match x with
           | None -> assert false
           | Some x -> Lwt.return x
         in
         let x = {x with owned_payload = pd} in
         Lwt.return @@ x :: accu
       ) []

let get_private_key uuid = Spool.get ~uuid Spool.private_key

let get_private_keys uuid = Spool.get_raw_list ~uuid Spool.private_keys

let empty_metadata = {
    e_owners = [];
    e_auth_config = None;
    e_cred_authority = None;
    e_trustees = None;
    e_languages = None;
    e_contact = None;
    e_booth_version = None;
  }

let get_election_metadata uuid =
  Spool.get_default ~default:empty_metadata ~uuid Spool.metadata

let get_owned_shuffles uuid =
  let* x = Web_events.get_roots ~uuid in
  match x.roots_last_shuffle_event with
  | None -> return_none
  | Some x ->
     let* x =
       fold_on_event_payloads uuid `Shuffle x
         (fun h x accu -> return @@ (h, owned_of_string read_hash x) :: accu) []
     in
     return_some x

let get_shuffles uuid =
  let* x = get_owned_shuffles uuid in
  match x with
  | None -> Lwt.return_none
  | Some x ->
     let* x =
       Lwt_list.map_s
         (fun (h, o) ->
           let* x = Web_events.get_data ~uuid o.owned_payload in
           match x with
           | None -> assert false
           | Some x -> Lwt.return (h, o, x)
         ) x
     in
     Lwt.return_some x

let make_result_transaction write_result result =
  let payload = string_of_election_result write_result result in
  let open Web_events in
  [
    Data payload;
    Event (`Result, Some (Hash.hash_string payload));
  ]

let remove_audit_cache uuid = Spool.del ~uuid Spool.audit_cache

let clear_shuffle_token uuid = Spool.del ~uuid Spool.shuffle_token

let internal_release_tally ~force uuid =
  let@ last = fun cont ->
    let* x = Spool.get ~uuid Spool.last_event in
    match x with
    | None -> assert false
    | Some x -> cont x
  in
  let* metadata = get_election_metadata uuid in
  let trustees_with_ids =
    Option.value metadata.e_trustees ~default:["server"]
    |> List.mapi (fun i x -> i + 1, x)
  in
  let* pds = get_partial_decryptions uuid in
  let@ () = fun cont ->
    if force then
      cont ()
    else
      (* check whether all trustees have done their job *)
      if
        List.for_all
          (fun (i, x) ->
            x = "server"
            || List.exists (fun x -> x.owned_owner = i) pds
          ) trustees_with_ids
      then cont ()
      else Lwt.return_false
  in
  let@ raw_election = fun cont ->
    let* x = get_raw_election uuid in
    match x with
    | None -> assert false
    | Some x -> cont x
  in
  let module W = Election.Make (struct let raw_election = raw_election end) (LwtRandom) () in
  let* tally =
    let* x = get_latest_encrypted_tally (module W) in
    match x with
    | None -> assert false
    | Some x -> Lwt.return @@ encrypted_tally_of_string W.G.read x
  in
  let* sized =
    let* x = get_sized_encrypted_tally uuid in
    match x with
    | None -> assert false
    | Some x ->
       let x = sized_encrypted_tally_of_string read_hash x in
       Lwt.return {x with sized_encrypted_tally = tally}
  in
  let* trustees =
    let* x = get_trustees uuid in
    Lwt.return @@ trustees_of_string W.G.read x
  in
  let* pds, transactions =
    let pds =
      List.rev_map
        (fun x ->
          let owned_payload = partial_decryption_of_string W.G.read x.owned_payload in
          {x with owned_payload}
        ) pds
    in
    let decrypt owned_owner =
      let* x = get_private_key uuid in
      match x with
      | None -> assert false
      | Some sk ->
         let* pd = W.E.compute_factor tally sk in
         let owned = {owned_owner; owned_payload = pd} in
         let pd = string_of_partial_decryption W.G.write pd in
         let payload =
           {
             owned_owner;
             owned_payload = Hash.hash_string pd;
           }
           |> string_of_owned write_hash
         in
         let transaction =
           let open Web_events in
           [
             Data pd;
             Data payload;
             Event (`PartialDecryption, Some (Hash.hash_string payload));
           ]
         in
         Lwt.return (owned, transaction)
    in
    Lwt_list.fold_left_s
      (fun ((pds, transactions) as accu) (i, t) ->
        if t = "server" then (
          if List.exists (fun x -> x.owned_owner = i) pds then (
            Lwt.return accu
          ) else (
            let* pd, transaction = decrypt i in
            Lwt.return (pd :: pds, transaction :: transactions)
          )
        ) else Lwt.return accu
      ) (pds, []) trustees_with_ids
  in
  match W.E.compute_result sized pds trustees with
  | Ok result ->
     let result_transaction = make_result_transaction W.write_result result in
     let* () =
       List.rev (result_transaction :: transactions)
       |> List.flatten
       |> Web_events.append ~uuid ~last
     in
     let* () = remove_audit_cache uuid in
     let* () = set_election_state uuid `Tallied in
     let* dates = get_election_dates uuid in
     let* () = set_election_dates uuid {dates with e_tally = Some (Datetime.now ())} in
     let* () = cleanup_file (uuid /// "decryption_tokens.json") in
     let* () = cleanup_file (uuid /// "shuffles.jsons") in
     let* () = clear_shuffle_token uuid in
     Lwt.return_true
  | Error e -> Lwt.fail @@ Failure (Trustees.string_of_combination_error e)

let get_election_state ?(update = true) ?(ignore_errors = true) uuid =
  let* x = Spool.get ~uuid Spool.state in
  let@ state = fun cont ->
    match x with Some x -> cont x | None -> return `Archived
  in
  let now = Datetime.now () in
  let* dates = get_election_dates uuid in
  let past = function
    | None -> false
    | Some t -> Datetime.compare t now < 0
  in
  let@ () = fun cont ->
    match state with
    | `EncryptedTally when update ->
       begin
         let* hidden = get_election_result_hidden uuid in
         match hidden with
         | Some _ when not (past hidden) -> cont ()
         | _ ->
            let@ () = fun cont2 ->
              if ignore_errors then
                Lwt.catch cont2 (fun _ -> cont ())
              else
                cont2 ()
            in
            let* b = internal_release_tally ~force:false uuid in
            return (if b then `Tallied else state)
       end
    | _ -> cont ()
  in
  let new_state = match state with
    | `Closed when past dates.e_auto_open -> `Open
    | x -> x
  in
  let new_state = match new_state with
    | `Open when past dates.e_auto_close -> `Closed
    | x -> x
  in
  let* () =
    if update && new_state <> state then set_election_state uuid new_state
    else return_unit
  in
  return new_state

let release_tally uuid =
  let* state = get_election_state uuid in
  match state with
  | `EncryptedTally ->
     let* b = internal_release_tally ~force:true uuid in
     assert b;
     set_election_state uuid `Tallied
  | _ -> Lwt.fail @@ Failure "election not in EncryptedTally state"

let add_partial_decryption uuid (owned_owner, pd) =
  let payload =
    {
      owned_owner;
      owned_payload = Hash.hash_string pd;
    }
    |> string_of_owned write_hash
  in
  Web_events.append ~uuid
    [
      Data pd;
      Data payload;
      Event (`PartialDecryption, Some (Hash.hash_string payload));
    ]

let get_decryption_tokens uuid = Spool.get ~uuid Spool.decryption_tokens
let set_decryption_tokens uuid = Spool.set ~uuid Spool.decryption_tokens

type election_kind =
  [ `Draft
  | `Validated
  | `Tallied
  | `Archived
  ]

let umap_add user x map =
  let xs =
    match IMap.find_opt user map with
    | None -> []
    | Some xs -> xs
  in
  IMap.add user (x :: xs) map

let build_elections_by_owner_cache () =
  Lwt_unix.files_of_directory !Web_config.spool_dir
  |> Lwt_stream.to_list
  >>= Lwt_list.fold_left_s
        (fun accu uuid_s ->
          if uuid_s = "." || uuid_s = ".." then
            return accu
          else (
            Lwt.catch
              (fun () ->
                let uuid = Uuid.wrap uuid_s in
                let* election = get_draft_election uuid in
                match election with
                | None ->
                   (
                     let* metadata = get_election_metadata uuid in
                     let ids = metadata.e_owners in
                     let* election = get_raw_election uuid in
                     match election with
                     | None -> return accu
                     | Some election ->
                        let* dates = get_election_dates uuid in
                        let* kind, date =
                          let* state = get_election_state ~update:false uuid in
                          match state with
                          | `Open | `Closed | `Shuffling | `EncryptedTally ->
                             let date = Option.value dates.e_finalization ~default:default_validation_date in
                             return (`Validated, date)
                          | `Tallied ->
                             let date = Option.value dates.e_tally ~default:default_tally_date in
                             return (`Tallied, date)
                          | `Archived ->
                             let date = Option.value dates.e_archive ~default:default_archive_date in
                             return (`Archived, date)
                        in
                        let election = Election.of_string election in
                        let item = kind, uuid, date, election.e_name in
                        return @@ List.fold_left (fun accu id -> umap_add id item accu) accu ids
                   )
                | Some se ->
                   let date = Option.value se.se_creation_date ~default:default_creation_date in
                   let ids = se.se_owners in
                   let item = `Draft, uuid, date, se.se_questions.t_name in
                   return @@ List.fold_left (fun accu id -> umap_add id item accu) accu ids
              )
              (function
               | Lwt.Canceled ->
                  Printf.ksprintf Ocsigen_messages.accesslog
                    "Building elections_by_owner_cache canceled while processing %s"
                    uuid_s;
                  Lwt.fail Lwt.Canceled
               | _ -> return accu)
          )
        ) IMap.empty

let get_elections_by_owner user =
  let* cache =
    match !elections_by_owner_cache with
    | Some x -> return x
    | None ->
       let@ () = Lwt_mutex.with_lock elections_by_owner_mutex in
       let* x = build_elections_by_owner_cache () in
       elections_by_owner_cache := Some x;
       return x
  in
  match IMap.find_opt user cache with
  | None -> return []
  | Some xs -> return xs

let get_voters uuid = Spool.get_raw_list ~uuid Spool.voters

let get_passwords uuid =
  let csv =
    try Some (Csv.load (uuid /// "passwords.csv"))
    with _ -> None
  in
  let&* csv in
  let res = List.fold_left (fun accu line ->
                match line with
                | [login; salt; hash] ->
                   SMap.add login (salt, hash) accu
                | _ -> accu
              ) SMap.empty csv in
  return_some res

type cred_cache =
  {
    weight : Weight.t;
    username : string option;
  }

module CredCacheTypes = struct
  type key = uuid
  type value = cred_cache SMap.t
end

module CredCache = Ocsigen_cache.Make (CredCacheTypes)

let get_public_creds uuid =
  let* x = get_from_setup_data uuid (fun x -> x.setup_credentials) in
  match x with
  | None -> assert false
  | Some x -> return @@ public_credentials_of_string x

let raw_get_credential_cache uuid =
  let* x = read_file_single_line ~uuid "public_creds.json" in
  match x with
  | None ->
     let* x = get_public_creds uuid in
     List.fold_left
       (fun accu x ->
         let x, weight = extract_weight x in
         SMap.add x {weight; username = None} accu
       ) SMap.empty x
     |> return
  | Some x ->
     let x = public_credentials_of_string x in
     List.fold_left
       (fun accu x ->
         let cred, weight, username =
           match String.split_on_char ',' x with
           | [x] -> x, Weight.one, None
           | [x; y] -> x, Weight.of_string y, None
           | [x; ""; z] -> x, Weight.one, Some z
           | [x; y; z] -> x, Weight.of_string y, Some z
           | _ -> assert false
         in
         SMap.add cred {weight; username} accu
       ) SMap.empty x
     |> return

let credential_cache =
  new CredCache.cache raw_get_credential_cache ~timer:3600. 10

let get_credential_cache uuid cred =
  Lwt.catch
    (fun () ->
      let* xs = credential_cache#find uuid in
      return @@ SMap.find cred xs
    )
    (fun _ ->
      Lwt.fail
        (Failure
           (Printf.sprintf
              "could not find credential record of %s/%s"
              (Uuid.unwrap uuid) cred
           )
        )
    )

let get_credential_weight uuid cred =
  let* x = get_credential_cache uuid cred in
  Lwt.return x.weight

let get_ballot_weight election ballot =
  let module W = (val election : Site_common_sig.ELECTION_LWT) in
  Lwt.catch
    (fun () ->
      let ballot = W.ballot_of_string ballot in
      match W.get_credential ballot with
      | None -> failwith "missing signature"
      | Some credential ->
         get_credential_weight W.election.e_uuid (W.G.to_string credential)
    )
    (fun e ->
      Printf.ksprintf failwith
        "anomaly in get_ballot_weight (%s)"
        (Printexc.to_string e)
    )

module BallotsCacheTypes = struct
  type key = uuid
  type value = (string * Weight.t) SMap.t
end

module BallotsCache = Ocsigen_cache.Make (BallotsCacheTypes)

let fold_on_ballots uuid f accu =
  let* x = Web_events.get_roots ~uuid in
  match x.roots_last_ballot_event with
  | None -> Lwt.return accu
  | Some e -> fold_on_event_payloads uuid `Ballot e f accu

let fold_on_ballots_weeded uuid f accu =
  let@ raw_election = fun cont ->
    let* x = get_raw_election uuid in
    match x with
    | None -> Lwt.return accu
    | Some x -> cont x
  in
  let module W = Election.Make (struct let raw_election = raw_election end) (LwtRandom) () in
  let module GSet = Set.Make (W.G) in
  let* _, accu =
    fold_on_ballots uuid
      (fun _ b ((seen, accu) as x) ->
        let ballot = W.ballot_of_string b in
        match W.get_credential ballot with
        | None -> assert false
        | Some credential ->
           if GSet.mem credential seen then
             Lwt.return x
           else
             let seen = GSet.add credential seen in
             let* accu = f b accu in
             Lwt.return (seen, accu)
      ) (GSet.empty, accu)
  in
  Lwt.return accu

let raw_get_ballots uuid =
  let* x = get_raw_election uuid in
  match x with
  | None -> return SMap.empty
  | Some x ->
     let module W = Election.Make (struct let raw_election = x end) (LwtRandom) () in
     fold_on_ballots_weeded uuid
       (fun b accu ->
         let hash = sha256_b64 b in
         let* weight = get_ballot_weight (module W) b in
         return (SMap.add hash (b, weight) accu)
       ) SMap.empty

let ballots_cache =
  new BallotsCache.cache raw_get_ballots ~timer:3600. 10

let get_ballot_hashes uuid =
  let* ballots = ballots_cache#find uuid in
  SMap.bindings ballots |> List.map (fun (h, (_, w)) -> h, w) |> return

let get_ballot_by_hash uuid hash =
  Lwt.catch
    (fun () ->
      let hash = Hash.of_b64 hash in
      Web_events.get_data ~uuid hash
    ) (fun _ -> Lwt.return_none)

let add_ballot election last ballot =
  let module W = (val election : Site_common_sig.ELECTION_LWT) in
  let uuid = W.election.e_uuid in
  let hash = sha256_b64 ballot in
  let* () =
    Web_events.append ~lock:false ~uuid ~last
      [
        Data ballot;
        Event (`Ballot, Some (Hash.hash_string ballot));
      ]
  in
  let () = ballots_cache#remove uuid in
  return hash

let compute_encrypted_tally election =
  let module W = (val election : Site_common_sig.ELECTION_LWT) in
  let module GMap = Map.Make (W.G) in
  let uuid = W.election.e_uuid in
  let@ last = fun cont ->
    let* x = Spool.get ~uuid Spool.last_event in
    match x with
    | None -> assert false
    | Some x -> cont x
  in
  let* ballots =
    fold_on_ballots uuid
      (fun _ b accu ->
        let ballot = W.ballot_of_string b in
        match W.get_credential ballot with
        | None -> assert false
        | Some credential ->
           if GMap.mem credential accu then
             Lwt.return accu
           else
             Lwt.return @@ GMap.add credential ballot accu
      ) GMap.empty
  in
  let* ballots =
    Lwt_list.fold_left_s
      (fun accu (credential, ballot) ->
        let* weight = get_credential_weight uuid (W.G.to_string credential) in
        Lwt.return @@ (weight, ballot) :: accu
      ) [] (GMap.bindings ballots)
  in
  let tally = W.E.process_ballots ballots in
  let tally_s = string_of_encrypted_tally W.G.write tally in
  let payload =
    {
      sized_num_tallied = List.length ballots;
      sized_total_weight =
        List.fold_left
          (fun accu (w, _) -> Weight.(accu + w)) Weight.zero ballots;
      sized_encrypted_tally = Hash.hash_string tally_s;
    }
    |> string_of_sized_encrypted_tally write_hash
  in
  let* () =
    Web_events.append ~uuid ~last
      [
        Event (`EndBallots, None);
        Data tally_s;
        Data payload;
        Event (`EncryptedTally, Some (Hash.hash_string payload));
      ]
  in
  return_unit

let get_shuffle_token uuid = Spool.get ~uuid Spool.shuffle_token

let gen_shuffle_token uuid tk_trustee tk_trustee_id tk_name =
  let* tk_token = generate_token () in
  let t = {tk_trustee; tk_token; tk_trustee_id; tk_name} in
  let* () = Spool.set ~uuid Spool.shuffle_token t in
  return t

let append_to_shuffles election owned_owner shuffle_s =
  let module W = (val election : Site_common_sig.ELECTION_LWT) in
  let uuid = W.election.e_uuid in
  let@ last = fun cont ->
    let* x = Spool.get ~uuid Spool.last_event in
    match x with
    | None -> assert false
    | Some x -> cont x
  in
  let shuffle = shuffle_of_string W.G.read shuffle_s in
  let shuffle_h = Hash.hash_string shuffle_s in
  let* last_nh = get_nh_ciphertexts election in
  let last_nh = nh_ciphertexts_of_string W.G.read last_nh in
  if string_of_shuffle W.G.write shuffle = shuffle_s && W.E.check_shuffle last_nh shuffle then (
    let owned =
      {
        owned_owner;
        owned_payload = shuffle_h;
      }
    in
    let owned_s = string_of_owned write_hash owned in
    let* () =
      Web_events.append ~uuid ~last
        [
          Data shuffle_s;
          Data owned_s;
          Event (`Shuffle, Some (Hash.hash_string owned_s));
        ]
    in
    return_some @@ sha256_b64 shuffle_s
  ) else return_none

module ExtendedRecordsCacheTypes = struct
  type key = uuid
  type value = (datetime * string) SMap.t
end

module ExtendedRecordsCache = Ocsigen_cache.Make (ExtendedRecordsCacheTypes)

let raw_get_extended_records uuid =
  Spool.get_fold_s_default ~uuid Spool.extended_records
    (fun x accu ->
      return @@ SMap.add x.r_username (x.r_date, x.r_credential) accu
    ) SMap.empty

let dump_extended_records uuid rs =
  let rs = SMap.bindings rs in
  let extended_records =
    List.map (fun (r_username, (r_date, r_credential)) ->
        {r_username; r_date; r_credential}
      ) rs
  in
  let records =
    List.map (fun (u, (d, _)) ->
        Printf.sprintf "%s %S" (string_of_datetime d) u
      ) rs
  in
  let* () = Spool.set_list ~uuid Spool.extended_records extended_records in
  Spool.set_list ~uuid Spool.records records

let extended_records_cache =
  new ExtendedRecordsCache.cache raw_get_extended_records ~timer:3600. 10

let find_extended_record uuid username =
  let* rs = extended_records_cache#find uuid in
  return (SMap.find_opt username rs)

let add_extended_record uuid username r =
  let* rs = extended_records_cache#find uuid in
  let rs = SMap.add username r rs in
  extended_records_cache#add uuid rs;
  dump_extended_records uuid rs

let has_voted uuid user =
  let* rs = extended_records_cache#find uuid in
  return @@ SMap.mem (string_of_user user) rs

module CredMappingsCacheTypes = struct
  type key = uuid
  type value = string option SMap.t
end

module CredMappingsCache = Ocsigen_cache.Make (CredMappingsCacheTypes)

let raw_get_credential_mappings uuid =
  Spool.get_fold_s_default ~uuid Spool.credential_mappings
    (fun x accu -> return @@ SMap.add x.c_credential x.c_ballot accu)
    SMap.empty

let dump_credential_mappings uuid xs =
  SMap.fold
    (fun c_credential c_ballot accu -> {c_credential; c_ballot} :: accu)
    xs []
  |> List.rev
  |> Spool.set_list ~uuid Spool.credential_mappings

let credential_mappings_cache =
  new CredMappingsCache.cache raw_get_credential_mappings ~timer:3600. 10

let init_credential_mapping uuid xs =
  let xs =
    List.fold_left
      (fun accu x ->
        let x, _ = extract_weight x in
        if SMap.mem x accu then
          failwith "trying to add duplicate credential"
        else
          SMap.add x None accu
      ) SMap.empty xs
  in
  credential_mappings_cache#add uuid xs;
  dump_credential_mappings uuid xs

let find_credential_mapping uuid cred =
  let* xs = credential_mappings_cache#find uuid in
  return @@ SMap.find_opt cred xs

let add_credential_mapping uuid cred mapping =
  let* xs = credential_mappings_cache#find uuid in
  let xs = SMap.add cred mapping xs in
  credential_mappings_cache#add uuid xs;
  dump_credential_mappings uuid xs

let do_cast_ballot election ~rawballot ~user ~weight date =
  let module W = (val election : Site_common_sig.ELECTION_LWT) in
  let uuid = W.election.e_uuid in
  let@ last = fun cont ->
    let* x = Spool.get ~uuid Spool.last_event in
    match x with
    | None -> assert false
    | Some x -> cont x
  in
  let module X =
    struct
      type user = string
      let get_username user =
        match String.index_opt user ':' with
        | None -> user
        | Some i -> String.sub user (i + 1) (String.length user - i - 1)
      let get_user_record user =
        let* x = find_extended_record uuid user in
        let&* _, old_credential = x in
        return_some old_credential
      let get_credential_record credential =
        let* cr_ballot = find_credential_mapping uuid credential in
        let&* cr_ballot in
        let* c = get_credential_cache uuid credential in
        return_some {cr_ballot; cr_weight = c.weight; cr_username = c.username}
    end
  in
  let module B = W.E.CastBallot (X) in
  let* x = B.cast ~user ~weight rawballot in
  match x with
  | Error _ as x -> return x
  | Ok (credential, _, old) ->
     let@ hash, revote = fun cont ->
       match old with
       | None ->
          let* h = add_ballot election last rawballot in
          cont (h, false)
       | Some _ ->
          if !Web_config.deny_revote then (
            return @@ Error `RevoteNotAllowed
          ) else (
            let* h = add_ballot election last rawballot in
            cont (h, true)
          )
     in
     let* () = add_credential_mapping uuid credential (Some hash) in
     let* () = add_extended_record uuid user (date, credential) in
     return (Ok (hash, revote))

let cast_ballot election ~rawballot ~user ~weight date =
  let module W = (val election : Site_common_sig.ELECTION_LWT) in
  let uuid = W.election.e_uuid in
  Web_election_mutex.with_lock uuid
    (fun () -> do_cast_ballot election ~rawballot ~user ~weight date)

let compute_audit_cache uuid =
  let* election = get_raw_election uuid in
  match election with
  | None ->
     Printf.ksprintf failwith
       "compute_cache: %s does not exist" (Uuid.unwrap uuid)
  | Some election ->
     let* voters =
       let* x = Spool.get_raw_list ~uuid Spool.voters in
       match x with
       | Some x -> return x
       | None -> failwith "voters are missing"
     in
     let cache_voters_hash = Hash.hash_string (String.concat "\n" voters ^ "\n") in
     let* shuffles =
       let* x = get_shuffles uuid in
       let&* x in
       Lwt.return_some (List.map (fun (_, x, _) -> x) x)
     in
     let* encrypted_tally =
       let module W = Election.Make (struct let raw_election = election end) (LwtRandom) () in
       let* x = get_latest_encrypted_tally (module W) in
       let&* x in
       Lwt.return_some (Hash.hash_string x)
     in
     let* trustees = get_trustees uuid in
     let* cache_checksums =
       let* setup_data = get_setup_data uuid in
       let election = setup_data.setup_election in
       let* public_credentials = get_public_creds uuid in
       Election.compute_checksums ~election ~shuffles ~encrypted_tally
         ~trustees ~public_credentials
       |> Lwt.return
     in
     return {
         cache_voters_hash;
         cache_checksums;
         cache_threshold = None;
       }

let get_audit_cache uuid =
  let* cache = Spool.get ~uuid Spool.audit_cache in
  match cache with
  | Some x -> return x
  | None ->
     let* cache = compute_audit_cache uuid in
     let* () = Spool.set ~uuid Spool.audit_cache cache in
     return cache
