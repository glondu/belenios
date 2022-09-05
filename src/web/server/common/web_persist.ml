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
open Web_serializable_builtin_t
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

let get_election_result uuid = Spool.get ~uuid Spool.result

let set_election_result_hidden uuid hidden =
  match hidden with
  | None -> Spool.del ~uuid Spool.hide_result
  | Some d -> Spool.set ~uuid Spool.hide_result d

let get_election_result_hidden uuid =
  let* t = Spool.get ~uuid Spool.hide_result in
  let&* t = t in
  if datetime_compare (now ()) t < 0 then
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

let get_election_state ?(update = true) uuid =
  let* x = Spool.get ~uuid Spool.state in
  let@ state = fun cont ->
    match x with Some x -> cont x | None -> return `Archived
  in
  let now = now () in
  let* dates = get_election_dates uuid in
  let past = function
    | None -> false
    | Some t -> datetime_compare t now < 0
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

let get_partial_decryptions uuid = Spool.get_default ~default:[] ~uuid Spool.partial_decryptions
let set_partial_decryptions uuid = Spool.set ~uuid Spool.partial_decryptions

let add_partial_decryption uuid pd =
  let* pds = get_partial_decryptions uuid in
  set_partial_decryptions uuid (pd :: pds)

let get_decryption_tokens uuid = Spool.get ~uuid Spool.decryption_tokens
let set_decryption_tokens uuid = Spool.set ~uuid Spool.decryption_tokens

let get_raw_election uuid = Spool.get ~uuid Spool.election

let empty_metadata = {
    e_owner = None;
    e_auth_config = None;
    e_cred_authority = None;
    e_trustees = None;
    e_languages = None;
    e_contact = None;
    e_booth_version = None;
  }

let get_election_metadata uuid =
  Spool.get_default ~default:empty_metadata ~uuid Spool.metadata

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

let get_id = function
  | `Id i -> return i
  | `User u ->
     let* x = Accounts.get_account u in
     match x with
     | None -> Lwt.fail Exit
     | Some a -> return [a.account_id]

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
                let uuid = uuid_of_raw_string uuid_s in
                let* election = get_draft_election uuid in
                match election with
                | None ->
                   (
                     let* metadata = get_election_metadata uuid in
                     match metadata.e_owner with
                     | None -> return accu
                     | Some o ->
                        let* id = get_id o in
                        let* election = get_raw_election uuid in
                        match election with
                        | None -> return accu
                        | Some election ->
                           let* dates = get_election_dates uuid in
                           let* kind, date =
                             let* state = get_election_state ~update:false uuid in
                             match state with
                             | `Open | `Closed | `Shuffling | `EncryptedTally _ ->
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
                           return @@ List.fold_left (fun accu id -> umap_add id item accu) accu id
                   )
                | Some se ->
                   let date = Option.value se.se_creation_date ~default:default_creation_date in
                   let* id = get_id se.se_owner in
                   let item = `Draft, uuid, date, se.se_questions.t_name in
                   return @@ List.fold_left (fun accu id -> umap_add id item accu) accu id
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

let build_elections_by_owner_cache () =
  let start = Unix.gettimeofday () in
  let () = Ocsigen_messages.accesslog "Building elections_by_owner_cache..." in
  Lwt.catch
    (fun () ->
      let@ () = Lwt_unix.with_timeout 10. in
      let* result = build_elections_by_owner_cache () in
      let duration = Unix.gettimeofday () -. start in
      let msg = Printf.sprintf "Built elections_by_owner_cache in %.3g seconds" duration in
      let () = Ocsigen_messages.accesslog msg in
      return result
    )
    (function
     | Lwt_unix.Timeout ->
        let msg = "Building elections_by_owner_cache timed out" in
        let () = Ocsigen_messages.accesslog msg in
        Lwt.fail (Failure msg)
     | e -> Lwt.fail e
    )

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
  let&* csv = csv in
  let res = List.fold_left (fun accu line ->
                match line with
                | [login; salt; hash] ->
                   SMap.add login (salt, hash) accu
                | _ -> accu
              ) SMap.empty csv in
  return_some res

let get_private_key uuid = Spool.get ~uuid Spool.private_key

let get_private_keys uuid = Spool.get_raw_list ~uuid Spool.private_keys

let get_trustees uuid =
  let* x = Spool.get ~uuid Spool.trustees in
  let@ () = fun cont ->
    match x with None -> cont () | Some x -> return x
  in
  let msg =
    Printf.sprintf "missing trustees for election %s"
      (raw_string_of_uuid uuid)
  in
  Lwt.fail (Failure msg)

module CredWeightsCacheTypes = struct
  type key = uuid
  type value = Weight.t SMap.t
end

module CredWeightsCache = Ocsigen_cache.Make (CredWeightsCacheTypes)

let raw_get_credential_weights uuid =
  Spool.get_fold_s_default ~uuid Spool.public_creds
    (fun x accu ->
      let x, w = extract_weight x in
      return @@ SMap.add x w accu
    ) SMap.empty

let credential_weights_cache =
  new CredWeightsCache.cache raw_get_credential_weights ~timer:3600. 10

let get_credential_weight uuid cred =
  Lwt.catch
    (fun () ->
      let* xs = credential_weights_cache#find uuid in
      return @@ SMap.find cred xs
    )
    (fun _ ->
      Lwt.fail
        (Failure
           (Printf.sprintf
              "could not find weight of %s/%s"
              (raw_string_of_uuid uuid) cred
           )
        )
    )

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

let raw_get_ballots_archived uuid =
  let* x = get_raw_election uuid in
  match x with
  | None -> return SMap.empty
  | Some x ->
     let module W = Election.Make (struct let raw_election = x end) (LwtRandom) () in
     Spool.get_fold_s_default ~uuid Spool.ballots
       (fun b accu ->
         let hash = sha256_b64 b in
         let* weight = get_ballot_weight (module W) b in
         return (SMap.add hash (b, weight) accu)
       ) SMap.empty

let archived_ballots_cache =
  new BallotsCache.cache raw_get_ballots_archived ~timer:3600. 10

let get_ballots_index uuid =
  let* x = Spool.get ~uuid Spool.ballots_index in
  match x with
  | None ->
     let dir = uuid /// "ballots" in
     Lwt.catch
       (fun () ->
         let* ballots = Lwt_unix.files_of_directory dir |> Lwt_stream.to_list in
         let ballots = List.filter (fun x -> x <> "." && x <> "..") ballots in
         return (List.rev_map (fun h -> unurlize h, Weight.one) ballots)
       )
       (function
        | Unix.Unix_error(Unix.ENOENT, "opendir", _) -> return []
        | e -> Lwt.fail e
       )
  | Some index ->
     let index =
       match index with
       | `Assoc index ->
          List.map
            (fun (hash, weight) -> hash, weight_of_json weight)
            index
       | _ -> failwith "anomaly in get_ballots_index (assoc expected)"
     in
     return index

let get_ballot_hashes uuid =
  let* state = get_election_state uuid in
  match state with
  | `Archived ->
     let* ballots = archived_ballots_cache#find uuid in
     SMap.bindings ballots |> List.map (fun (h, (_, w)) -> h, w) |> return
  | _ -> get_ballots_index uuid

let get_ballot_by_hash uuid hash =
  let* state = get_election_state uuid in
  match state with
  | `Archived ->
     let* ballots = archived_ballots_cache#find uuid in
     let&* b, _ =  SMap.find_opt hash ballots in
     return_some b
  | _ -> read_file_single_line ~uuid ("ballots" // urlize hash)

let load_ballots uuid =
  let ballots_dir = uuid /// "ballots" in
  let* b = Lwt_unix.file_exists ballots_dir in
  if b then (
    let ballots = Lwt_unix.files_of_directory ballots_dir in
    let* ballots = Lwt_stream.to_list ballots in
    Lwt_list.filter_map_s (fun x ->
        read_file_single_line (ballots_dir // x)
      ) ballots
  ) else return []

let dump_ballots election =
  let module W = (val election : Site_common_sig.ELECTION_LWT) in
  let uuid = W.election.e_uuid in
  let* ballots = load_ballots uuid in
  let* index =
    Lwt_list.map_s
      (fun b ->
        let* w = get_ballot_weight election b in
        return (sha256_b64 b, `Intlit (Weight.to_string w))
      ) ballots
  in
  let* () = Spool.set ~uuid Spool.ballots_index (`Assoc index) in
  write_file ~uuid "ballots.jsons" ballots

let add_ballot election ballot =
  let module W = (val election : Site_common_sig.ELECTION_LWT) in
  let uuid = W.election.e_uuid in
  let hash = sha256_b64 ballot in
  let ballots_dir = uuid /// "ballots" in
  let* () = Lwt.catch (fun () -> Lwt_unix.mkdir ballots_dir 0o755) (fun _ -> return_unit) in
  let* () = write_file (ballots_dir // urlize hash) [ballot] in
  let* () = dump_ballots election in
  return hash

let remove_ballot election hash =
  let module W = (val election : Site_common_sig.ELECTION_LWT) in
  let uuid = W.election.e_uuid in
  let ballots_dir = uuid /// "ballots" in
  Lwt.catch (fun () -> Lwt_unix.unlink (ballots_dir // urlize hash)) (fun _ -> return_unit)

let replace_ballot election ~hash ~rawballot =
  let* () = remove_ballot election hash in
  add_ballot election rawballot

let compute_encrypted_tally election =
  let module W = (val election : Site_common_sig.ELECTION_LWT) in
  let uuid = W.election.e_uuid in
  let* ballots = load_ballots uuid in
  let* ballots =
    Lwt_list.map_s
      (fun raw_ballot ->
        let ballot = W.ballot_of_string raw_ballot in
        let* weight = get_ballot_weight election raw_ballot in
        return (weight, ballot)
      ) ballots
  in
  let tally = W.E.process_ballots ballots in
  let* () = Spool.set ~uuid (Spool.encrypted_tally (module W.G)) tally in
  return @@ string_of_encrypted_tally W.G.write tally

let get_shuffle_token uuid = Spool.get ~uuid Spool.shuffle_token

let gen_shuffle_token uuid tk_trustee tk_name =
  let* tk_token = generate_token () in
  let t = {tk_trustee; tk_token; tk_name} in
  let* () = Spool.set ~uuid Spool.shuffle_token t in
  return t

let clear_shuffle_token uuid = Spool.del ~uuid Spool.shuffle_token

let get_nh_ciphertexts election =
  let module W = (val election : Site_common_sig.ELECTION_LWT) in
  let uuid = W.election.e_uuid in
  let* x =
    Spool.get_fold_s_default ~uuid Spool.shuffles
      (fun x _ ->
        let x = shuffle_of_string W.G.read x in
        return @@ fun () -> return @@ string_of_nh_ciphertexts W.G.write x.shuffle_ciphertexts
      ) (fun () ->
        let* x = Spool.get ~uuid (Spool.encrypted_tally (module W.G)) in
        match x with
        | Some x -> return @@ string_of_nh_ciphertexts W.G.write @@ W.E.extract_nh_ciphertexts x
        | _ -> Lwt.fail (Failure "get_nh_ciphertexts: encrypted tally not found or invalid")
      )
  in
  x ()

let get_shuffles uuid =
  let* election = get_raw_election uuid in
  let&* _ = election in
  Spool.get_raw_list ~uuid Spool.shuffles

let get_shuffle_hashes uuid =
  let* x =
    Spool.get_fold_s ~uuid Spool.shuffle_hashes
      (fun x accu -> return @@ x :: accu) []
  in
  return @@ Option.map List.rev x

let add_shuffle_hash uuid sh =
  let* current =
    let* x = get_shuffle_hashes uuid in
    match x with
    | None -> return []
    | Some x -> return x
  in
  let () =
    if List.exists (fun x -> x.sh_trustee = sh.sh_trustee) current then (
      Printf.ksprintf failwith "add_shuffle_hash(%s, %s): existing trustee"
        (raw_string_of_uuid uuid) sh.sh_trustee
    )
  in
  Spool.set_list ~uuid Spool.shuffle_hashes @@ current @ [sh]

let compute_encrypted_tally_after_shuffling election =
  let module W = (val election : Site_common_sig.ELECTION_LWT) in
  let file_encrypted_tally = Spool.encrypted_tally (module W.G) in
  let uuid = W.election.e_uuid in
  let* x = Spool.get ~uuid file_encrypted_tally in
  match x with
  | Some tally ->
     let* nh = get_nh_ciphertexts election in
     let nh = nh_ciphertexts_of_string W.G.read nh in
     let tally = W.E.merge_nh_ciphertexts nh tally in
     let* () = Spool.set ~uuid file_encrypted_tally tally in
     return_some @@ string_of_encrypted_tally W.G.write tally
  | _ -> return_none

let append_to_shuffles election shuffle =
  let module W = (val election : Site_common_sig.ELECTION_LWT) in
  let uuid = W.election.e_uuid in
  let shuffle = shuffle_of_string W.G.read shuffle in
  Web_election_mutex.with_lock uuid (fun () ->
      let* last_ciphertext = get_nh_ciphertexts election in
      let last_ciphertext = nh_ciphertexts_of_string W.G.read last_ciphertext in
      if W.E.check_shuffle last_ciphertext shuffle then (
        let* x =
          Spool.get_fold_s_default ~uuid Spool.shuffles
            (fun x accu -> return @@ x :: accu) []
        in
        let shuffle_ = string_of_shuffle W.G.write shuffle in
        let x = List.rev @@ shuffle_ :: x in
        let* () = Spool.set_list ~uuid Spool.shuffles x in
        return_some (sha256_b64 shuffle_)
      ) else return_none
    )

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
  let module X =
    struct
      type user = string
      let get_user_record user =
        let* x = find_extended_record uuid user in
        let&* _, old_credential = x in
        return_some old_credential
      let get_credential_record credential =
        let* x = find_credential_mapping uuid credential in
        let&* cr_ballot = x in
        let* cr_weight = get_credential_weight uuid credential in
        return_some {cr_ballot; cr_weight}
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
          let* h = add_ballot election rawballot in
          cont (h, false)
       | Some hash ->
          if !Web_config.deny_revote then (
            return @@ Error `RevoteNotAllowed
          ) else (
            let* h = replace_ballot election ~hash ~rawballot in
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
       "compute_cache: %s does not exist" (raw_string_of_uuid uuid)
  | Some election ->
     let* voters =
       let* x = Spool.get_raw_list ~uuid Spool.voters in
       match x with
       | Some x -> return x
       | None -> failwith "voters are missing"
     in
     let total_weight, min_weight, max_weight =
       let open Weight in
       let min a b =
         match a with
         | None -> Some b
         | Some a -> Some (min a b)
       and max a b =
         match a with
         | None -> Some b
         | Some a -> Some (max a b)
       in
       match
         List.fold_left
           (fun (tw, minw, maxw) voter ->
             let _, _, weight = split_identity voter in
             tw + weight, min minw weight, max maxw weight
           ) (zero, None, None) voters
       with
       | tw, Some minw, Some maxw -> tw, minw, maxw
       | _ -> failwith "no voters"
     in
     let cache_num_voters = List.length voters in
     let cache_total_weight, cache_min_weight, cache_max_weight =
       if not Weight.(is_int total_weight cache_num_voters) then (
         Some total_weight, Some min_weight, Some max_weight
       ) else (
         None, None, None
       )
     in
     let cache_voters_hash = Hash.hash_string (String.concat "\n" voters ^ "\n") in
     let* result_or_shuffles =
       let* raw_election_result = get_election_result uuid in
       match raw_election_result with
       | Some r -> return (`Result r)
       | None ->
          let* shuffles = get_shuffles uuid in
          match shuffles with
          | None -> return `Nothing
          | Some shuffles ->
             let* shuffle_hashes = get_shuffle_hashes uuid in
             match shuffle_hashes with
             | None -> return `Nothing
             | Some sh ->
                let sh = List.filter (fun x -> x.sh_hash <> "") sh in
                let shufflers = List.map (fun x -> x.sh_name) sh in
                return (`Shuffles (shuffles, Some shufflers))
     in
     let* trustees = get_trustees uuid in
     let* credentials =
       let* x = Spool.get_raw_list ~uuid Spool.public_creds in
       match x with
       | Some x -> return x
       | None -> failwith "public credentials are missing"
     in
     let public_credentials = String.concat "\n" credentials ^ "\n" in
     let cache_checksums =
       Election.compute_checksums ~election result_or_shuffles
         ~trustees ~public_credentials
     in
     return {
         cache_num_voters;
         cache_total_weight;
         cache_min_weight;
         cache_max_weight;
         cache_voters_hash;
         cache_checksums;
         cache_threshold = None;
       }

let get_audit_cache uuid =
  let* cache = Spool.get ~uuid Spool.audit_cache in
  match cache with
  | Some x when x.cache_checksums.ec_trustees_threshold <> None -> return x
  | _ ->
     let* cache = compute_audit_cache uuid in
     let* () = Spool.set ~uuid Spool.audit_cache cache in
     return cache

let remove_audit_cache uuid = Spool.del ~uuid Spool.audit_cache
