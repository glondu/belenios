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
open Belenios_platform
open Belenios_core
open Signatures
open Belenios
open Platform
open Serializable_builtin_t
open Serializable_j
open Common
open Web_serializable_builtin_t
open Web_serializable_j
open Web_common

let ( / ) = Filename.concat

module UMap = Map.Make (struct type t = int let compare = compare end)

let elections_by_owner_cache = ref None
let elections_by_owner_mutex = Lwt_mutex.create ()

let clear_elections_by_owner_cache () =
  let@ () = Lwt_mutex.with_lock elections_by_owner_mutex in
  elections_by_owner_cache := None;
  return_unit

let get_draft_election uuid =
  let* file = read_file ~uuid "draft.json" in
  match file with
  | Some [x] -> return_some (draft_election_of_string x)
  | _ -> return_none

let set_draft_election uuid se =
  write_file ~uuid "draft.json" [string_of_draft_election se]

let get_election_result uuid =
  let* file = read_file ~uuid "result.json" in
  match file with
  | Some [x] -> return_some x
  | _ -> return_none

let set_election_result_hidden uuid hidden =
  match hidden with
  | None ->
     Lwt.catch
       (fun () ->
         Lwt_unix.unlink
           (!Web_config.spool_dir / raw_string_of_uuid uuid / "hide_result")
       )
       (fun _ -> Lwt.return_unit)
  | Some d -> write_file ~uuid "hide_result" [string_of_datetime d]

let get_election_result_hidden uuid =
  let* file = read_file ~uuid "hide_result" in
  match file with
  | Some [x] ->
     let t = datetime_of_string x in
     if datetime_compare (now ()) t < 0 then
       return_some t
     else
       let* () = set_election_result_hidden uuid None in
       return_none
  | _ -> return_none

let get_election_dates uuid =
  let* file = read_file ~uuid "dates.json" in
  match file with
  | Some [x] -> return (election_dates_of_string x)
  | _ -> return {
             e_creation = None;
             e_finalization = None;
             e_tally = None;
             e_archive = None;
             e_last_mail = None;
             e_auto_open = None;
             e_auto_close = None;
           }

let set_election_dates uuid dates =
  let* () = write_file ~uuid "dates.json" [string_of_election_dates dates] in
  clear_elections_by_owner_cache ()

let set_election_state uuid s =
  let* () = match s with
    | `Archived ->
       Lwt.catch
         (fun () ->
           Lwt_unix.unlink (!Web_config.spool_dir / raw_string_of_uuid uuid / "state.json")
         )
         (fun _ -> return_unit)
    | _ -> write_file ~uuid "state.json" [string_of_election_state s]
  in
  clear_elections_by_owner_cache ()

let get_election_state uuid =
  let* file = read_file ~uuid "state.json" in
  match file with
  | Some [x] ->
     let state = election_state_of_string x and now = now () in
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
       if new_state <> state then set_election_state uuid new_state
       else return_unit
     in
     return new_state
  | _ -> return `Archived

let get_partial_decryptions uuid =
  let* file = read_file ~uuid "partial_decryptions.json" in
  match file with
  | Some [x] -> return @@ partial_decryptions_of_string x
  | _ -> return []

let set_partial_decryptions uuid pds =
  write_file ~uuid "partial_decryptions.json"
    [string_of_partial_decryptions pds]

let get_decryption_tokens uuid =
  let* file = read_file ~uuid "decryption_tokens.json" in
  match file with
  | Some [x] -> return_some (decryption_tokens_of_string x)
  | _ -> return_none

let set_decryption_tokens uuid pds =
  write_file ~uuid "decryption_tokens.json"
    [string_of_decryption_tokens pds]

let get_raw_election uuid =
  let* file = read_file ~uuid "election.json" in
  match file with
  | Some [x] -> return_some x
  | _ -> return_none

let empty_metadata = {
    e_owner = None;
    e_auth_config = None;
    e_cred_authority = None;
    e_trustees = None;
    e_languages = None;
    e_contact = None;
    e_server_is_trustee = None;
    e_booth_version = None;
  }

let return_empty_metadata = return empty_metadata

let get_election_metadata uuid =
  let* file = read_file ~uuid "metadata.json" in
  match file with
  | Some [x] -> return (metadata_of_string x)
  | _ -> return_empty_metadata

type election_kind =
  [ `Draft
  | `Validated
  | `Tallied
  | `Archived
  ]

let umap_add user x map =
  let xs =
    match UMap.find_opt user map with
    | None -> []
    | Some xs -> xs
  in
  UMap.add user (x :: xs) map

let get_id = function
  | `Id i -> return i
  | `User u ->
     let* x = Accounts.get_account u in
     match x with
     | None -> Lwt.fail Exit
     | Some a -> return a.account_id

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
                             let* state = get_election_state uuid in
                             match state with
                             | `Open | `Closed | `Shuffling | `EncryptedTally _ ->
                                let date = Option.get dates.e_finalization default_validation_date in
                                return (`Validated, date)
                             | `Tallied ->
                                let date = Option.get dates.e_tally default_tally_date in
                                return (`Tallied, date)
                             | `Archived ->
                                let date = Option.get dates.e_archive default_archive_date in
                                return (`Archived, date)
                           in
                           let election = Election.of_string election in
                           return @@ umap_add id (kind, uuid, date, election.e_name) accu
                   )
                | Some se ->
                   let date = Option.get se.se_creation_date default_creation_date in
                   let* id = get_id se.se_owner in
                   return @@ umap_add id (`Draft, uuid, date, se.se_questions.t_name) accu
              )
              (fun _ -> return accu)
          )
        ) UMap.empty

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
  match UMap.find_opt user cache with
  | None -> return []
  | Some xs -> return xs

let get_voters uuid =
  read_file ~uuid "voters.txt"

let get_passwords uuid =
  let csv =
    try Some (Csv.load (!Web_config.spool_dir / raw_string_of_uuid uuid / "passwords.csv"))
    with _ -> None
  in
  match csv with
  | None -> return_none
  | Some csv ->
     let res = List.fold_left (fun accu line ->
                   match line with
                   | [login; salt; hash] ->
                      SMap.add login (salt, hash) accu
                   | _ -> accu
                 ) SMap.empty csv in
     return_some res

let get_private_key uuid =
  let* file = read_file ~uuid "private_key.json" in
  match file with
  | Some [x] -> return_some (number_of_string x)
  | _ -> return_none

let get_private_keys uuid =
  read_file ~uuid "private_keys.jsons"

let get_trustees uuid =
  let* file = read_file ~uuid "trustees.json" in
  match file with
  | Some [x] -> return x
  | _ ->
     let msg =
       Printf.sprintf "missing trustees.json for election %s"
         (raw_string_of_uuid uuid)
     in
     Lwt.fail (Failure msg)

let get_public_keys uuid =
  read_file ~uuid "public_keys.jsons"

let get_threshold uuid =
  let* file = read_file ~uuid "threshold.json" in
  match file with
  | Some [x] -> return_some x
  | _ -> return_none

let get_trustees_legacy uuid =
  let* x = get_threshold uuid in
  match x with
  | Some x ->
     x
     |> threshold_parameters_of_string Yojson.Safe.read_json
     |> (fun x -> [`Pedersen x])
     |> string_of_trustees Yojson.Safe.write_json
     |> return_some
  | None ->
     let* x = get_public_keys uuid in
     match x with
     | Some x ->
        x
        |> List.map (web_trustee_public_key_of_string Yojson.Safe.read_json)
        |> List.map (fun x -> `Single (unwebize_trustee_public_key x))
        |> string_of_trustees Yojson.Safe.write_json
        |> return_some
     | None -> return_none

let convert_trustees () =
  Lwt_unix.files_of_directory !Web_config.spool_dir
  |> Lwt_stream.to_list
  >>= Lwt_list.iter_s
        (fun x ->
          if x = "." || x = ".." then
            return_unit
          else
            let uuid = uuid_of_raw_string x in
            let* legacy = get_trustees_legacy uuid in
            match legacy with
            | None -> return_unit
            | Some trustees ->
               let* () = write_file ~uuid "trustees.json" [trustees] in
               let* () = cleanup_file (!Web_config.spool_dir / x / "threshold.json") in
               cleanup_file (!Web_config.spool_dir / x / "public_keys.jsons")
        )

module StringMap = Map.Make (String)

module CredWeightsCacheTypes = struct
  type key = uuid
  type value = Weight.t StringMap.t
end

module CredWeightsCache = Ocsigen_cache.Make (CredWeightsCacheTypes)

let raw_get_credential_weights uuid =
  let* file = read_file ~uuid "public_creds.txt" in
  match file with
  | Some xs ->
     xs
     |> List.map extract_weight
     |> List.fold_left
          (fun accu (x, w) ->
            StringMap.add x w accu
          ) StringMap.empty
     |> return
  | None -> return StringMap.empty

let credential_weights_cache =
  new CredWeightsCache.cache raw_get_credential_weights ~timer:3600. 10

let get_credential_weight uuid cred =
  Lwt.catch
    (fun () ->
      let* xs = credential_weights_cache#find uuid in
      return @@ StringMap.find cred xs
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
  type value = (string * Weight.t) StringMap.t
end

module BallotsCache = Ocsigen_cache.Make (BallotsCacheTypes)

let raw_get_ballots_archived uuid =
  let* x = get_raw_election uuid in
  match x with
  | None -> return StringMap.empty
  | Some x ->
     let module W = Election.Make (struct let raw_election = x end) (LwtRandom) () in
     let* file = read_file ~uuid "ballots.jsons" in
     match file with
     | Some bs ->
        Lwt_list.fold_left_s (fun accu b ->
            let hash = sha256_b64 b in
            let* weight = get_ballot_weight (module W) b in
            return (StringMap.add hash (b, weight) accu)
          ) StringMap.empty bs
     | None -> return StringMap.empty

let archived_ballots_cache =
  new BallotsCache.cache raw_get_ballots_archived ~timer:3600. 10

let get_ballots_index uuid =
  let* index = read_file ~uuid "ballots_index.json" in
  match index with
  | None ->
     let uuid_s = raw_string_of_uuid uuid in
     let dir = !Web_config.spool_dir / uuid_s / "ballots" in
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
  | Some [index] ->
     let index =
       match Yojson.Safe.from_string index with
       | `Assoc index ->
          List.map
            (fun (hash, weight) -> hash, weight_of_json weight)
            index
       | _ -> failwith "anomaly in get_ballots_index (assoc expected)"
     in
     return index
  | _ -> failwith "anomaly in get_ballots_index (invalid ballots_index.json)"

let get_ballot_hashes uuid =
  let* state = get_election_state uuid in
  match state with
  | `Archived ->
     let* ballots = archived_ballots_cache#find uuid in
     StringMap.bindings ballots |> List.map (fun (h, (_, w)) -> h, w) |> return
  | _ -> get_ballots_index uuid

let get_ballot_by_hash uuid hash =
  let* state = get_election_state uuid in
  match state with
  | `Archived ->
     let* ballots = archived_ballots_cache#find uuid in
     (match StringMap.find_opt hash ballots with
      | Some (b, _) -> return_some b
      | None -> return_none
     )
  | _ ->
     let* ballot = read_file ~uuid ("ballots" / urlize hash) in
     match ballot with
     | Some [x] -> return_some x
     | _ -> return_none

let load_ballots uuid =
  let ballots_dir = !Web_config.spool_dir / raw_string_of_uuid uuid / "ballots" in
  let* b = Lwt_unix.file_exists ballots_dir in
  if b then (
    let ballots = Lwt_unix.files_of_directory ballots_dir in
    let* ballots = Lwt_stream.to_list ballots in
    Lwt_list.filter_map_s (fun x ->
        let* file = read_file (ballots_dir / x) in
        match file with
        | Some [x] -> return_some x
        | _ -> return_none
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
  let index = Yojson.Safe.to_string (`Assoc index) in
  let* () = write_file ~uuid "ballots_index.json" [index] in
  write_file ~uuid "ballots.jsons" ballots

let add_ballot election ballot =
  let module W = (val election : Site_common_sig.ELECTION_LWT) in
  let uuid = W.election.e_uuid in
  let hash = sha256_b64 ballot in
  let ballots_dir = !Web_config.spool_dir / raw_string_of_uuid uuid / "ballots" in
  let* () = Lwt.catch (fun () -> Lwt_unix.mkdir ballots_dir 0o755) (fun _ -> return_unit) in
  let* () = write_file (ballots_dir / urlize hash) [ballot] in
  let* () = dump_ballots election in
  return hash

let remove_ballot election hash =
  let module W = (val election : Site_common_sig.ELECTION_LWT) in
  let uuid = W.election.e_uuid in
  let ballots_dir = !Web_config.spool_dir / raw_string_of_uuid uuid / "ballots" in
  Lwt.catch (fun () -> Lwt_unix.unlink (ballots_dir / urlize hash)) (fun _ -> return_unit)

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
  let tally = W.E.process_ballots (Array.of_list ballots) in
  let tally = string_of_encrypted_tally W.G.write tally in
  let* () = write_file ~uuid (string_of_election_file ESETally) [tally] in
  return tally

let get_shuffle_token uuid =
  let* file = read_file ~uuid "shuffle_token.json" in
  match file with
  | Some [x] -> return_some (shuffle_token_of_string x)
  | _ -> return_none

let gen_shuffle_token uuid tk_trustee tk_name =
  let* tk_token = generate_token () in
  let t = {tk_trustee; tk_token; tk_name} in
  let* () = write_file ~uuid "shuffle_token.json" [string_of_shuffle_token t] in
  return t

let clear_shuffle_token uuid =
  let f = !Web_config.spool_dir / raw_string_of_uuid uuid / "shuffle_token.json" in
  Lwt.catch (fun () -> Lwt_unix.unlink f) (fun _ -> return_unit)

let get_nh_ciphertexts election =
  let module W = (val election : Site_common_sig.ELECTION_LWT) in
  let uuid = W.election.e_uuid in
  let* current =
    let* file = read_file ~uuid "shuffles.jsons" in
    match file with
    | None -> return []
    | Some x -> return x
  in
  match List.rev current with
  | [] ->
     let* tally =
       let* file = read_file ~uuid (string_of_election_file ESETally) in
       match file with
       | Some [x] -> return (encrypted_tally_of_string W.G.read x)
       | _ -> Lwt.fail (Failure "get_nh_ciphertexts: encrypted tally not found or invalid")
     in
     return (string_of_nh_ciphertexts W.G.write (W.E.extract_nh_ciphertexts tally))
  | x :: _ ->
     let s = shuffle_of_string W.G.read x in
     return (string_of_nh_ciphertexts W.G.write s.shuffle_ciphertexts)

let get_shuffles uuid =
  let* election = get_raw_election uuid in
  match election with
  | None -> return_none
  | Some _ ->
     let* file = read_file ~uuid "shuffles.jsons" in
     match file with
     | None -> return_none
     | Some x ->
        let rec loop accu = function
          | s :: rest -> loop (s :: accu) rest
          | [] -> return_some (List.rev accu)
        in
        loop [] x

let get_shuffle_hashes uuid =
  let* file = read_file ~uuid "shuffle_hashes.jsons" in
  match file with
  | None -> return_none
  | Some x ->
     let rec loop accu = function
       | s :: rest -> loop (shuffle_hash_of_string s :: accu) rest
       | [] -> return_some (List.rev accu)
     in
     loop [] x

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
  let new_ = current @ [sh] in
  write_file ~uuid "shuffle_hashes.jsons" (List.map string_of_shuffle_hash new_)

let compute_encrypted_tally_after_shuffling election =
  let module W = (val election : Site_common_sig.ELECTION_LWT) in
  let uuid = W.election.e_uuid in
  let* file = read_file ~uuid (string_of_election_file ESETally) in
  match file with
  | Some [x] ->
     let tally = encrypted_tally_of_string W.G.read x in
     let* nh = get_nh_ciphertexts election in
     let nh = nh_ciphertexts_of_string W.G.read nh in
     let tally = W.E.merge_nh_ciphertexts nh tally in
     let tally = string_of_encrypted_tally W.G.write tally in
     let* () = write_file ~uuid (string_of_election_file ESETally) [tally] in
     return_some tally
  | _ -> return_none

let append_to_shuffles election shuffle =
  let module W = (val election : Site_common_sig.ELECTION_LWT) in
  let uuid = W.election.e_uuid in
  let shuffle = shuffle_of_string W.G.read shuffle in
  Web_election_mutex.with_lock uuid (fun () ->
      let* last_ciphertext = get_nh_ciphertexts election in
      let last_ciphertext = nh_ciphertexts_of_string W.G.read last_ciphertext in
      if W.E.check_shuffle last_ciphertext shuffle then (
        let* current =
          let* file = read_file ~uuid "shuffles.jsons" in
          match file with
          | None -> return []
          | Some x -> return x
        in
        let shuffle_ = string_of_shuffle W.G.write shuffle in
        let new_ = current @ [shuffle_] in
        let* () = write_file ~uuid "shuffles.jsons" new_ in
        return_some (sha256_b64 shuffle_)
      ) else return_none
    )

module ExtendedRecordsCacheTypes = struct
  type key = uuid
  type value = (datetime * string) StringMap.t
end

module ExtendedRecordsCache = Ocsigen_cache.Make (ExtendedRecordsCacheTypes)

let raw_get_extended_records uuid =
  let* file = read_file ~uuid "extended_records.jsons" in
  match file with
  | Some xs ->
     let xs = List.map extended_record_of_string xs in
     return (
         List.fold_left (fun accu r ->
             StringMap.add r.r_username (r.r_date, r.r_credential) accu
           ) StringMap.empty xs
       )
  | None -> return StringMap.empty

let dump_extended_records uuid rs =
  let rs = StringMap.bindings rs in
  let extended_records =
    List.map (fun (r_username, (r_date, r_credential)) ->
        string_of_extended_record {r_username; r_date; r_credential}
      ) rs
  in
  let records =
    List.map (fun (u, (d, _)) ->
        Printf.sprintf "%s %S" (string_of_datetime d) u
      ) rs
  in
  let* () = write_file ~uuid "extended_records.jsons" extended_records in
  write_file ~uuid (string_of_election_file ESRecords) records

let extended_records_cache =
  new ExtendedRecordsCache.cache raw_get_extended_records ~timer:3600. 10

let find_extended_record uuid username =
  let* rs = extended_records_cache#find uuid in
  return (StringMap.find_opt username rs)

let add_extended_record uuid username r =
  let* rs = extended_records_cache#find uuid in
  let rs = StringMap.add username r rs in
  extended_records_cache#add uuid rs;
  dump_extended_records uuid rs

let has_voted uuid user =
  let* rs = extended_records_cache#find uuid in
  return @@ StringMap.mem (string_of_user user) rs

module CredMappingsCacheTypes = struct
  type key = uuid
  type value = string option StringMap.t
end

module CredMappingsCache = Ocsigen_cache.Make (CredMappingsCacheTypes)

let raw_get_credential_mappings uuid =
  let* file = read_file ~uuid "credential_mappings.jsons" in
  match file with
  | Some xs ->
     let xs = List.map credential_mapping_of_string xs in
     return (
         List.fold_left (fun accu x ->
             StringMap.add x.c_credential x.c_ballot accu
           ) StringMap.empty xs
       )
  | None -> return StringMap.empty

let dump_credential_mappings uuid xs =
  let xs = StringMap.bindings xs in
  let mappings =
    List.map (fun (c_credential, c_ballot) ->
        string_of_credential_mapping {c_credential; c_ballot}
      ) xs
  in
  write_file ~uuid "credential_mappings.jsons" mappings

let credential_mappings_cache =
  new CredMappingsCache.cache raw_get_credential_mappings ~timer:3600. 10

let init_credential_mapping uuid xs =
  let xs =
    List.fold_left
      (fun accu x ->
        let x, _ = extract_weight x in
        if StringMap.mem x accu then
          failwith "trying to add duplicate credential"
        else
          StringMap.add x None accu
      ) StringMap.empty xs
  in
  credential_mappings_cache#add uuid xs;
  dump_credential_mappings uuid xs

let find_credential_mapping uuid cred =
  let* xs = credential_mappings_cache#find uuid in
  return @@ StringMap.find_opt cred xs

let add_credential_mapping uuid cred mapping =
  let* xs = credential_mappings_cache#find uuid in
  let xs = StringMap.add cred mapping xs in
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
        match x with
        | None -> return_none
        | Some (_, old_credential) -> return_some old_credential
      let get_credential_record credential =
        let* x = find_credential_mapping uuid credential in
        match x with
        | None -> return_none
        | Some cr_ballot ->
           let* cr_weight = get_credential_weight uuid credential in
           return_some {cr_ballot; cr_weight}
    end
  in
  let module B = W.E.CastBallot (X) in
  let* x = B.cast ~user ~weight rawballot in
  match x with
  | Error _ as x -> return x
  | Ok (credential, _, old) ->
     let* hash, revote =
       match old with
       | None ->
          let* h = add_ballot election rawballot in
          return (h, false)
       | Some hash ->
          let* h = replace_ballot election ~hash ~rawballot in
          return (h, true)
     in
     let* () = add_credential_mapping uuid credential (Some hash) in
     let* () = add_extended_record uuid user (date, credential) in
     return (Ok (hash, revote))

let cast_ballot election ~rawballot ~user ~weight date =
  let module W = (val election : Site_common_sig.ELECTION_LWT) in
  let uuid = W.election.e_uuid in
  Web_election_mutex.with_lock uuid
    (fun () -> do_cast_ballot election ~rawballot ~user ~weight date)

let get_raw_election_result uuid =
  let* file = read_file ~uuid "result.json" in
  match file with
  | Some [x] -> return_some x
  | _ -> return_none

let compute_audit_cache uuid =
  let* election = get_raw_election uuid in
  match election with
  | None ->
     Printf.ksprintf failwith
       "compute_cache: %s does not exist" (raw_string_of_uuid uuid)
  | Some election ->
     let* voters =
       let* file = read_file ~uuid "voters.txt" in
       match file with
       | Some x -> return x
       | None -> failwith "voters.txt is missing"
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
     let cache_voters_hash = sha256_b64 (String.concat "\n" voters ^ "\n") in
     let* result_or_shuffles =
       let* raw_election_result = get_raw_election_result uuid in
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
       let* file = read_file ~uuid "public_creds.txt" in
       match file with
       | Some x -> return x
       | None -> failwith "public_creds.txt is missing"
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
  let* cache =
    let* file = read_file ~uuid "audit_cache.json" in
    match file with
    | Some [x] ->
       let cache = try Some (audit_cache_of_string x) with _ -> None in
       return cache
    | _ -> return_none
  in
  match cache with
  | Some x when x.cache_checksums.ec_trustees_threshold <> None -> return x
  | _ ->
     let* cache = compute_audit_cache uuid in
     let* () = write_file ~uuid "audit_cache.json" [string_of_audit_cache cache] in
     return cache

let remove_audit_cache uuid =
  Lwt.catch
    (fun () ->
      Lwt_unix.unlink (!Web_config.spool_dir / raw_string_of_uuid uuid / "audit_cache.json")
    )
    (fun _ -> return_unit)
