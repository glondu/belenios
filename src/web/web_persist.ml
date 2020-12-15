(**************************************************************************)
(*                                BELENIOS                                *)
(*                                                                        *)
(*  Copyright © 2012-2020 Inria                                           *)
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
open Belenios_platform
open Belenios
open Platform
open Signatures
open Serializable_builtin_t
open Serializable_j
open Common
open Web_serializable_builtin_t
open Web_serializable_j
open Web_common

let ( / ) = Filename.concat

let get_draft_election uuid =
  match%lwt read_file ~uuid "draft.json" with
  | Some [x] -> return_some (draft_election_of_string x)
  | _ -> return_none

let set_draft_election uuid se =
  write_file ~uuid "draft.json" [string_of_draft_election se]

let get_election_result uuid =
  match%lwt read_file ~uuid "result.json" with
  | Some [x] -> return_some (election_result_of_string Yojson.Safe.read_json x)
  | _ -> return_none

let set_election_result_hidden uuid hidden =
  match hidden with
  | None ->
     (try%lwt Lwt_unix.unlink (!Web_config.spool_dir / raw_string_of_uuid uuid / "hide_result") with
      | _ -> return_unit
     )
  | Some d -> write_file ~uuid "hide_result" [string_of_datetime d]

let get_election_result_hidden uuid =
  match%lwt read_file ~uuid "hide_result" with
  | Some [x] ->
     let t = datetime_of_string x in
     if datetime_compare (now ()) t < 0 then
       return_some t
     else
       let%lwt () = set_election_result_hidden uuid None in
       return_none
  | _ -> return_none

let get_election_dates uuid =
  match%lwt read_file ~uuid "dates.json" with
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
  write_file ~uuid "dates.json" [string_of_election_dates dates]

let set_election_state uuid s =
  match s with
  | `Archived ->
     (try%lwt Lwt_unix.unlink (!Web_config.spool_dir / raw_string_of_uuid uuid / "state.json") with
      | _ -> return_unit
     )
  | _ -> write_file ~uuid "state.json" [string_of_election_state s]

let get_election_state uuid =
  match%lwt read_file ~uuid "state.json" with
  | Some [x] ->
     let state = election_state_of_string x and now = now () in
     let%lwt dates = get_election_dates uuid in
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
     let%lwt () =
       if new_state <> state then set_election_state uuid new_state
       else return_unit
     in
     return new_state
  | _ -> return `Archived

let get_partial_decryptions uuid =
  match%lwt read_file ~uuid "partial_decryptions.json" with
  | Some [x] -> return @@ partial_decryptions_of_string x
  | _ -> return []

let set_partial_decryptions uuid pds =
  write_file ~uuid "partial_decryptions.json"
    [string_of_partial_decryptions pds]

let get_decryption_tokens uuid =
  match%lwt read_file ~uuid "decryption_tokens.json" with
  | Some [x] -> return_some (decryption_tokens_of_string x)
  | _ -> return_none

let set_decryption_tokens uuid pds =
  write_file ~uuid "decryption_tokens.json"
    [string_of_decryption_tokens pds]

let get_raw_election uuid =
  match%lwt read_file ~uuid "election.json" with
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
}

let return_empty_metadata = return empty_metadata

let get_election_metadata uuid =
  match%lwt read_file ~uuid "metadata.json" with
  | Some [x] -> return (metadata_of_string x)
  | _ -> return_empty_metadata

type election_kind =
  [ `Draft
  | `Validated
  | `Tallied
  | `Archived
  ]

let get_elections_by_owner user =
  Lwt_unix.files_of_directory !Web_config.spool_dir |>
    Lwt_stream.to_list >>=
    Lwt_list.filter_map_s
      (fun x ->
        if x = "." || x = ".." then
          return_none
        else (
          try
            let uuid = uuid_of_raw_string x in
            match%lwt get_draft_election uuid with
            | None ->
               (
                 let%lwt metadata = get_election_metadata uuid in
                 match metadata.e_owner with
                 | Some o when o = user ->
                    (
                      match%lwt get_raw_election uuid with
                      | None -> return_none
                      | Some election ->
                         let election = Election.of_string election in
                         let%lwt dates = get_election_dates uuid in
                         let%lwt kind, date =
                           match%lwt get_election_state uuid with
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
                         return_some (kind, uuid, date, election.e_params.e_name)
                    )
                 | _ -> return_none
               )
            | Some se ->
               if se.se_owner = user then
                 let date = Option.get se.se_creation_date default_creation_date in
                 return_some (`Draft, uuid, date, se.se_questions.t_name)
               else return_none
          with _ -> return_none
        )
      )

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
  match%lwt read_file ~uuid "private_key.json" with
  | Some [x] -> return_some (number_of_string x)
  | _ -> return_none

let get_private_keys uuid =
  read_file ~uuid "private_keys.jsons"

let get_trustees uuid =
  match%lwt read_file ~uuid "trustees.json" with
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
  match%lwt read_file ~uuid "threshold.json" with
  | Some [x] -> return_some x
  | _ -> return_none

let get_trustees_legacy uuid =
  match%lwt get_threshold uuid with
  | Some x ->
     x
     |> threshold_parameters_of_string Yojson.Safe.read_json
     |> (fun x -> [`Pedersen x])
     |> string_of_trustees Yojson.Safe.write_json
     |> return_some
  | None ->
     match%lwt get_public_keys uuid with
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
            match%lwt get_trustees_legacy uuid with
            | None -> return_unit
            | Some trustees ->
               let%lwt () = write_file ~uuid "trustees.json" [trustees] in
               let%lwt () = cleanup_file (!Web_config.spool_dir / x / "threshold.json") in
               cleanup_file (!Web_config.spool_dir / x / "public_keys.jsons")
        )

module StringMap = Map.Make (String)

module BallotsCacheTypes = struct
  type key = uuid
  type value = string StringMap.t
end

module BallotsCache = Ocsigen_cache.Make (BallotsCacheTypes)

let raw_get_ballots_archived uuid =
  match%lwt read_file ~uuid "ballots.jsons" with
  | Some bs ->
     return (
         List.fold_left (fun accu b ->
             let hash = sha256_b64 b in
             StringMap.add hash b accu
           ) StringMap.empty bs
       )
  | None -> return StringMap.empty

let archived_ballots_cache =
  new BallotsCache.cache raw_get_ballots_archived ~timer:3600. 10

let get_ballot_hashes uuid =
  match%lwt get_election_state uuid with
  | `Archived ->
     let%lwt ballots = archived_ballots_cache#find uuid in
     StringMap.bindings ballots |> List.map fst |> return
  | _ ->
     let uuid_s = raw_string_of_uuid uuid in
     match%lwt Lwt_unix.files_of_directory (!Web_config.spool_dir / uuid_s / "ballots") |> Lwt_stream.to_list with
     | ballots ->
        let ballots = List.filter (fun x -> x <> "." && x <> "..") ballots in
        return (List.rev_map unurlize ballots)
     | exception Unix.Unix_error(Unix.ENOENT, "opendir", _) ->
        return []

let get_ballot_by_hash uuid hash =
  match%lwt get_election_state uuid with
  | `Archived ->
     let%lwt ballots = archived_ballots_cache#find uuid in
     return (StringMap.find_opt hash ballots)
  | _ ->
     let%lwt ballot = read_file ~uuid ("ballots" / urlize hash) in
     match ballot with
     | Some [x] -> return_some x
     | _ -> return_none

let load_ballots uuid =
  let ballots_dir = !Web_config.spool_dir / raw_string_of_uuid uuid / "ballots" in
  if%lwt Lwt_unix.file_exists ballots_dir then (
    let ballots = Lwt_unix.files_of_directory ballots_dir in
    let%lwt ballots = Lwt_stream.to_list ballots in
    Lwt_list.filter_map_s (fun x ->
        match%lwt read_file (ballots_dir / x) with
        | Some [x] -> return_some x
        | _ -> return_none
      ) ballots
  ) else return []

let dump_ballots uuid =
  let%lwt ballots = load_ballots uuid in
  write_file ~uuid "ballots.jsons" ballots

let add_ballot uuid ballot =
  let hash = sha256_b64 ballot in
  let ballots_dir = !Web_config.spool_dir / raw_string_of_uuid uuid / "ballots" in
  let%lwt () = try%lwt Lwt_unix.mkdir ballots_dir 0o755 with _ -> return_unit in
  let%lwt () = write_file (ballots_dir / urlize hash) [ballot] in
  let%lwt () = dump_ballots uuid in
  return hash

let remove_ballot uuid hash =
  let ballots_dir = !Web_config.spool_dir / raw_string_of_uuid uuid / "ballots" in
  try%lwt Lwt_unix.unlink (ballots_dir / urlize hash) with _ -> return_unit

let replace_ballot uuid ~hash ~rawballot =
  let%lwt () = remove_ballot uuid hash in
  add_ballot uuid rawballot

let compute_encrypted_tally uuid =
  let%lwt election = get_raw_election uuid in
  match election with
  | None -> return_none
  | Some election ->
     let election = Election.of_string election in
     let module W = (val Election.get_group election) in
     let module E = Election.Make (W) (LwtRandom) in
     let%lwt ballots = load_ballots uuid in
     let ballots = Array.map (ballot_of_string E.G.read) (Array.of_list ballots) in
     let ballots = Array.map (fun x -> 1, x) ballots in
     let tally = E.process_ballots ballots in
     let tally = string_of_encrypted_tally E.G.write tally in
     let%lwt () = write_file ~uuid (string_of_election_file ESETally) [tally] in
     return_some tally

let get_shuffle_token uuid =
  match%lwt read_file ~uuid "shuffle_token.json" with
  | Some [x] -> return_some (shuffle_token_of_string x)
  | _ -> return_none

let gen_shuffle_token uuid tk_trustee tk_name =
  let%lwt tk_token = generate_token () in
  let t = {tk_trustee; tk_token; tk_name} in
  let%lwt () = write_file ~uuid "shuffle_token.json" [string_of_shuffle_token t] in
  return t

let clear_shuffle_token uuid =
  let f = !Web_config.spool_dir / raw_string_of_uuid uuid / "shuffle_token.json" in
  try%lwt Lwt_unix.unlink f with _ -> return_unit

let get_nh_ciphertexts uuid =
  match%lwt get_raw_election uuid with
  | None -> Lwt.fail (Failure "get_nh_ciphertexts: election not found")
  | Some election ->
     let election = Election.of_string election in
     let module W = (val Election.get_group election) in
     let module E = Election.Make (W) (LwtRandom) in
     let%lwt current =
       match%lwt read_file ~uuid "shuffles.jsons" with
       | None -> return []
       | Some x -> return x
     in
     match List.rev current with
     | [] ->
        let%lwt tally =
          match%lwt read_file ~uuid (string_of_election_file ESETally) with
          | Some [x] -> return (encrypted_tally_of_string E.G.read x)
          | _ -> Lwt.fail (Failure "get_nh_ciphertexts: encrypted tally not found or invalid")
        in
        return (string_of_nh_ciphertexts E.G.write (E.extract_nh_ciphertexts tally))
     | x :: _ ->
        let s = shuffle_of_string E.G.read x in
        return (string_of_nh_ciphertexts E.G.write s.shuffle_ciphertexts)

let get_shuffles uuid =
  match%lwt get_raw_election uuid with
  | None -> return_none
  | Some _ ->
     match%lwt read_file ~uuid "shuffles.jsons" with
     | None -> return_none
     | Some x ->
        let rec loop accu = function
          | s :: rest -> loop (s :: accu) rest
          | [] -> return_some (List.rev accu)
        in
        loop [] x

let get_shuffle_hashes uuid =
  match%lwt read_file ~uuid "shuffle_hashes.jsons" with
  | None -> return_none
  | Some x ->
     let rec loop accu = function
       | s :: rest -> loop (shuffle_hash_of_string s :: accu) rest
       | [] -> return_some (List.rev accu)
     in
     loop [] x

let add_shuffle_hash uuid sh =
  let%lwt current =
    match%lwt get_shuffle_hashes uuid with
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

let compute_encrypted_tally_after_shuffling uuid =
  let%lwt election = get_raw_election uuid in
  match election with
  | None -> return_none
  | Some election ->
     let election = Election.of_string election in
     let module W = (val Election.get_group election) in
     let module E = Election.Make (W) (LwtRandom) in
     match%lwt read_file ~uuid (string_of_election_file ESETally) with
     | Some [x] ->
        let tally = encrypted_tally_of_string E.G.read x in
        let%lwt nh = get_nh_ciphertexts uuid in
        let nh = nh_ciphertexts_of_string E.G.read nh in
        let tally = E.merge_nh_ciphertexts nh tally in
        let tally = string_of_encrypted_tally E.G.write tally in
        let%lwt () = write_file ~uuid (string_of_election_file ESETally) [tally] in
        return_some tally
     | _ -> return_none

let append_to_shuffles uuid shuffle =
  match%lwt get_raw_election uuid with
  | None -> Lwt.fail (Failure "append_to_shuffles: election not found")
  | Some election ->
     let election = Election.of_string election in
     let module W = (val Election.get_group election) in
     let module E = Election.Make (W) (LwtRandom) in
     let shuffle = shuffle_of_string E.G.read shuffle in
     Web_election_mutex.with_lock uuid (fun () ->
         let%lwt last_ciphertext = get_nh_ciphertexts uuid in
         let last_ciphertext = nh_ciphertexts_of_string E.G.read last_ciphertext in
         if E.check_shuffle last_ciphertext shuffle then (
           let%lwt current =
             match%lwt read_file ~uuid "shuffles.jsons" with
             | None -> return []
             | Some x -> return x
           in
           let shuffle_ = string_of_shuffle E.G.write shuffle in
           let new_ = current @ [shuffle_] in
           let%lwt () = write_file ~uuid "shuffles.jsons" new_ in
           return_some (sha256_b64 shuffle_)
         ) else return_none
       )

module ExtendedRecordsCacheTypes = struct
  type key = uuid
  type value = (datetime * string) StringMap.t
end

module ExtendedRecordsCache = Ocsigen_cache.Make (ExtendedRecordsCacheTypes)

let raw_get_extended_records uuid =
  match%lwt read_file ~uuid "extended_records.jsons" with
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
  let%lwt () = write_file ~uuid "extended_records.jsons" extended_records in
  write_file ~uuid (string_of_election_file ESRecords) records

let extended_records_cache =
  new ExtendedRecordsCache.cache raw_get_extended_records ~timer:3600. 10

let find_extended_record uuid username =
  let%lwt rs = extended_records_cache#find uuid in
  return (StringMap.find_opt username rs)

let add_extended_record uuid username r =
  let%lwt rs = extended_records_cache#find uuid in
  let rs = StringMap.add username r rs in
  extended_records_cache#add uuid rs;
  dump_extended_records uuid rs

let has_voted uuid user =
  let%lwt rs = extended_records_cache#find uuid in
  return @@ StringMap.mem (string_of_user user) rs

module CredMappingsCacheTypes = struct
  type key = uuid
  type value = string option StringMap.t
end

module CredMappingsCache = Ocsigen_cache.Make (CredMappingsCacheTypes)

module CredWeightsCacheTypes = struct
  type key = uuid
  type value = int StringMap.t
end

module CredWeightsCache = Ocsigen_cache.Make (CredWeightsCacheTypes)

let raw_get_credential_weights uuid =
  match%lwt read_file ~uuid "public_creds.txt" with
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
  try%lwt
    let%lwt xs = credential_weights_cache#find uuid in
    return @@ StringMap.find cred xs
  with _ ->
    Lwt.fail
      (Failure
         (Printf.sprintf
            "could not find weight of %s/%s"
            (raw_string_of_uuid uuid) cred
         )
      )

let raw_get_credential_mappings uuid =
  match%lwt read_file ~uuid "credential_mappings.jsons" with
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
  let%lwt xs = credential_mappings_cache#find uuid in
  return @@ StringMap.find_opt cred xs

let add_credential_mapping uuid cred mapping =
  let%lwt xs = credential_mappings_cache#find uuid in
  let xs = StringMap.add cred mapping xs in
  credential_mappings_cache#add uuid xs;
  dump_credential_mappings uuid xs

let do_cast_ballot election ~rawballot ~user ~weight date =
  let module E = (val election : ELECTION) in
  let uuid = E.election.e_params.e_uuid in
  match
    try
      if String.contains rawballot '\n' then invalid_arg "multiline ballot";
      let ballot = ballot_of_string E.G.read rawballot in
      if string_of_ballot E.G.write ballot <> rawballot then
        invalid_arg "ballot not in canonical form";
      Ok ballot
    with e -> Error (ECastSerialization e)
  with
  | Error _ as x -> return x
  | Ok ballot ->
     match ballot.signature with
     | None -> return (Error ECastMissingCredential)
     | Some s ->
        let credential = E.G.to_string s.s_public_key in
        match%lwt find_credential_mapping uuid credential with
        | None -> return (Error ECastInvalidCredential)
        | Some old_cred ->
           let%lwt cweight = get_credential_weight uuid credential in
           if cweight = weight then (
             let%lwt old_record = find_extended_record uuid user in
             match old_cred, old_record with
             | None, None ->
                (* first vote *)
                if%lwt Lwt_preemptive.detach E.check_ballot ballot then (
                  let%lwt hash = add_ballot uuid rawballot in
                  let%lwt () = add_credential_mapping uuid credential (Some hash) in
                  let%lwt () = add_extended_record uuid user (date, credential) in
                  return (Ok (hash, false))
                ) else return (Error ECastProofCheck)
             | Some hash, Some (_, old_credential) ->
                (* revote *)
                if credential = old_credential then (
                  if%lwt Lwt_preemptive.detach E.check_ballot ballot then (
                    let%lwt hash = replace_ballot uuid ~hash ~rawballot in
                    let%lwt () = add_credential_mapping uuid credential (Some hash) in
                    let%lwt () = add_extended_record uuid user (date, credential) in
                    return (Ok (hash, true))
                  ) else return (Error ECastProofCheck)
                ) else return (Error ECastWrongCredential)
             | None, Some _ -> return (Error ECastRevoteNotAllowed)
             | Some _, None -> return (Error ECastReusedCredential)
           ) else return (Error ECastBadWeight)

let cast_ballot uuid ~rawballot ~user ~weight date =
  match%lwt get_raw_election uuid with
  | None -> Lwt.fail Not_found
  | Some raw_election ->
     let election = Election.of_string raw_election in
     let module W = (val Election.get_group election) in
     let module E = Election.Make (W) (LwtRandom) in
     Web_election_mutex.with_lock uuid
       (fun () -> do_cast_ballot (module E) ~rawballot ~user ~weight date)

let get_raw_election_result uuid =
  match%lwt read_file ~uuid "result.json" with
  | Some [x] -> return_some x
  | _ -> return_none

let compute_audit_cache uuid =
  match%lwt get_raw_election uuid with
  | None ->
     Printf.ksprintf failwith
       "compute_cache: %s does not exist" (raw_string_of_uuid uuid)
  | Some election ->
     let%lwt voters =
       match%lwt read_file ~uuid "voters.txt" with
       | Some x -> return x
       | None -> failwith "voters.txt is missing"
     in
     let cache_num_voters = List.length voters in
     let cache_voters_hash = sha256_b64 (String.concat "\n" voters ^ "\n") in
     let%lwt result_or_shuffles =
       match%lwt get_raw_election_result uuid with
       | Some r -> return (`Result r)
       | None ->
          match%lwt get_shuffles uuid with
          | None -> return `Nothing
          | Some shuffles ->
             match%lwt get_shuffle_hashes uuid with
             | None -> return `Nothing
             | Some sh ->
                let sh = List.filter (fun x -> x.sh_hash <> "") sh in
                let shufflers = List.map (fun x -> x.sh_name) sh in
                return (`Shuffles (shuffles, Some shufflers))
     in
     let%lwt trustees = get_trustees uuid in
     let%lwt credentials =
       match%lwt read_file ~uuid "public_creds.txt" with
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
         cache_voters_hash;
         cache_checksums;
         cache_threshold = None;
       }

let get_audit_cache uuid =
  let%lwt cache =
    match%lwt read_file ~uuid "audit_cache.json" with
    | Some [x] ->
       let cache = try Some (audit_cache_of_string x) with _ -> None in
       return cache
    | _ -> return_none
  in
  match cache with
  | Some x when x.cache_checksums.ec_trustees_threshold <> None -> return x
  | _ ->
     let%lwt cache = compute_audit_cache uuid in
     let%lwt () = write_file ~uuid "audit_cache.json" [string_of_audit_cache cache] in
     return cache

let remove_audit_cache uuid =
  try%lwt Lwt_unix.unlink (!Web_config.spool_dir / raw_string_of_uuid uuid / "audit_cache.json") with
  | _ -> return_unit
