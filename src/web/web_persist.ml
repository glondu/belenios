(**************************************************************************)
(*                                BELENIOS                                *)
(*                                                                        *)
(*  Copyright © 2012-2018 Inria                                           *)
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
open Platform
open Serializable_builtin_t
open Serializable_j
open Common
open Web_serializable_j
open Web_common

let ( / ) = Filename.concat

let get_election_result uuid =
  match%lwt read_file ~uuid "result.json" with
  | Some [x] -> return (Some (result_of_string Yojson.Safe.read_json x))
  | _ -> return_none

type election_state =
  [ `Open
  | `Closed
  | `EncryptedTally of int * int * string
  | `Tallied of plaintext
  | `Archived
  ]

let election_states = Ocsipersist.open_table "election_states"

let get_election_state x =
  try%lwt Ocsipersist.find election_states (raw_string_of_uuid x)
  with Not_found -> return `Archived

let set_election_state x s =
  Ocsipersist.add election_states (raw_string_of_uuid x) s

type election_date =
  [ `Creation
  | `Validation
  | `Tally
  | `Archive
  | `LastMail
  ]

let get_election_dates uuid =
  match%lwt read_file ~uuid "dates.json" with
  | Some [x] -> return (election_dates_of_string x)
  | _ -> return {
             e_creation = None;
             e_finalization = None;
             e_tally = None;
             e_archive = None;
             e_last_mail = None;
           }

let set_election_date kind uuid d =
  let%lwt dates = get_election_dates uuid in
  let dates = match kind with
    | `Creation -> { dates with e_creation = Some d }
    | `Validation -> { dates with e_finalization = Some d }
    | `Tally -> { dates with e_tally = Some d }
    | `Archive -> { dates with e_archive = Some d }
    | `LastMail -> { dates with e_last_mail = Some d }
  in
  let dates = string_of_election_dates dates in
  write_file ~uuid "dates.json" [dates]

let get_election_date kind uuid =
  let%lwt dates = get_election_dates uuid in
  match kind with
  | `Creation -> return dates.e_creation
  | `Validation -> return dates.e_finalization
  | `Tally -> return dates.e_tally
  | `Archive -> return dates.e_archive
  | `LastMail -> return dates.e_last_mail

let election_pds = Ocsipersist.open_table "election_pds"

let get_partial_decryptions x =
  try%lwt Ocsipersist.find election_pds (raw_string_of_uuid x)
  with Not_found -> return []

let set_partial_decryptions x pds =
  Ocsipersist.add election_pds (raw_string_of_uuid x) pds

let get_raw_election uuid =
  match%lwt read_file ~uuid "election.json" with
  | Some [x] -> return (Some x)
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

let get_auth_config uuid =
  let%lwt metadata = get_election_metadata uuid in
  match metadata.e_auth_config with
  | None -> return []
  | Some x -> return (List.map compile_auth_config x)

let get_elections_by_owner user =
  Lwt_unix.files_of_directory !spool_dir |>
    Lwt_stream.filter_map_s
      (fun x ->
        if x = "." || x = ".." then
          return None
        else (
          try
            let uuid = uuid_of_raw_string x in
            let%lwt metadata = get_election_metadata uuid in
            match metadata.e_owner with
            | Some o when o = user -> return (Some uuid)
            | _ -> return None
          with _ -> return None
        )
      ) |>
    Lwt_stream.to_list

let get_voters uuid =
  read_file ~uuid "voters.txt"

let get_passwords uuid =
  let csv =
    try Some (Csv.load (!spool_dir / raw_string_of_uuid uuid / "passwords.csv"))
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
     return @@ Some res

let get_public_keys uuid =
  read_file ~uuid "public_keys.jsons"

let get_private_key uuid =
  match%lwt read_file ~uuid "private_key.json" with
  | Some [x] -> return (Some (number_of_string x))
  | _ -> return_none

let get_private_keys uuid =
  read_file ~uuid "private_keys.jsons"

let get_threshold uuid =
  match%lwt read_file ~uuid "threshold.json" with
  | Some [x] -> return (Some x)
  | _ -> return_none

module Ballots = Map.Make (String)

module BallotsCacheTypes = struct
  type key = uuid
  type value = string Ballots.t
end

module BallotsCache = Ocsigen_cache.Make (BallotsCacheTypes)

let raw_get_ballots_archived uuid =
  match%lwt read_file ~uuid "ballots.jsons" with
  | Some bs ->
     return (
         List.fold_left (fun accu b ->
             let hash = sha256_b64 b in
             Ballots.add hash b accu
           ) Ballots.empty bs
       )
  | None -> return Ballots.empty

let archived_ballots_cache =
  new BallotsCache.cache raw_get_ballots_archived ~timer:3600. 10

let get_ballot_hashes uuid =
  match%lwt get_election_state uuid with
  | `Archived ->
     let%lwt ballots = archived_ballots_cache#find uuid in
     Ballots.bindings ballots |> List.map fst |> return
  | _ ->
     let table = Ocsipersist.open_table ("ballots_" ^ underscorize uuid) in
     Ocsipersist.fold_step (fun hash _ accu ->
       return (hash :: accu)
     ) table [] >>= (fun x -> return @@ List.rev x)

let get_ballot_by_hash uuid hash =
  match%lwt get_election_state uuid with
  | `Archived ->
     let%lwt ballots = archived_ballots_cache#find uuid in
     (try Some (Ballots.find hash ballots) with Not_found -> None) |> return
  | _ ->
     let table = Ocsipersist.open_table ("ballots_" ^ underscorize uuid) in
     try%lwt Ocsipersist.find table hash >>= (fun x -> return @@ Some x)
     with Not_found -> return_none

let has_voted uuid user =
  let uuid_u = underscorize uuid in
  let records_table = Ocsipersist.open_table ("records_" ^ uuid_u) in
  try%lwt
    let%lwt _ = Ocsipersist.find records_table (string_of_user user) in
    return true
  with Not_found ->
    return false
