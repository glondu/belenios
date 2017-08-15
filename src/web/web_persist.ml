(**************************************************************************)
(*                                BELENIOS                                *)
(*                                                                        *)
(*  Copyright © 2012-2016 Inria                                           *)
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
  try%lwt
    Lwt_io.chars_of_file (!spool_dir / raw_string_of_uuid uuid / "result.json") |>
    Lwt_stream.to_string >>= fun x ->
    return @@ Some (result_of_string (Yojson.Safe.from_lexbuf ~stream:true) x)
  with _ -> return_none

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

let past = datetime_of_string "\"2015-10-01 00:00:00.000000\""

let set_election_date uuid d =
  let dates = { e_finalization = d } in
  Lwt_io.(with_file Output (!spool_dir / raw_string_of_uuid uuid / "dates.json") (fun oc ->
    write_line oc (string_of_election_dates dates)
  ))

let get_election_date uuid =
    try%lwt
      Lwt_io.chars_of_file (!spool_dir / raw_string_of_uuid uuid / "dates.json") |>
      Lwt_stream.to_string >>= fun x ->
      let dates = election_dates_of_string x in
      return dates.e_finalization
    with _ ->
      return past

let election_pds = Ocsipersist.open_table "election_pds"

let get_partial_decryptions x =
  try%lwt Ocsipersist.find election_pds (raw_string_of_uuid x)
  with Not_found -> return []

let set_partial_decryptions x pds =
  Ocsipersist.add election_pds (raw_string_of_uuid x) pds

let auth_configs = Ocsipersist.open_table "auth_configs"

let key_of_uuid_option = function
  | None -> ""
  | Some x -> raw_string_of_uuid x

let get_auth_config x =
  try%lwt Ocsipersist.find auth_configs (key_of_uuid_option x)
  with Not_found -> return []

let set_auth_config x c =
  Ocsipersist.add auth_configs (key_of_uuid_option x) c

let get_raw_election uuid =
  try%lwt
    let lines = Lwt_io.lines_of_file (!spool_dir / raw_string_of_uuid uuid / "election.json") in
    begin match%lwt Lwt_stream.to_list lines with
    | x :: _ -> return @@ Some x
    | [] -> return_none
    end
  with _ -> return_none

let empty_metadata = {
  e_owner = None;
  e_auth_config = None;
  e_cred_authority = None;
  e_trustees = None;
  e_languages = None;
}

let return_empty_metadata = return empty_metadata

let get_election_metadata uuid =
  try%lwt
    Lwt_io.chars_of_file (!spool_dir / raw_string_of_uuid uuid / "metadata.json") |>
    Lwt_stream.to_string >>= fun x ->
    return @@ metadata_of_string x
  with _ -> return_empty_metadata

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
  try%lwt
    let lines = Lwt_io.lines_of_file (!spool_dir / raw_string_of_uuid uuid / "voters.txt") in
    let%lwt lines = Lwt_stream.to_list lines in
    return @@ Some lines
  with _ -> return_none

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
  try%lwt
    let lines = Lwt_io.lines_of_file (!spool_dir / raw_string_of_uuid uuid / "public_keys.jsons") in
    let%lwt lines = Lwt_stream.to_list lines in
    return @@ Some lines
  with _ -> return_none

let get_private_keys uuid =
  try%lwt
    let lines = Lwt_io.lines_of_file (!spool_dir / raw_string_of_uuid uuid / "private_keys.jsons") in
    let%lwt lines = Lwt_stream.to_list lines in
    return @@ Some lines
  with _ -> return_none

let get_threshold uuid =
  try%lwt
    Lwt_io.chars_of_file (!spool_dir / raw_string_of_uuid uuid / "threshold.json") |>
    Lwt_stream.to_string >>= fun x ->
    return (Some x)
  with _ -> return_none

module Ballots = Map.Make (String)

module BallotsCacheTypes = struct
  type key = uuid
  type value = string Ballots.t
end

module BallotsCache = Ocsigen_cache.Make (BallotsCacheTypes)

let raw_get_ballots_archived uuid =
  try%lwt
    let ballots = Lwt_io.lines_of_file (!spool_dir / raw_string_of_uuid uuid / "ballots.jsons") in
    Lwt_stream.fold (fun b accu ->
      let hash = sha256_b64 b in
      Ballots.add hash b accu
    ) ballots Ballots.empty
  with _ -> return Ballots.empty

let archived_ballots_cache =
  new BallotsCache.cache raw_get_ballots_archived 10

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
