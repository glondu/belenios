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
open Serializable_j
open Common
open Web_serializable_j
open Web_common

let ( / ) = Filename.concat

let get_election_result uuid =
  try_lwt
    Lwt_io.chars_of_file (!spool_dir / uuid / "result.json") |>
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
  try_lwt Ocsipersist.find election_states x
  with Not_found -> return `Archived

let set_election_state x s =
  Ocsipersist.add election_states x s

let past = datetime_of_string "\"2015-10-01 00:00:00.000000\""

let set_election_date uuid d =
  let dates = { e_finalization = d } in
  Lwt_io.(with_file Output (!spool_dir / uuid / "dates.json") (fun oc ->
    write_line oc (string_of_election_dates dates)
  ))

let get_election_date uuid =
    try_lwt
      Lwt_io.chars_of_file (!spool_dir / uuid / "dates.json") |>
      Lwt_stream.to_string >>= fun x ->
      let dates = election_dates_of_string x in
      return dates.e_finalization
    with _ ->
      return past

let election_pds = Ocsipersist.open_table "election_pds"

let get_partial_decryptions x =
  try_lwt Ocsipersist.find election_pds x
  with Not_found -> return []

let set_partial_decryptions x pds =
  Ocsipersist.add election_pds x pds

let auth_configs = Ocsipersist.open_table "auth_configs"

let get_auth_config x =
  try_lwt Ocsipersist.find auth_configs x
  with Not_found -> return []

let set_auth_config x c =
  Ocsipersist.add auth_configs x c

let get_raw_election uuid =
  try_lwt
    let lines = Lwt_io.lines_of_file (!spool_dir / uuid / "election.json") in
    begin match_lwt Lwt_stream.to_list lines with
    | x :: _ -> return @@ Some x
    | [] -> return_none
    end
  with _ -> return_none

let empty_metadata = {
  e_owner = None;
  e_auth_config = None;
  e_cred_authority = None;
  e_trustees = None;
}

let return_empty_metadata = return empty_metadata

let get_election_metadata uuid =
  try_lwt
    Lwt_io.chars_of_file (!spool_dir / uuid / "metadata.json") |>
    Lwt_stream.to_string >>= fun x ->
    return @@ metadata_of_string x
  with _ -> return_empty_metadata

let get_elections_by_owner user =
  Lwt_unix.files_of_directory !spool_dir |>
  Lwt_stream.filter_s (fun x ->
    if x = "." || x = ".." then return false else
    lwt metadata = get_election_metadata x in
    match metadata.e_owner with
    | Some o -> return (o = user)
    | None -> return false
  ) |> Lwt_stream.to_list

let get_voters uuid =
  try_lwt
    let lines = Lwt_io.lines_of_file (!spool_dir / uuid / "voters.txt") in
    lwt lines = Lwt_stream.to_list lines in
    return @@ Some lines
  with _ -> return_none

let get_passwords uuid =
  let csv =
    try Some (Csv.load (!spool_dir / uuid / "passwords.csv"))
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


module Ballots = Map.Make (String)

module BallotsCacheTypes = struct
  type key = string
  type value = string Ballots.t
end

module BallotsCache = Ocsigen_cache.Make (BallotsCacheTypes)

let raw_get_ballots_archived uuid =
  try_lwt
    let ballots = Lwt_io.lines_of_file (!spool_dir / uuid / "ballots.jsons") in
    Lwt_stream.fold (fun b accu ->
      let hash = sha256_b64 b in
      Ballots.add hash b accu
    ) ballots Ballots.empty
  with _ -> return Ballots.empty

let archived_ballots_cache =
  new BallotsCache.cache raw_get_ballots_archived 10

let get_ballot_hashes ~uuid =
  match_lwt get_election_state uuid with
  | `Archived ->
     lwt ballots = archived_ballots_cache#find uuid in
     Ballots.bindings ballots |> List.map fst |> return
  | _ ->
     let table = Ocsipersist.open_table ("ballots_" ^ underscorize uuid) in
     Ocsipersist.fold_step (fun hash _ accu ->
       return (hash :: accu)
     ) table [] >>= (fun x -> return @@ List.rev x)

let get_ballot_by_hash ~uuid ~hash =
  match_lwt get_election_state uuid with
  | `Archived ->
     lwt ballots = archived_ballots_cache#find uuid in
     (try Some (Ballots.find hash ballots) with Not_found -> None) |> return
  | _ ->
     let table = Ocsipersist.open_table ("ballots_" ^ underscorize uuid) in
     try_lwt Ocsipersist.find table hash >>= (fun x -> return @@ Some x)
     with Not_found -> return_none
