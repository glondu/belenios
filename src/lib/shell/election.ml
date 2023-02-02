(**************************************************************************)
(*                                BELENIOS                                *)
(*                                                                        *)
(*  Copyright © 2012-2022 Inria                                           *)
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

open Belenios_core
open Serializable_j
open Signatures
open Common

let get_version x =
  let j = Yojson.Safe.from_string x in
  match j with
  | `Assoc o ->
     (match List.assoc_opt "version" o with
      | None -> 0
      | Some (`Int x) -> x
      | _ -> failwith "Election.of_string: invalid version"
     )
  | _ -> failwith "Election.of_string: invalid data"

let of_string x =
  match get_version x with
  | 1 -> Belenios_v1.Election.of_string x
  | n -> Printf.ksprintf failwith "Election.of_string: unsupported version: %d" n


let election_uuid_of_string_ballot x =
  let j = Yojson.Safe.from_string x in
  match j with
  | `Assoc o ->
     (match List.assoc_opt "election_uuid" o with
      | Some (`String x) -> Uuid.wrap x
      | _ -> failwith "election_uuid_of_string_ballot: invalid election_uuid"
     )
  | _ -> failwith "election_uuid_of_string_ballot: invalid ballot"

let make_raw_election params ~group ~public_key =
  match params.e_version with
  | 1 -> Belenios_v1.Election.to_string params ~group ~public_key
  | n ->
     Printf.ksprintf invalid_arg
       "make_raw_election: unsupported version: %d" n

module MakeResult (X : ELECTION_BASE) = struct
  open X
  type result = S.t Election_result.t

  let write_result buf x =
    Yojson.Safe.write_json buf (Election_result.unwrap x)

  let read_result state buf =
    Election_result.wrap S.x (Yojson.Safe.read_json state buf)

end

(** Helper functions *)

let has_nh_questions e =
  Array.exists (function
      | Question.NonHomomorphic _ -> true
      | Question.Homomorphic _ -> false
    ) e.e_questions

module type MAKER =
  functor (MakeResult : MAKE_RESULT) (R : RAW_ELECTION) (M : RANDOM) ()  ->
  ELECTION with type 'a m = 'a M.t

module Make (R : RAW_ELECTION) (M : RANDOM) () = struct

  let x =
    match get_version R.raw_election with
    | 1 -> (module Belenios_v1.Election.Make : MAKER)
    | n -> Printf.ksprintf failwith "Election.Make: unsupported version: %d" n

  module X = (val x)
  include X (MakeResult) (R) (M) ()
end

(** Computing checksums *)

let compute_checksums ~election ~trustees ~public_credentials ~shuffles ~encrypted_tally =
  let ec_public_credentials =
    Hash.hash_string (string_of_public_credentials public_credentials)
  in
  let ec_num_voters = List.length public_credentials in
  let ec_weights =
    let w_total, min, max =
      List.fold_left
        (fun (total, min, max) cred ->
          match String.index cred ',' with
          | exception Not_found -> Weight.(total + one), min, max
          | i ->
             let n = String.length cred in
             let w = Weight.of_string (String.sub cred (i + 1) (n - i - 1)) in
             let total = Weight.(total + w) in
             let min = match min with None -> Some w | Some w' -> Some (Weight.min w w') in
             let max = match max with None -> Some w | Some w' -> Some (Weight.max w w') in
             total, min, max
        ) (Weight.zero, None, None) public_credentials
    in
    if Weight.is_int w_total ec_num_voters then (
      None
    ) else (
      match min, max with
      | Some w_min, Some w_max -> Some {w_total; w_min; w_max}
      | _ -> failwith "inconsistent weights in credentials"
    )
  in
  let tc_of_tpk k =
    let tc_checksum = Hash.hash_string (Yojson.Safe.to_string k.trustee_public_key) in
    let tc_name = k.trustee_name in
    {tc_checksum; tc_name}
  in
  let trustees = trustees_of_string Yojson.Safe.read_json trustees in
  let ec_trustees =
    trustees
    |> List.map
         (function
          | `Single k -> [tc_of_tpk k]
          | `Pedersen _ -> []
         )
    |> List.flatten
  in
  let ec_trustees_threshold =
    trustees
    |> List.map
         (function
          | `Single _ -> []
          | `Pedersen p ->
             let ts_trustees =
               List.combine (Array.to_list p.t_verification_keys) (Array.to_list p.t_certs)
               |> List.map
                    (fun (key, cert) ->
                      {
                        ttc_name = key.trustee_name;
                        ttc_pki_key = Hash.hash_string cert.s_message;
                        ttc_verification_key =
                          Hash.hash_string (Yojson.Safe.to_string key.trustee_public_key);
                      }
                    )
             in
             [{ ts_trustees; ts_threshold = p.t_threshold }]
         )
    |> List.flatten
  in
  let find_trustee_name_by_id =
    let names =
      trustees
      |> List.map
           (function
            | `Single k -> [k.trustee_name]
            | `Pedersen t ->
               Array.to_list t.t_verification_keys
               |> List.map (fun x -> x.trustee_name)
           )
      |> List.flatten
      |> Array.of_list
    in
    fun id ->
    if 0 < id && id <= Array.length names then names.(id - 1) else None
  in
  let process_shuffles shuffles =
    List.map
      (fun x ->
        {
          tc_checksum = x.owned_payload;
          tc_name = find_trustee_name_by_id x.owned_owner;
        }
      ) shuffles
  in
  let ec_shuffles =
    let& shuffles in
    Some (process_shuffles shuffles)
  in
  {
    ec_election = election;
    ec_trustees;
    ec_trustees_threshold;
    ec_public_credentials;
    ec_shuffles;
    ec_encrypted_tally = encrypted_tally;
    ec_num_voters; ec_weights;
  }
