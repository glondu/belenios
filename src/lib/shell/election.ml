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

open Belenios_platform
open Belenios_core
open Platform
open Serializable_builtin_t
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
  | 0 -> Belenios_v0.Election.of_string x
  | 1 -> Belenios_v1.Election.of_string x
  | n -> Printf.ksprintf failwith "Election.of_string: unsupported version: %d" n


let election_uuid_of_string_ballot x =
  let j = Yojson.Safe.from_string x in
  match j with
  | `Assoc o ->
     (match List.assoc_opt "election_uuid" o with
      | Some (`String x) -> uuid_of_raw_string x
      | _ -> failwith "election_uuid_of_string_ballot: invalid election_uuid"
     )
  | _ -> failwith "election_uuid_of_string_ballot: invalid ballot"

let make_raw_election params ~group ~public_key =
  match params.e_version with
  | 0 -> Belenios_v0.Election.to_string params ~group ~public_key
  | 1 -> Belenios_v1.Election.to_string params ~group ~public_key
  | n ->
     Printf.ksprintf invalid_arg
       "make_raw_election: unsupported version: %d" n

module MakeResult (X : ELECTION_BASE) = struct
  open X
  type result = raw_result

  let cast_result x =
    let questions = election.e_questions in
    let n = Array.length questions in
    if Array.length x = n then (
      let rec check i =
        if i < n then (
          match questions.(i), x.(i) with
          | Homomorphic _, RHomomorphic _ -> check (i + 1)
          | NonHomomorphic _, RNonHomomorphic _ -> check (i + 1)
          | _, _ -> failwith "cast_result: type mismatch"
        ) else ()
      in
      check 0;
      x
    ) else failwith "cast_result: length mismatch"

  let write_result = write_raw_result

  let read_result state buf =
    match Yojson.Safe.from_lexbuf ~stream:true state buf with
    | `List xs ->
       let n = Array.length election.e_questions in
       let result = Array.make n (RHomomorphic [||]) in
       let rec loop i xs =
         match (i < n), xs with
         | true, (x :: xs) ->
            (match election.e_questions.(i) with
             | Homomorphic _ ->
                (match x with
                 | `List ys ->
                    ys
                    |> Array.of_list
                    |> Array.map weight_of_json
                    |> (fun x -> result.(i) <- RHomomorphic x)
                    |> (fun () -> loop (i + 1) xs)
                 | _ -> failwith "read_result/Homomorphic: list expected"
                )
             | NonHomomorphic _ ->
                (match x with
                 | `List ys ->
                    ys
                    |> Array.of_list
                    |> Array.map
                         (function
                          | `List zs ->
                             zs
                             |> Array.of_list
                             |> Array.map
                                  (function
                                   | `Int i -> i
                                   | _ -> failwith "read_result: int expected"
                                  )
                          | _ -> failwith "read_result/NonHomomorphic: list list expected"
                         )
                    |> (fun x -> result.(i) <- RNonHomomorphic x)
                    |> (fun () -> loop (i + 1) xs)
                 | _ -> failwith "read_result/NonHomomorphic: list expected"
                )
            )
         | true, [] -> failwith "read_result: list too short"
         | false, _ :: _ -> failwith "read_result: list too long"
         | false, [] -> ()
       in
       loop 0 xs;
       result
    | _ -> failwith "read_result: list expected"

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
    | 0 -> (module Belenios_v0.Election.Make : MAKER)
    | 1 -> (module Belenios_v1.Election.Make : MAKER)
    | n -> Printf.ksprintf failwith "Election.Make: unsupported version: %d" n

  module X = (val x)
  include X (MakeResult) (R) (M) ()
end

(** Computing checksums *)

let compute_checksums ~election result_or_shuffles ~trustees ~public_credentials =
  let ec_election = sha256_b64 election in
  let ec_public_credentials = sha256_b64 public_credentials in
  let tc_of_tpk k =
    let tc_checksum = sha256_b64 (Yojson.Safe.to_string k.trustee_public_key) in
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
                        ttc_pki_key = sha256_b64 cert.s_message;
                        ttc_verification_key =
                          sha256_b64 (Yojson.Safe.to_string key.trustee_public_key);
                      }
                    )
             in
             [{ ts_trustees; ts_threshold = p.t_threshold }]
         )
    |> List.flatten
    |> (fun x -> Some x)
  in
  let combine shuffles shufflers =
    match shuffles, shufflers with
    | Some shuffles, Some shufflers ->
       List.combine shuffles shufflers
       |> List.map
            (fun (shuffle, shuffler) ->
              let shuffle = string_of_shuffle Yojson.Safe.write_json shuffle in
              let tc_checksum = sha256_b64 shuffle in
              {tc_checksum; tc_name = shuffler}
            )
       |> (fun x -> Some x)
    | Some shuffles, None ->
       shuffles
       |> List.map
            (fun shuffle ->
              let shuffle = string_of_shuffle Yojson.Safe.write_json shuffle in
              let tc_checksum = sha256_b64 shuffle in
              {tc_checksum; tc_name = None}
            )
       |> (fun x -> Some x)
    | None, None -> None
    | _, _ -> failwith "ill-formed result"
  in
  let ec_shuffles, ec_encrypted_tally =
    match result_or_shuffles with
    | `Nothing -> None, None
    | `Shuffles (shuffles, shufflers) ->
       let shuffles = List.map (shuffle_of_string Yojson.Safe.read_json) shuffles in
       combine (Some shuffles) shufflers, None
    | `Result result ->
       let result = election_result_of_string Yojson.Safe.read_json Yojson.Safe.read_json result in
       let tally = string_of_encrypted_tally Yojson.Safe.write_json result.encrypted_tally in
       combine result.shuffles result.shufflers, Some (sha256_b64 tally)
  in
  {
    ec_election; ec_pki = None; ec_trustees; ec_trustees_threshold;
    ec_public_credentials; ec_shuffles; ec_encrypted_tally
  }
