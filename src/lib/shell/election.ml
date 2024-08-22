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
  | `Assoc o -> (
      match List.assoc_opt "version" o with
      | None -> 0
      | Some (`Int x) -> x
      | _ -> failwith "Election.of_string: invalid version")
  | _ -> failwith "Election.of_string: invalid data"

let get_uuid x =
  let j = Yojson.Safe.from_string x in
  match j with
  | `Assoc o -> (
      match List.assoc_opt "uuid" o with
      | Some (`String x) -> Uuid.wrap x
      | _ -> failwith "Election.get_uuid: invalid data")
  | _ -> failwith "Election.get_uuid: object expected"

module type SERIALIZABLE_QUESTION = sig
  type t

  val read_question : t reader
  val write_question : t writer
  val of_concrete : Belenios_question.t -> t
  val to_concrete : t -> Belenios_question.t
end

module Serializable_question_v1 = struct
  open Belenios_v1

  type t = Question.t

  let read_question = Serializable_j.read_question
  let write_question = Serializable_j.write_question
  let of_concrete = Question.of_concrete
  let to_concrete = Question.to_concrete
end

type _ version = V1 : Belenios_v1.Question.t version

let get_serializers (type a) (v : a version) :
    (module SERIALIZABLE_QUESTION with type t = a) =
  match v with V1 -> (module Serializable_question_v1)

let compare_version (type t) (x : t version) (type u) (y : u version) :
    (t, u) eq option =
  match (x, y) with V1, V1 -> Some Refl

type some_version = Version : 'a version -> some_version

let int_of_version (type t) (x : t version) = match x with V1 -> 1

let version_of_int = function
  | 1 -> Version V1
  | v -> Printf.ksprintf invalid_arg "unsupported version: %d" v

type versioned_template =
  | Template : 'a version * 'a template -> versioned_template

let template_of_string x =
  match get_version x with
  | 1 -> Template (V1, Belenios_v1.Election.template_of_string x)
  | n ->
      Printf.ksprintf failwith "Election.of_string: unsupported version: %d" n

let string_of_template (Template (V1, x)) =
  let open Belenios_v1.Serializable_j in
  string_of_template write_question x

let election_uuid_of_string_ballot x =
  let j = Yojson.Safe.from_string x in
  match j with
  | `Assoc o -> (
      match List.assoc_opt "election_uuid" o with
      | Some (`String x) -> Uuid.wrap x
      | _ -> failwith "election_uuid_of_string_ballot: invalid election_uuid")
  | _ -> failwith "election_uuid_of_string_ballot: invalid ballot"

let make_raw_election ~version template ~uuid ~group ~public_key =
  match version with
  | 1 -> (
      match template with
      | Template (V1, template) ->
          Belenios_v1.Election.make_raw_election template ~uuid ~group
            ~public_key)
  | n ->
      Printf.ksprintf invalid_arg "make_raw_election: unsupported version: %d" n

(** Helper functions *)

let has_nh_questions (Template (V1, e)) =
  Array.exists Belenios_v1.Question.is_nh_question e.t_questions

let get_questions (Template (V1, e)) =
  Array.map Belenios_v1.Question.to_concrete e.t_questions

module type ELECTION = sig
  include ELECTION

  val witness : question version
end

let of_string random x =
  match get_version x with
  | 1 ->
      let module R = struct
        let raw_election = x
      end in
      let module M = (val random : RANDOM) in
      let module X = struct
        type question = Belenios_v1.Question.t

        include Belenios_v1.Election.Make (R) (M) ()

        let witness = V1
      end in
      (module X : ELECTION)
  | n ->
      Printf.ksprintf failwith "Election.of_string: unsupported version: %d" n

let supported_crypto_versions = [ Version V1 ]

(** Computing checksums *)

let compute_checksums ~election ~trustees ~public_credentials ~shuffles
    ~encrypted_tally =
  let ec_public_credentials =
    Hash.hash_string (string_of_public_credentials public_credentials)
  in
  let ec_num_voters = List.length public_credentials in
  let ec_weights =
    let w_total, min, max =
      List.fold_left
        (fun (total, min, max) cred ->
          let p = parse_public_credential Fun.id cred in
          let w = Option.value ~default:Weight.one p.weight in
          let total = Weight.(total + w) in
          let min =
            match min with None -> Some w | Some w' -> Some (Weight.min w w')
          in
          let max =
            match max with None -> Some w | Some w' -> Some (Weight.max w w')
          in
          (total, min, max))
        (Weight.zero, None, None) public_credentials
    in
    if Weight.is_int w_total ec_num_voters then None
    else
      match (min, max) with
      | Some w_min, Some w_max -> Some { w_total; w_min; w_max }
      | _ -> failwith "inconsistent weights in credentials"
  in
  let tc_of_tpk k =
    let tc_checksum =
      Hash.hash_string (Yojson.Safe.to_string k.trustee_public_key)
    in
    let tc_name = k.trustee_name in
    { tc_checksum; tc_name }
  in
  let trustees =
    trustees_of_string Yojson.Safe.read_json Yojson.Safe.read_json trustees
  in
  let ec_trustees =
    trustees
    |> List.map (function `Single k -> [ tc_of_tpk k ] | `Pedersen _ -> [])
    |> List.flatten
  in
  let ec_trustees_threshold =
    trustees
    |> List.map (function
         | `Single _ -> []
         | `Pedersen p ->
             let ts_trustees =
               List.combine
                 (Array.to_list p.t_verification_keys)
                 (Array.to_list p.t_certs)
               |> List.map (fun (key, cert) ->
                      {
                        ttc_name = key.trustee_name;
                        ttc_pki_key = Hash.hash_string cert.s_message;
                        ttc_verification_key =
                          Hash.hash_string
                            (Yojson.Safe.to_string key.trustee_public_key);
                      })
             in
             [ { ts_trustees; ts_threshold = p.t_threshold } ])
    |> List.flatten
  in
  let find_trustee_name_by_id =
    let names =
      trustees
      |> List.map (function
           | `Single k -> [ k.trustee_name ]
           | `Pedersen t ->
               Array.to_list t.t_verification_keys
               |> List.map (fun x -> x.trustee_name))
      |> List.flatten |> Array.of_list
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
        })
      shuffles
  in
  let ec_shuffles =
    let& shuffles = shuffles in
    Some (process_shuffles shuffles)
  in
  {
    ec_election = election;
    ec_trustees;
    ec_trustees_threshold;
    ec_public_credentials;
    ec_shuffles;
    ec_encrypted_tally = encrypted_tally;
    ec_num_voters;
    ec_weights;
  }
