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

let get_version j =
  match j with
  | `Assoc o -> (
      match List.assoc_opt "version" o with
      | None -> 0
      | Some (`Int x) -> x
      | _ -> failwith "Election.of_string: invalid version")
  | _ -> failwith "Election.of_string: invalid data"

let get_uuid x =
  let j = Json.of_string x in
  match j with
  | `Assoc o -> (
      match List.assoc_opt "uuid" o with
      | Some (`String x) -> Uuid.of_string x
      | _ -> failwith "Election.get_uuid: invalid data")
  | _ -> failwith "Election.get_uuid: object expected"

module type SERIALIZABLE_QUESTION = sig
  type t [@@deriving yojson]

  val of_concrete : Belenios_question.t -> t
  val to_concrete : t -> Belenios_question.t
end

type _ version = V2 : Belenios_v2.Question.t version

let get_serializers (type a) (v : a version) :
    (module SERIALIZABLE_QUESTION with type t = a) =
  match v with V2 -> (module Belenios_v2.Question)

let compare_version (type t) (x : t version) (type u) (y : u version) :
    (t, u) Type.eq option =
  match (x, y) with V2, V2 -> Some Equal

type some_version = Version : 'a version -> some_version

let int_of_version (type t) (x : t version) = match x with V2 -> 2

let version_of_int = function
  | 2 -> Version V2
  | v -> Printf.ksprintf invalid_arg "unsupported version: %d" v

type versioned_template =
  | Template : 'a version * 'a template -> versioned_template

let versioned_template_of_yojson x =
  match get_version x with
  | 2 -> Template (V2, Belenios_v2.Election.template_of_yojson x)
  | n ->
      Printf.ksprintf failwith "Election.of_string: unsupported version: %d" n

let yojson_of_versioned_template (Template (V2, x)) =
  let open Belenios_v2 in
  yojson_of_template yojson_of_question x

let election_uuid_of_string_ballot x =
  let j = Json.of_string x in
  match j with
  | `Assoc o -> (
      match List.assoc_opt "election_uuid" o with
      | Some (`String x) -> Uuid.of_string x
      | _ -> failwith "election_uuid_of_string_ballot: invalid election_uuid")
  | _ -> failwith "election_uuid_of_string_ballot: invalid ballot"

let make_raw_election ~version template ~uuid ~group ~public_key =
  match version with
  | 2 -> (
      match template with
      | Template (V2, template) ->
          Belenios_v2.Election.make_raw_election template ~uuid ~group
            ~public_key)
  | n ->
      Printf.ksprintf invalid_arg "make_raw_election: unsupported version: %d" n

(** Helper functions *)

let has_nh_questions (Template (V2, e)) =
  Array.exists Belenios_v2.Question.is_nh_question e.questions

let get_questions (Template (V2, e)) =
  Array.map Belenios_v2.Question.to_concrete e.questions

let get_complexity (Template (V2, e)) =
  Belenios_v2.Election.get_complexity e.questions

module type ELECTION = sig
  include ELECTION

  val witness : question version
  val json : json
end

type t = (module ELECTION)
type ('a, 'b) u = (module ELECTION with type G.t = 'a and type G.Zq.t = 'b)

let t_of_yojson x =
  match get_version x with
  | 2 ->
      let module R = struct
        let raw_election = Json.to_string x
      end in
      let module X = struct
        type question = Belenios_v2.Question.t

        include Belenios_v2.Election.Make (R) ()

        let witness = V2
        let json = x
      end in
      (module X : ELECTION)
  | n ->
      Printf.ksprintf failwith "Election.of_string: unsupported version: %d" n

let yojson_of_t x =
  let module X = (val x : ELECTION) in
  X.json

let supported_crypto_versions = [ Version V2 ]

(** Computing checksums *)

let compute_checksums (type a b) (election : (a, b) u) trustees
    public_credentials ~shuffles ~encrypted_tally ~final =
  let module W = (val election) in
  let ec_public_credentials =
    Hash.hash_string
      (!+(yojson_of_public_credentials !&W.G.to_string) public_credentials)
  in
  let ec_num_voters = List.length public_credentials in
  let ec_weights =
    let total, min, max =
      List.fold_left
        (fun (total, min, max) (p : _ public_credential) ->
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
    if Weight.is_int total ec_num_voters then None
    else
      match (min, max) with
      | Some min, Some max -> Some { total; min; max }
      | _ -> failwith "inconsistent weights in credentials"
  in
  let tc_of_basic (k : _ basic_parameters) =
    let checksum =
      Hash.hash_string @@ W.G.to_string k.cert.message.verification
    in
    let name = k.verification_key.message.message.name in
    { checksum; name }
  in
  let tc_of_pedersen (p : _ threshold_parameters) : trustee_threshold_set =
    let trustees =
      List.combine (Array.to_list p.verification_keys) (Array.to_list p.certs)
      |> List.map
           (fun
             ((key : _ threshold_verification_key), (cert : _ pedersen_cert)) ->
             ({
                checksum =
                  Hash.hash_string @@ W.G.to_string cert.message.verification;
                name = key.message.message.name;
              }
               : trustee_checksum))
    in
    { trustees; threshold = p.context.threshold }
  in
  let trustees_basic =
    trustees
    |> List.map (function `Single k -> [ tc_of_basic k ] | `Pedersen _ -> [])
    |> List.flatten
  in
  let trustees_threshold =
    trustees
    |> List.map (function
      | `Single _ -> []
      | `Pedersen p -> [ tc_of_pedersen p ])
    |> List.flatten
  in
  let find_trustee_name_by_id =
    let names =
      trustees
      |> List.map (fun (x : _ trustee_kind) ->
          match x with
          | `Single k -> [ k.verification_key.message.message.name ]
          | `Pedersen t ->
              Array.to_list t.verification_keys
              |> List.map (fun (x : _ threshold_verification_key) ->
                  x.message.message.name))
      |> List.flatten |> Array.of_list
    in
    fun id ->
      if 0 < id && id <= Array.length names then names.(id - 1)
      else failwith __FUNCTION__
  in
  let process_shuffles shuffles =
    List.map
      (fun x ->
        ({ checksum = x.payload; name = find_trustee_name_by_id x.owner }
          : trustee_checksum))
      shuffles
  in
  let ec_shuffles =
    let& shuffles = shuffles in
    Some (process_shuffles shuffles)
  in
  {
    election = Hash.hash_yojson W.json;
    trustees_basic;
    trustees_threshold;
    public_credentials = ec_public_credentials;
    shuffles = ec_shuffles;
    encrypted_tally;
    num_voters = ec_num_voters;
    weights = ec_weights;
    final;
  }
