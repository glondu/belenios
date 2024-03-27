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

open Belenios_core.Common
module Homomorphic = Homomorphic
module Non_homomorphic = Non_homomorphic
module Lists = Lists

type t = Types.question = {
  type_ : string;
  value : Types.raw_question;
  extra : Yojson.Safe.t option;
}

let types : (module Types.QUESTION) list =
  [ (module Homomorphic); (module Non_homomorphic); (module Lists) ]

let lookup_type type_ =
  let rec loop = function
    | [] -> None
    | x :: xs ->
        let module X = (val x : Types.QUESTION) in
        if X.type_ = type_ then Some x else loop xs
  in
  loop types

let wrap = function
  | `Assoc o as j -> (
      let type_, value, extra =
        match List.assoc_opt "type" o with
        | None -> ("Homomorphic", j, None)
        | Some (`String type_) ->
            let value =
              match List.assoc_opt "value" o with
              | None -> invalid_arg "read_question: value expected"
              | Some x -> x
            in
            (type_, value, List.assoc_opt "extra" o)
        | _ -> invalid_arg "read_question: string expected in type"
      in
      match lookup_type type_ with
      | None ->
          Printf.ksprintf invalid_arg "read_question: unsupported type %s" type_
      | Some x ->
          let module X = (val x) in
          X.wrap ~value ~extra)
  | _ -> invalid_arg "read_question: object expected"

let unwrap q =
  match lookup_type q.type_ with
  | None ->
      Printf.ksprintf invalid_arg "write_question: unsupported type %s" q.type_
  | Some x -> (
      let module X = (val x) in
      match X.unwrap q with Some x -> x | None -> invalid_arg "write_question")

let read_question a b = Yojson.Safe.read_json a b |> wrap
let write_question b x = unwrap x |> Yojson.Safe.write_json b

type counting_method =
  [ `None
  | `MajorityJudgment of Question_nh_t.mj_extra
  | `Schulze of Question_nh_t.schulze_extra
  | `STV of Question_nh_t.stv_extra ]

let get_counting_method extra =
  let open Question_nh_j in
  match extra with
  | Some (`Assoc o as extra) -> (
      match List.assoc_opt "method" o with
      | Some (`String "MajorityJudgment") -> (
          match extra |> Yojson.Safe.to_string |> mj_extra_of_string with
          | x -> `MajorityJudgment x
          | exception _ -> `None)
      | Some (`String "Schulze") -> (
          match extra |> Yojson.Safe.to_string |> schulze_extra_of_string with
          | x -> `Schulze x
          | exception _ -> `None)
      | Some (`String "STV") -> (
          match extra |> Yojson.Safe.to_string |> stv_extra_of_string with
          | x -> `STV x
          | exception _ -> `None)
      | _ -> `None)
  | _ -> `None

let erase_question x =
  match x.value with
  | Homomorphic.Q q ->
      let open Question_h_t in
      let q =
        {
          q_answers = Array.map (fun _ -> "") q.q_answers;
          q_blank = q.q_blank;
          q_min = q.q_min;
          q_max = q.q_max;
          q_question = "";
        }
      in
      { x with value = Homomorphic.Q q }
  | Non_homomorphic.Q q ->
      let open Question_nh_t in
      let q =
        { q_answers = Array.map (fun _ -> "") q.q_answers; q_question = "" }
      in
      { x with value = Non_homomorphic.Q q }
  | Lists.Q q ->
      let open Question_l_t in
      let q =
        {
          q_answers = Array.map (Array.map (fun _ -> "")) q.q_answers;
          q_question = "";
        }
      in
      { x with value = Lists.Q q }
  | _ -> failwith "erase_question"

let is_nh_question x =
  match x.value with Non_homomorphic.Q _ -> true | _ -> false
