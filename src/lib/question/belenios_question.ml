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

type t = Types.question

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

let unwrap (q : t) =
  match lookup_type q.type_ with
  | None ->
      Printf.ksprintf invalid_arg "write_question: unsupported type %s" q.type_
  | Some x -> (
      let module X = (val x) in
      match X.unwrap q with Some x -> x | None -> invalid_arg "write_question")

let read_question a b = Yojson.Safe.read_json a b |> wrap
let write_question b x = unwrap x |> Yojson.Safe.write_json b

let is_nh_question (x : t) =
  match x.value with Non_homomorphic.Q _ -> true | _ -> false

let extract (x : t) =
  match lookup_type x.type_ with
  | None -> None
  | Some q -> (
      let module Ops = (val q) in
      match Ops.extract x.value with
      | None -> None
      | Some x ->
          let module X = struct
            module Ops = Ops

            let it = x
          end in
          Some (module X : Types.PACK))

let erase_question x =
  match extract x with
  | None -> failwith "erase_question"
  | Some p ->
      let module P = (val p) in
      { x with value = P.Ops.Q (P.Ops.erase P.it) }

let check_question group x =
  match extract x with
  | None -> Ok ()
  | Some p ->
      let module P = (val p) in
      P.Ops.check group { x with value = P.it }
