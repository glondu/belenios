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

open Types
module Homomorphic = Homomorphic
module Non_homomorphic = Non_homomorphic
module Lists = Lists

type t = question

let types : (module QUESTION) list =
  [ (module Homomorphic); (module Non_homomorphic); (module Lists) ]

let lookup_type type_ =
  let rec loop = function
    | [] -> None
    | x :: xs ->
        let module X = (val x : QUESTION) in
        if X.type_ = type_ then Some x else loop xs
  in
  loop types

let t_of_yojson x =
  let x = generic_question_of_yojson Fun.id x in
  match lookup_type x.type_ with
  | None ->
      Printf.ksprintf invalid_arg "%s: unsupported type %s" __FUNCTION__ x.type_
  | Some q ->
      let open (val q) in
      { x with value = Q (t_of_yojson x.value) }

let yojson_of_t (x : t) =
  match lookup_type x.type_ with
  | None ->
      Printf.ksprintf invalid_arg "%s: unsupported type %s" __FUNCTION__ x.type_
  | Some q -> (
      let open (val q) in
      match x.value with
      | Q q ->
          yojson_of_generic_question Fun.id { x with value = yojson_of_t q }
      | _ -> invalid_arg __FUNCTION__)

let is_nh_question (x : t) =
  match x.value with Non_homomorphic.Q _ -> true | _ -> false

let extract (x : t) =
  match lookup_type x.type_ with
  | None -> None
  | Some q -> (
      let module Ops = (val q) in
      match x.value with
      | Ops.Q q ->
          let module X = struct
            module Ops = Ops

            let it = q
          end in
          Some (module X : Types.PACK)
      | _ -> None)

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
