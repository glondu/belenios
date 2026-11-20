(**************************************************************************)
(*                                BELENIOS                                *)
(*                                                                        *)
(*  Copyright © 2012-2022 Inria                                           *)
(*  Copyright © 2026 VCAST                                                *)
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

include Question_types
module Homomorphic = Question_h
module Non_homomorphic = Question_nh
module Lists = Question_l

type t = wrapped_question

let types : (module QUESTION_TYPE) list =
  [ (module Homomorphic); (module Non_homomorphic); (module Lists) ]

let lookup_type type_ =
  let rec loop = function
    | [] -> None
    | x :: xs ->
        let module X = (val x : QUESTION_TYPE) in
        if X.type_ = type_ then Some x else loop xs
  in
  loop types

(* [{"type":…,"value":…,"extra":…}]: [value] is decoded according to
   [type]. *)
let t_d : t Desc.Core.t =
  Desc.Core.map
    (generic_question_d Desc.Core.string Desc.Core.json)
    (fun x ->
      match lookup_type x.type_ with
      | None ->
          Printf.ksprintf invalid_arg "%s: unsupported type %s" __FUNCTION__
            x.type_
      | Some q ->
          let module Q = (val q) in
          Q
            {
              x with
              type_ = (module Q);
              value = Desc.Json.of_json Q.question_d x.value;
            })
    (fun (Q x) ->
      let open (val x.type_) in
      { x with type_; value = Desc.Json.to_json question_d x.value })

let yojson_of_t = Desc.Json.to_json t_d
let t_of_yojson = Desc.Json.of_json t_d

let is_nh_question (Q x : t) =
  let open (val x.type_) in
  match Id with Non_homomorphic.Id -> true | _ -> false

let erase_question (Q x : t) =
  let open (val x.type_) in
  Q { x with value = erase x.value }

let check_question group (Q x : t) =
  let open (val x.type_) in
  check group ~extra:x.extra x.value
