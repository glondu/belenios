(**************************************************************************)
(*                                BELENIOS                                *)
(*                                                                        *)
(*  Copyright © 2012-2023 Inria                                           *)
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
open Belenios_core.Question
open Common_types

let types : (module QUESTION_IMPL) list =
  [ (module Question_h); (module Question_nh); (module Question_l) ]

let lookup_type (type a) (type_ : a question_type) =
  let module Q = (val type_) in
  let rec loop = function
    | [] -> None
    | x :: xs -> (
        let module X = (val x : QUESTION_IMPL) in
        match X.Q.Id with
        | Q.Id -> Some ((module X) : a question_impl)
        | _ -> loop xs)
  in
  loop types

type question = Q : ('a question_impl, 'a) generic_question -> question

let extract (Q q : question) : Belenios_core.Question.t =
  let open (val q.type_) in
  Q { q with type_ = (module Q) }

let intract (Q q : Belenios_core.Question.t) : question =
  let open (val q.type_) in
  match lookup_type q.type_ with
  | None -> Printf.ksprintf failwith "of_concrete: unsupported type %s" type_
  | Some impl ->
      let module Impl = (val impl) in
      Q { q with type_ = (module Impl) }

let question_of_yojson = Belenios_core.Question.t_of_yojson >> intract
let yojson_of_question = extract >> Belenios_core.Question.yojson_of_t
let is_nh_question = extract >> is_nh_question

let get_complexity (Q q : question) =
  let open (val q.type_) in
  get_complexity q.value

type answer = Json.t [@@deriving yojson]
type result = Json.t [@@deriving yojson]

module Make (G : GROUP) = struct
  module G = G

  type nonrec question = question [@@deriving yojson]
  type nonrec answer = answer [@@deriving yojson]
  type nonrec result = result [@@deriving yojson]

  let create_answer (Q q : question) ~public_key ~prefix m =
    let open (val q.type_) in
    let open Make (G) in
    create_answer q.value ~public_key ~prefix m |> yojson_of_answer

  let verify_answer (Q q : question) ~public_key ~prefix a =
    let open (val q.type_) in
    let open Make (G) in
    a |> answer_of_yojson |> verify_answer q.value ~public_key ~prefix

  let extract_ciphertexts (Q q : question) a =
    let open (val q.type_) in
    let open Make (G) in
    a |> answer_of_yojson |> extract_ciphertexts q.value

  let process_ciphertexts (Q q : question) e =
    let open (val q.type_) in
    let open Make (G) in
    process_ciphertexts q.value e

  let compute_result ~total_weight (Q q : question) x =
    let open (val q.type_) in
    let open Make (G) in
    compute_result ~total_weight q.value x |> Q.yojson_of_result

  let check_result ~total_weight (Q q : question) x r =
    let open (val q.type_) in
    let open Make (G) in
    r |> Q.result_of_yojson |> check_result ~total_weight q.value x
end
