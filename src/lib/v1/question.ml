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

open Belenios_core.Signatures
open Belenios_core.Common
open Belenios_question

let types : (module Types.QUESTION_KIND) list =
  [ (module Question_h); (module Question_nh) ]

let lookup_type type_ =
  let rec loop = function
    | [] -> None
    | x :: xs ->
        let module X = (val x : Types.QUESTION_KIND) in
        if X.type_ = type_ then Some x else loop xs
  in
  loop types

module type PACK = sig
  type question

  val concrete : Belenios_question.t
  val abstract : question

  module Kind : Types.QUESTION_KIND with type question = question
end

type t = (module PACK)

let to_concrete (x : t) =
  let module X = (val x) in
  X.concrete

let of_concrete (x : Belenios_question.t) : t =
  match lookup_type x.type_ with
  | None -> Printf.ksprintf failwith "of_concrete: unsupported type %s" x.type_
  | Some kind ->
      let module Kind = (val kind) in
      let module X = struct
        type question = Kind.question

        let concrete = x

        let abstract =
          match Kind.of_concrete x with
          | Some x -> x
          | None -> failwith "of_concrete"

        module Kind = Kind
      end in
      (module X)

let wrap x = x |> Belenios_question.wrap |> of_concrete
let unwrap x = x |> to_concrete |> Belenios_question.unwrap
let is_nh_question x = is_nh_question (to_concrete x)

module Make (M : RANDOM) (G : GROUP) = struct
  let read_answer = Yojson.Safe.read_json
  let write_answer = Yojson.Safe.write_json

  let create_answer (x : t) ~public_key ~prefix m =
    let module X = (val x) in
    let module Q = X.Kind.Make (M) (G) in
    Q.create_answer X.abstract ~public_key ~prefix m
    |> ( -- ) Q.write_answer |> Yojson.Safe.from_string

  let verify_answer (x : t) ~public_key ~prefix a =
    let module X = (val x) in
    let module Q = X.Kind.Make (M) (G) in
    a |> Yojson.Safe.to_string |> ( ++ ) Q.read_answer
    |> Q.verify_answer X.abstract ~public_key ~prefix

  let extract_ciphertexts (x : t) a =
    let module X = (val x) in
    let module Q = X.Kind.Make (M) (G) in
    a |> Yojson.Safe.to_string |> ( ++ ) Q.read_answer
    |> Q.extract_ciphertexts X.abstract

  let process_ciphertexts (x : t) e =
    let module X = (val x) in
    let module Q = X.Kind.Make (M) (G) in
    Q.process_ciphertexts X.abstract e

  let compute_result ~total_weight (q : t) x =
    let module X = (val q) in
    let module Q = X.Kind.Make (M) (G) in
    Q.compute_result ~total_weight X.abstract x |> ( -- ) X.Kind.write_result

  let check_result ~total_weight (q : t) x r =
    let module X = (val q) in
    let module Q = X.Kind.Make (M) (G) in
    r |> ( ++ ) X.Kind.read_result |> Q.check_result ~total_weight X.abstract x
end
