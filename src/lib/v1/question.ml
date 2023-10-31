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

module Make (M : RANDOM) (G : GROUP) = struct
  module QHomomorphic = Question_h.Make (M) (G)
  module QNonHomomorphic = Question_nh.Make (M) (G)

  let create_answer x ~public_key ~prefix m =
    match x.value with
    | Homomorphic.Q q ->
        let open Homomorphic.Syntax in
        let answer = QHomomorphic.create_answer q ~public_key ~prefix m in
        answer
        |> string_of_answer (swrite G.to_string) (swrite G.Zq.to_string)
        |> Yojson.Safe.from_string
    | Non_homomorphic.Q q ->
        let open Non_homomorphic.Syntax in
        let answer = QNonHomomorphic.create_answer q ~public_key ~prefix m in
        answer
        |> string_of_answer (swrite G.to_string) (swrite G.Zq.to_string)
        |> Yojson.Safe.from_string
    | _ -> failwith "create_answer"

  let verify_answer x ~public_key ~prefix a =
    match x.value with
    | Homomorphic.Q q ->
        let open Homomorphic.Syntax in
        a |> Yojson.Safe.to_string
        |> answer_of_string (sread G.of_string) (sread G.Zq.of_string)
        |> QHomomorphic.verify_answer q ~public_key ~prefix
    | Non_homomorphic.Q q ->
        let open Non_homomorphic.Syntax in
        a |> Yojson.Safe.to_string
        |> answer_of_string (sread G.of_string) (sread G.Zq.of_string)
        |> QNonHomomorphic.verify_answer q ~public_key ~prefix
    | _ -> failwith "verify_answer"

  let extract_ciphertexts x a =
    match x.value with
    | Homomorphic.Q q ->
        let open Homomorphic.Syntax in
        a |> Yojson.Safe.to_string
        |> answer_of_string (sread G.of_string) (sread G.Zq.of_string)
        |> QHomomorphic.extract_ciphertexts q
    | Non_homomorphic.Q q ->
        let open Non_homomorphic.Syntax in
        a |> Yojson.Safe.to_string
        |> answer_of_string (sread G.of_string) (sread G.Zq.of_string)
        |> QNonHomomorphic.extract_ciphertexts q
    | _ -> failwith "extract_ciphertexts"

  let process_ciphertexts x e =
    match x.value with
    | Homomorphic.Q q -> QHomomorphic.process_ciphertexts q e
    | Non_homomorphic.Q q -> QNonHomomorphic.process_ciphertexts q e
    | _ -> failwith "process_ciphertexts"

  let compute_result ~total_weight q x =
    match q.value with
    | Homomorphic.Q q ->
        let open Homomorphic.Syntax in
        QHomomorphic.compute_result ~total_weight q x |> string_of_result
    | Non_homomorphic.Q q ->
        let open Non_homomorphic.Syntax in
        QNonHomomorphic.compute_result ~total_weight q x |> string_of_result
    | _ -> failwith "compute_result"

  let check_result ~total_weight q x r =
    match q.value with
    | Homomorphic.Q q ->
        let open Homomorphic.Syntax in
        r |> result_of_string |> QHomomorphic.check_result ~total_weight q x
    | Non_homomorphic.Q q ->
        let open Non_homomorphic.Syntax in
        r |> result_of_string |> QNonHomomorphic.check_result ~total_weight q x
    | _ -> failwith "check_result"
end
