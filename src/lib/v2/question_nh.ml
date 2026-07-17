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
open Belenios_core.Question
module Q = Non_homomorphic
open Q

type nonrec 'a ciphertext = 'a ciphertext = { alpha : 'a; beta : 'a }

let cast (Q x : Belenios_core.Question.t) : question option =
  let module Q = (val x.type_) in
  match Q.Id with Id -> Some x.value | _ -> None

let get_complexity _ = { nb_ciphertexts = 1; nb_zkps = 1 }

module Make (G : GROUP) = struct
  module G = G
  open G

  (** Serializers *)

  let question_of_yojson = question_of_yojson
  let yojson_of_question = yojson_of_question
  let answer_of_yojson = [%group_of_yojson: _ answer]
  let yojson_of_answer = [%yojson_of_group: _ answer]
  let result_of_yojson = result_of_yojson
  let yojson_of_result = yojson_of_result

  (** Implementation *)

  let random () = Zq.random (Crypto_primitives.get_rng ())

  let create_answer q ~public_key:y ~prefix m =
    let m = Shape.to_array m in
    assert (Array.length q.answers = Array.length m);
    let r = random () in
    let@ m cont =
      match G.of_ints m with
      | Ok m -> cont m
      | Error _ -> invalid_arg "encoding error"
    in
    let alpha = g **~ r and beta = (y **~ r) *~ m in
    let w = random () in
    let commitment = g **~ w in
    let zkp =
      Printf.sprintf "%s|%s,%s,%s|" prefix (G.to_string y) (G.to_string alpha)
        (G.to_string beta)
    in
    let dst = dst_prefix ^ "-raweg" in
    let challenge = G.hash ~dst zkp [| commitment |] in
    let response = Zq.(w - (r * challenge)) in
    let proof = { challenge; response } in
    let choices = { alpha; beta } in
    ({ choices; proof } : _ answer)

  let verify_answer _ ~public_key:y ~prefix a =
    let { alpha; beta } = a.choices in
    let { challenge; response } = a.proof in
    G.check alpha && G.check beta
    &&
    let commitment = (g **~ response) *~ (alpha **~ challenge) in
    let zkp =
      Printf.sprintf "%s|%s,%s,%s|" prefix (G.to_string y) (G.to_string alpha)
        (G.to_string beta)
    in
    let dst = dst_prefix ^ "-raweg" in
    Zq.(challenge =% G.hash ~dst zkp [| commitment |])

  let extract_ciphertexts _ a = Shape.Atomic a.choices

  let compare_ciphertexts x y =
    match (x, y) with
    | Shape.Atomic x, Shape.Atomic y ->
        let c = G.compare x.alpha y.alpha in
        if c = 0 then G.compare x.beta y.beta else c
    | _, _ -> invalid_arg "Question_nh.compare_ciphertexts"

  let process_ciphertexts _ es =
    let es =
      Array.map
        (fun (w, e) ->
          if Weight.(compare w one) <> 0 then
            invalid_arg "Question_nh.process_ciphertexts"
          else e)
        (Array.of_list es)
    in
    Array.fast_sort compare_ciphertexts es;
    Shape.Array es

  let compute_result ~total_weight:_ q x =
    match x with
    | Shape.Array xs ->
        xs
        |> Array.map (function
          | Shape.Atomic x -> G.to_ints (Array.length q.answers) x
          | _ -> invalid_arg "Question_nh.compute_result/1")
    | _ -> invalid_arg "Question_nh.compute_result/2"

  let check_result ~total_weight q x r = r = compute_result ~total_weight q x
end
