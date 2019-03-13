(**************************************************************************)
(*                                BELENIOS                                *)
(*                                                                        *)
(*  Copyright © 2012-2019 Inria                                           *)
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

open Platform
open Signatures_core
open Serializable_builtin_t
open Serializable_core_t
open Question_open_t

let check_modulo p x = Z.(geq x zero && lt x p)

module type S = sig
  type elt
  type 'a m

  val create_answer : question -> public_key:elt -> prefix:string -> int array -> elt answer m
  val verify_answer : question -> public_key:elt -> prefix:string -> elt answer -> bool

  val extract_ciphertexts : elt answer -> elt ciphertext shape

  val compute_result : question -> elt shape -> int shape
  val check_result : question -> elt shape -> int shape -> bool
end

module Make (M : RANDOM) (G : GROUP) = struct
  type elt = G.t
  type 'a m = 'a M.t
  let ( >>= ) = M.bind
  open G

  let create_answer q ~public_key:y ~prefix m =
    assert (Array.length q.q_answers = Array.length m);
    M.random G.q >>= fun r ->
    let alpha = g **~ r and beta = (y **~ r) *~ (G.of_ints m) in
    M.random G.q >>= fun w ->
    let commitment = g **~ w in
    let zkp = Printf.sprintf "raweg|%s|%s,%s,%s|" prefix (G.to_string y) (G.to_string alpha) (G.to_string beta) in
    let challenge = G.hash zkp [| commitment |] in
    let response = Z.(erem (w - r * challenge) G.q) in
    let proof = {challenge; response} in
    let choices = {alpha; beta} in
    M.return {choices; proof}

  let verify_answer _ ~public_key:y ~prefix a =
    let {alpha; beta} = a.choices in
    let {challenge; response} = a.proof in
    G.check alpha && G.check beta &&
    check_modulo G.q challenge && check_modulo G.q response &&
    let commitment = (g **~ response) *~ (alpha **~ challenge) in
    let zkp = Printf.sprintf "raweg|%s|%s,%s,%s|" prefix (G.to_string y) (G.to_string alpha) (G.to_string beta) in
    Z.(challenge =% G.hash zkp [| commitment |])

  let extract_ciphertexts a =
    SAtomic a.choices

  let compute_result q x =
    let n = Array.length q.q_answers in
    let rec aux = function
      | SAtomic x -> SArray (Array.map (fun x -> SAtomic x) (G.to_ints n x))
      | SArray xs -> SArray (Array.map aux xs)
    in aux x

  let check_result q x r =
    r = compute_result q x
end
