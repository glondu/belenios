(**************************************************************************)
(*                                BELENIOS                                *)
(*                                                                        *)
(*  Copyright © 2012-2021 Inria                                           *)
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

open Belenios_platform
open Platform
open Common

type 'a shape = 'a Shape.t =
  | SAtomic of 'a
  | SArray of 'a shape array

module Weight : sig
  type t
  val zero : t
  val one : t
  val is_int : t -> int -> bool
  val ( + ) : t -> t -> t
  val expand : total:t -> t -> Z.t
  val reduce : total:t -> Z.t -> t
  val max_expanded_weight : Z.t
  val min : t -> t -> t
  val max : t -> t -> t
  val of_string : string -> t
  val to_string : t -> string
  val compare : t -> t -> int
end

type weight = Weight.t

val weight_of_json : Yojson.Safe.t -> weight
val json_of_weight : weight -> Yojson.Safe.t

(** Input: [str = "something[,weight]"]
    Output:
    - if [weight] is an integer > 0, return [(something, weight)]
    - else, return [(str, 1)] *)
val extract_weight : string -> string * Weight.t

val split_identity : string -> string * string * Weight.t
val split_identity_opt : string -> string * string option * Weight.t option

type question_result =
  | RHomomorphic of weight array
  | RNonHomomorphic of int array array

val json_of_question_result : question_result -> Yojson.Safe.t
