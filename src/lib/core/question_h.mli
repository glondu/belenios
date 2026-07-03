(**************************************************************************)
(*                                BELENIOS                                *)
(*                                                                        *)
(*  Copyright © 2012-2023 Inria                                           *)
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

open Common_types
open Crypto_types

type question = {
  answers : string array;
  blank : bool;
  min : int;
  max : int;
  question : string;
}
[@@deriving yojson]

type ('a, 'b) answer = {
  choices : 'a ciphertext array;
  individual_proofs : 'b disjunctive_proof array;
  overall_proof : 'b disjunctive_proof;
  blank_proof : 'b disjunctive_proof option; [@yojson.option]
}
[@@deriving yojson]
(** An answer to a question. It consists of a weight for each choice, a proof
    that each of these weights is 0 or 1, and an overall proof that the total
    weight is within bounds. *)

type result = weight array [@@deriving yojson]

include
  Question_types.QUESTION_TYPE
    with type question := question
     and type ('a, 'b) answer := ('a, 'b) answer
     and type result := result
