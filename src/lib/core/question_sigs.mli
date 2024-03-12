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

open Signatures
open Common
open Serializable_core_t

module type QUESTION = sig
  type question
  type answer
  type element
  type result

  val read_answer : answer reader
  val write_answer : answer writer

  val create_answer :
    question -> public_key:element -> prefix:string -> int Shape.t -> answer

  val verify_answer :
    question -> public_key:element -> prefix:string -> answer -> bool

  val extract_ciphertexts : question -> answer -> element ciphertext Shape.t

  val process_ciphertexts :
    question ->
    (Weight.t * element ciphertext Shape.t) list ->
    element ciphertext Shape.t

  val compute_result :
    total_weight:Weight.t -> question -> element Shape.t -> result

  val check_result :
    total_weight:Weight.t -> question -> element Shape.t -> result -> bool
end
