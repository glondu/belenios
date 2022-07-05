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

open Serializable_builtin_t
open Serializable_core_t

module type QUESTION = sig
  type question
  type answer
  type elt
  type 'a m

  val create_answer : question -> public_key:elt -> prefix:string -> int array -> answer m
  val verify_answer : question -> public_key:elt -> prefix:string -> answer -> bool

  val extract_ciphertexts : question -> answer -> elt ciphertext shape
  val process_ciphertexts : question -> (Weight.t * elt ciphertext shape) list -> elt ciphertext shape

  val compute_result : num_tallied:Weight.t -> question -> elt shape -> question_result
  val check_result : num_tallied:Weight.t -> question -> elt shape -> question_result -> bool
end
