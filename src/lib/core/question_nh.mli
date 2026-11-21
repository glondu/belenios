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

open Common_types
open Crypto_types

(** {2 Questions and answers} *)

type question = { answers : string array; question : string } [@@deriving desc]

type ('a, 'b) answer = { choices : 'a ciphertext; proof : 'b proof }
[@@deriving desc]
(** An answer to a question. *)

type result = int array array [@@deriving desc]

(** {2 Counting methods} *)

type mj_extra = { blank : bool; grades : string array } [@@deriving desc]
type schulze_extra = { blank : bool } [@@deriving desc]
type stv_extra = { blank : bool; seats : int } [@@deriving desc]

include
  Question_types.QUESTION_TYPE
    with type question := question
     and type ('a, 'b) answer := ('a, 'b) answer
     and type result := result

type counting_method =
  [ `None
  | `MajorityJudgment of mj_extra
  | `Schulze of schulze_extra
  | `STV of stv_extra ]

val get_counting_method : json option -> counting_method
