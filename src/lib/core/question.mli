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

open Signatures_core

type t =
  | Homomorphic of Question_h_t.question
  | NonHomomorphic of Question_nh_t.question * Yojson.Safe.t option

val compute_signature : t list -> (module QUESTION_SIGNATURE_PACK)
val wrap : Yojson.Safe.t -> t
val unwrap : t -> Yojson.Safe.t

type counting_method =
  [ `None
  | `MajorityJudgment of Question_nh_t.mj_extra
  | `Schulze of Question_nh_t.schulze_extra
  | `STV of Question_nh_t.stv_extra ]

val get_counting_method : Yojson.Safe.t option -> counting_method
val erase_question : t -> t

module Make
    (M : RANDOM)
    (G : GROUP)
    (QHomomorphic : Question_sigs.QUESTION_H
                      with type elt := G.t
                       and type question := Question_h_t.question
                       and type answer := G.t Question_h_t.answer)
    (QNonHomomorphic : Question_sigs.QUESTION_NH
                         with type elt := G.t
                          and type question := Question_nh_t.question
                          and type answer := G.t Question_nh_t.answer) :
  Question_sigs.QUESTION
    with type elt := G.t
     and type question := t
     and type answer := Yojson.Safe.t
