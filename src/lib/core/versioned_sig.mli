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

module type GROUP_SIG = sig

  val of_string : string -> (module GROUP)

end

module type QUESTION_H_SIG = sig

  module Make (M : RANDOM) (G : GROUP) : Question_sigs.QUESTION_H
         with type 'a m := 'a M.t
          and type elt := G.t
          and type question := Question_h_t.question
          and type answer := G.t Question_h_t.answer

end

module type QUESTION_NH_SIG = sig

  module Make (M : RANDOM) (G : GROUP) : Question_sigs.QUESTION_NH
         with type 'a m := 'a M.t
          and type elt := G.t
          and type question := Question_nh_t.question
          and type answer := G.t Question_nh_t.answer

end

module type MIXNET_SIG = sig

  module Make (W : ELECTION_DATA) (M : RANDOM) : MIXNET
         with type 'a m := 'a M.t
          and type elt := W.G.t
          and type 'a proof := 'a Serializable_t.shuffle_proof

end

module type ELECTION_SIG = sig

  val of_string : string -> Serializable_t.params
  val to_string : Serializable_t.params -> group:string -> public_key:string -> string

  module Make (MakeResult : MAKE_RESULT) (R : RAW_ELECTION) (M : RANDOM) () : ELECTION
         with type 'a m = 'a M.t

end
