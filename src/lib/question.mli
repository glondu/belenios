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

open Signatures_core

type question =
  | Homomorphic of Question_h_t.question
  | NonHomomorphic of Question_nh_t.question

val read_question : Yojson.Safe.lexer_state -> Lexing.lexbuf -> question
val write_question : Bi_outbuf.t -> question -> unit

val erase_question : question -> question

module Make (M : RANDOM) (G : GROUP) : Question_sigs.QUESTION
       with type 'a m := 'a M.t
        and type elt := G.t
        and type question := question
        and type answer := Yojson.Safe.t
