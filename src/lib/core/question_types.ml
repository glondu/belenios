(**************************************************************************)
(*                                BELENIOS                                *)
(*                                                                        *)
(*  Copyright © 2023-2024 Inria                                           *)
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
open Signatures_core

type ('a, 'b) generic_question = {
  type_ : 'a; [@key "type"]
  value : 'b;
  extra : json option; [@yojson.option]
}
[@@deriving yojson]

type _ id = ..

module type QUESTION = sig
  type question [@@deriving yojson]
  type _ id += Id : question id

  val type_ : string
  val erase : question -> question

  val check :
    (module GROUP) Lazy.t ->
    extra:json option ->
    question ->
    (unit, question_error) result
end

type 'a question_module = (module QUESTION with type question = 'a)

type wrapped_question =
  | Q : ('a question_module, 'a) generic_question -> wrapped_question
