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

open Ppx_yojson_conv_lib.Yojson_conv
open Belenios_core

type 'a generic_question = {
  type_ : string; [@key "type"]
  value : 'a;
  extra : json option; [@yojson.option]
}
[@@deriving yojson]

type raw_question = ..
type question = raw_question generic_question

module type QUESTION = sig
  type t [@@deriving yojson]
  type raw_question += Q of t

  val type_ : string
  val make : value:t -> extra:json option -> question
  val erase : t -> t

  val check :
    (module GROUP) Lazy.t -> t generic_question -> (unit, question_error) result
end

module type PACK = sig
  module Ops : QUESTION

  val it : Ops.t
end
