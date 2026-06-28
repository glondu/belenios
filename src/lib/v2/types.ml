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

open Ppx_yojson_conv_lib.Yojson_conv
open Belenios_core

type question = Question.t [@@deriving yojson]

type 'a params = {
  version : int;
  description : string;
  name : string;
  group : string;
  public_key : 'a;
  questions : question array;
  uuid : uuid;
  administrator : string option; [@yojson.option]
  credential_authority : string option; [@yojson.option]
  language : (string * lang_dir) option; [@yojson.option]
}
[@@deriving yojson]
(** Election parameters relevant for creating a ballot. *)

type ('a, 'b) raw_ballot = {
  election_uuid : uuid;
  election_hash : hash;
  credential : 'a;
  answers : json array;
}
[@@deriving yojson]

type ('a, 'b) ballot = ('a, 'b, ('a, 'b) raw_ballot) signed_msg
[@@deriving yojson]
