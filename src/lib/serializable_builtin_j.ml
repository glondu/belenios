(**************************************************************************)
(*                                BELENIOS                                *)
(*                                                                        *)
(*  Copyright © 2012-2018 Inria                                           *)
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

open Platform
open Serializable_builtin_t

(** {1 Helpers for interacting with atd-generated stuff} *)

let make_write to_string buf x =
  Bi_outbuf.add_char buf '"';
  Bi_outbuf.add_string buf (to_string x);
  Bi_outbuf.add_char buf '"'

let make_read name of_string state buf =
  match Yojson.Safe.from_lexbuf ~stream:true state buf with
  | `String s -> of_string s
  | _ -> invalid_arg (name ^ ": a string was expected")

(** {1 Serializers for type number} *)

let write_number = make_write Z.to_string

let read_number = make_read "read_number" Z.of_string

(** {1 Serializers for type uuid} *)

let write_uuid = make_write raw_string_of_uuid

let read_uuid = make_read "read_uuid" uuid_of_raw_string
