(**************************************************************************)
(*                                BELENIOS                                *)
(*                                                                        *)
(*  Copyright © 2012-2016 Inria                                           *)
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

open Web_serializable_builtin_t

(** {1 Serializers for type datetime} *)

let write_datetime buf n =
  Bi_outbuf.add_char buf '"';
  Bi_outbuf.add_string buf (raw_string_of_datetime n);
  Bi_outbuf.add_char buf '"'

let datetime_of_json = function
  | `String s -> raw_datetime_of_string s
  | _ -> invalid_arg "datetime_of_json: a string was expected"

let read_datetime state buf =
  datetime_of_json (Yojson.Safe.from_lexbuf ~stream:true state buf)
