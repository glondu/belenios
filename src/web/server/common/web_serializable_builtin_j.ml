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

open Belenios_core.Serializable_core_j
open Web_serializable_builtin_t

(** {1 Serializers for type datetime} *)

let write_datetime buf n =
  Buffer.add_char buf '"';
  Buffer.add_string buf (raw_string_of_datetime n);
  Buffer.add_char buf '"'

let datetime_of_json = function
  | `String s -> raw_datetime_of_string s
  | _ -> invalid_arg "datetime_of_json: a string was expected"

let read_datetime state buf =
  datetime_of_json (Yojson.Safe.from_lexbuf ~stream:true state buf)

(** {1 Serializers for type user_or_id} *)

let write_user_or_id write_user buf = function
  | `Id i -> write_int_list buf i
  | `User u -> write_user buf u

let user_or_id_of_json read_user = function
  | `Int i -> `Id [i]
  | `List _ as x -> `Id (int_list_of_string (Yojson.Safe.to_string x))
  | `Assoc _ as x -> `User (unboxed_of_string read_user (Yojson.Safe.to_string x))
  | _ -> invalid_arg "user_or_id_of_json"

let read_user_or_id read_user state buf =
  user_or_id_of_json read_user (Yojson.Safe.from_lexbuf ~stream:true state buf)
