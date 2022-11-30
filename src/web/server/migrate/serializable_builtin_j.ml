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

open Belenios_core.Signatures
open Belenios_core.Serializable_core_j

(** {1 Serializers for type user_or_id} *)

let write_user_or_id write_user buf = function
  | `Id i -> write_int_list buf i
  | `User u -> write_user buf u

let user_or_id_of_json read_user = function
  | `Int i -> `Id [i]
  | `List _ as x -> `Id (int_list_of_string (Yojson.Safe.to_string x))
  | `Assoc _ as x -> `User (Json.from_string read_user (Yojson.Safe.to_string x))
  | _ -> invalid_arg "user_or_id_of_json"

let read_user_or_id read_user state buf =
  user_or_id_of_json read_user (Yojson.Safe.from_lexbuf ~stream:true state buf)
