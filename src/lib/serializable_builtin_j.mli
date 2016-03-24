(**************************************************************************)
(*                                BELENIOS                                *)
(*                                                                        *)
(*  Copyright © 2012-2014 Inria                                           *)
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

open Serializable_builtin_t

(** {1 Serializers for type number} *)

val write_number : Bi_outbuf.t -> number -> unit
val string_of_number : ?len:int -> number -> string
val read_number : Yojson.Safe.lexer_state -> Lexing.lexbuf -> number
val number_of_string : string -> number

(** {1 Serializers for type uuid} *)

val write_uuid : Bi_outbuf.t -> uuid -> unit
val string_of_uuid : ?len:int -> uuid -> string
val read_uuid : Yojson.Safe.lexer_state -> Lexing.lexbuf -> uuid
val uuid_of_string : string -> uuid

(** {1 Serializers for type int_or_null} *)

val write_int_or_null : Bi_outbuf.t -> int_or_null -> unit
val string_of_int_or_null : ?len:int -> int_or_null -> string
val read_int_or_null : Yojson.Safe.lexer_state -> Lexing.lexbuf -> int_or_null
val int_or_null_of_string : string -> int_or_null

(** {1 Serializers for type string_set} *)

val write_string_set : Bi_outbuf.t -> string_set -> unit
val string_of_string_set : ?len:int -> string_set -> string
val read_string_set :  Yojson.Safe.lexer_state -> Lexing.lexbuf -> string_set
val string_set_of_string : string -> string_set
