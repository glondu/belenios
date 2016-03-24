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

open Serializable_builtin_t

(** {1 Serializers for type number} *)

val write_number : Bi_outbuf.t -> number -> unit
val read_number : Yojson.Safe.lexer_state -> Lexing.lexbuf -> number

(** {1 Serializers for type uuid} *)

val write_uuid : Bi_outbuf.t -> uuid -> unit
val read_uuid : Yojson.Safe.lexer_state -> Lexing.lexbuf -> uuid

(** {1 Serializers for type int_or_null} *)

val write_int_or_null : Bi_outbuf.t -> int_or_null -> unit
val read_int_or_null : Yojson.Safe.lexer_state -> Lexing.lexbuf -> int_or_null

(** {1 Serializers for type string_set} *)

val write_string_set : Bi_outbuf.t -> string_set -> unit
val read_string_set :  Yojson.Safe.lexer_state -> Lexing.lexbuf -> string_set
