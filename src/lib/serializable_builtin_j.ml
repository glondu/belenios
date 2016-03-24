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

let write_uuid = make_write Uuidm.to_string

let raw_uuid_of_string x =
  match Uuidm.of_string x with
  | Some s -> s
  | _ -> invalid_arg "uuid_of_string: invalid UUID"

let read_uuid = make_read "read_uuid" raw_uuid_of_string

(** {1 Serializers for type int_or_null} *)

let write_int_or_null buf = function
  | Some n -> Bi_outbuf.add_string buf (string_of_int n)
  | None -> Bi_outbuf.add_string buf "null"

let int_or_null_of_json = function
  | `Int i -> Some i
  | `Null -> None
  | _ -> invalid_arg "int_or_null_of_json: unexpected input"

let read_int_or_null state buf =
  int_or_null_of_json (Yojson.Safe.from_lexbuf ~stream:true state buf)

(** {1 Serializers for type string_set} *)

let write_string_set buf set =
  `List (SSet.elements set |> List.map (fun x -> `String x)) |>
  Yojson.Safe.to_outbuf buf

let string_set_of_json = function
  | `List xs ->
    List.fold_left (fun accu x ->
      match x with
      | `String y -> SSet.add y accu
      | _ -> invalid_arg "string_set_of_json: a string was expected"
    ) SSet.empty xs
  | _ -> invalid_arg "string_set_of_json: a list was expected"

let read_string_set state buf =
  Yojson.Safe.from_lexbuf ~stream:true state buf |> string_set_of_json
