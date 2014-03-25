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

(** {1 Helpers for interacting with atd-generated stuff} *)

let make_write to_string buf x =
  Bi_outbuf.add_char buf '"';
  Bi_outbuf.add_string buf (to_string x);
  Bi_outbuf.add_char buf '"'

let make_read of_string state buf =
  match Yojson.Safe.from_lexbuf ~stream:true state buf with
  | `String s -> of_string s
  | _ -> assert false

(** {1 Serializers for type number} *)

let write_number = make_write Z.to_string

let string_of_number ?(len=2048) n =
  let buf = Bi_outbuf.create len in
  write_number buf n;
  Bi_outbuf.contents buf

let read_number = make_read Z.of_string

let number_of_string x =
  match Yojson.Safe.from_string x with
  | `String s -> Z.of_string s
  | _ -> assert false

(** {1 Serializers for type uuid} *)

let write_uuid = make_write Uuidm.to_string

let string_of_uuid ?(len=38) n =
  let buf = Bi_outbuf.create len in
  write_uuid buf n;
  Bi_outbuf.contents buf

let raw_uuid_of_string x =
  match Uuidm.of_string x with
  | Some s -> s
  | _ -> assert false

let read_uuid = make_read raw_uuid_of_string

let uuid_of_string x =
  match Yojson.Safe.from_string x with
  | `String s -> raw_uuid_of_string s
  | _ -> assert false

(** {1 Serializers for type datetime} *)

open CalendarLib
let datetime_format = "%Y-%m-%d %H:%M:%S"

let write_datetime buf (n, s) =
  Bi_outbuf.add_char buf '"';
  (match s with
     | Some s -> Bi_outbuf.add_string buf s
     | None ->
       let n = Fcalendar.Precise.to_gmt n in
       Bi_outbuf.add_string buf (Printer.Precise_Fcalendar.sprint datetime_format n);
       let ts = Printf.sprintf "%.6f" (Fcalendar.Precise.to_unixfloat n) in
       let i = String.index ts '.' in
       Bi_outbuf.add_substring buf ts i (String.length ts - i);
  );
  Bi_outbuf.add_char buf '"'

let string_of_datetime ?(len=28) n =
  let buf = Bi_outbuf.create len in
  write_datetime buf n;
  Bi_outbuf.contents buf

let datetime_of_json = function
  | `String s ->
    let i = String.index s '.' in
    let l = Printer.Precise_Fcalendar.from_fstring datetime_format (String.sub s 0 i) in
    let l = Fcalendar.Precise.from_gmt l in
    let r = float_of_string ("0" ^ String.sub s i (String.length s-i)) in
    (Fcalendar.Precise.add l (Fcalendar.Precise.Period.second r), Some s)
  | _ -> assert false

let read_datetime state buf =
  datetime_of_json (Yojson.Safe.from_lexbuf ~stream:true state buf)

let datetime_of_string s =
  datetime_of_json (Yojson.Safe.from_string s)

(** {1 Serializers for type int_or_null} *)

let write_int_or_null buf = function
  | Some n -> Bi_outbuf.add_string buf (string_of_int n)
  | None -> Bi_outbuf.add_string buf "null"

let string_of_int_or_null ?(len=4) n =
  let buf = Bi_outbuf.create len in
  write_int_or_null buf n;
  Bi_outbuf.contents buf

let int_or_null_of_json = function
  | `Int i -> Some i
  | `Null -> None
  | _ -> assert false

let read_int_or_null state buf =
  int_or_null_of_json (Yojson.Safe.from_lexbuf ~stream:true state buf)

let int_or_null_of_string s =
  int_or_null_of_json (Yojson.Safe.from_string s)
