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

open Serializable_builtin_t

(** {1 Serializers for type shape} *)

let rec write_shape write buf = function
  | `Atomic x -> write buf x
  | `Array xs -> Atdgen_runtime.Oj_run.write_array (write_shape write) buf xs

let rec read_shape read state buf =
  Yojson.Safe.read_space state buf;
  let open Lexing in
  if buf.lex_curr_pos >= buf.lex_buffer_len then buf.refill_buff buf;
  if buf.lex_curr_pos >= buf.lex_buffer_len then Yojson.json_error "Unexpected end of input";
  if Bytes.get buf.lex_buffer buf.lex_curr_pos = '[' then
    `Array (Yojson.Safe.read_array (read_shape read) state buf)
  else
    `Atomic (read state buf)

(** {1 Serializers for type question_result} *)

let write_question_result buf x =
  Yojson.Safe.write_t buf (json_of_question_result x)

let read_question_result _ _ =
  (* should not be called *)
  failwith "read_question_result"
