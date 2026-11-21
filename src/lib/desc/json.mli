(**************************************************************************)
(*                                BELENIOS                                *)
(*                                                                        *)
(*  Copyright © 2026-2026 Inria                                           *)
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

(** Canonical JSON interpreter for {!Desc}. *)

val to_json : 'a Core.t -> 'a -> Core.json
(** Deterministic: fields in declaration order, sorted keys for sorted objects,
    optional and default fields left out. *)

exception Error of string list * string
(** Decoding error: path (e.g. [["questions"; "2"; "answers"]]) and message. *)

val error_to_string : string list * string -> string

val of_json : 'a Core.t -> Core.json -> 'a
(** Tolerant: key order does not matter, unknown fields are ignored. Duplicate
    keys are rejected. Raises {!Error}. *)

val to_string : 'a Core.t -> 'a -> string
val of_string : 'a Core.t -> string -> 'a
