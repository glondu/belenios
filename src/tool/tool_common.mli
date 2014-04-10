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

exception Cmdline_error of string

val failcmd : ('a, unit, string, 'b) format4 -> 'a

val common_man : [> `Noblank | `P of string | `S of string ] list
val get_mandatory_opt : string -> 'a option -> 'a
val wrap_main : (unit -> unit) -> [> `Error of bool * string | `Ok of unit ]

val group_t : (string * (module Signatures.GROUP)) option Cmdliner.Term.t
val uuid_t : Uuidm.t option Cmdliner.Term.t
