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

(** Election primitives *)

open Belenios_core
open Signatures
open Serializable_t

val of_string : string -> params

val election_uuid_of_string_ballot : string -> uuid

val has_nh_questions : params -> bool

val make_raw_election : params -> group:string -> public_key:string -> string

module Make (R : RAW_ELECTION) (M : RANDOM) () : ELECTION with type 'a m = 'a M.t

val compute_checksums :
  election:string ->
  [`Nothing
  | `Shuffles of string list * string option list option
  | `Result of string] ->
  trustees:string -> public_credentials:string ->
  Serializable_t.election_checksums
