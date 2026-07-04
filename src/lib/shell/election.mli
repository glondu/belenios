(**************************************************************************)
(*                                BELENIOS                                *)
(*                                                                        *)
(*  Copyright © 2012-2022 Inria                                           *)
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

val get_version : json -> int
val get_uuid : string -> uuid

type 'a version

val compare_version : 'a version -> 'b version -> ('a, 'b) Type.eq option

type some_version = Version : 'a version -> some_version

val int_of_version : 'a version -> int
val version_of_int : int -> some_version

type versioned_template =
  | Template : 'a version * template -> versioned_template
[@@deriving yojson]

val election_uuid_of_string_ballot : string -> uuid
val has_nh_questions : versioned_template -> bool
val get_complexity : versioned_template -> complexity

val make_raw_election :
  version:int ->
  versioned_template ->
  uuid:uuid ->
  group:string ->
  public_key:string ->
  json

type t = (module ELECTION) [@@deriving yojson]
type ('a, 'b) u = (module ELECTION with type G.t = 'a and type G.Zq.t = 'b)

val supported_crypto_versions : some_version list

val compute_checksums :
  ('a, 'b) u ->
  ('a, 'b) trustees ->
  'a public_credentials ->
  shuffles:hash owned list option ->
  encrypted_tally:hash option ->
  final:hash option ->
  election_checksums
