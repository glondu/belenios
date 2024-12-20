(**************************************************************************)
(*                                BELENIOS                                *)
(*                                                                        *)
(*  Copyright © 2012-2023 Inria                                           *)
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

open Belenios
open Belenios_server_core

type 'a t

val get : ('a t -> 'a option Lwt.t) Storage.u
val del : ('a t -> unit Lwt.t) Storage.u
val update : ('a t -> 'a Web_common.updatable option Lwt.t) Storage.u
val create : ('a t -> 'a -> unit Lwt.t) Storage.u
val ensure : ('a t -> 'a -> unit Lwt.t) Storage.u

(* draft elections *)
val draft : draft_election t
val draft_public_credentials : public_credentials t

(* sensitive data *)
val state : election_state t
val private_key : Yojson.Safe.t t
val private_keys : string list t
val decryption_tokens : decryption_tokens t

(* other data *)
val last_event : last_event t
val dates_full : Belenios_storage_api.election_dates t
val metadata : metadata t
val audit_cache : audit_cache t
val shuffle_token : shuffle_token t
val skipped_shufflers : skipped_shufflers t
