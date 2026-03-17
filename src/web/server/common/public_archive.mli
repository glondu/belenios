(**************************************************************************)
(*                                BELENIOS                                *)
(*                                                                        *)
(*  Copyright © 2012-2024 Inria                                           *)
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
open Belenios_storage_api

val get_roots : Storage.E.t -> roots Lwt.t
val get_data : Storage.E.t -> hash -> string option Lwt.t
val get_trustees : Storage.E.t -> string Lwt.t
val get_election : Storage.E.t -> string option Lwt.t
val get_partial_decryptions : Storage.E.t -> string owned list Lwt.t
val get_result : Storage.E.t -> string option Lwt.t
val get_public_creds : Storage.E.t -> public_credentials Lwt.t
val get_ballot_by_hash : Storage.E.t -> string -> string option Lwt.t
val get_nh_ciphertexts : Storage.E.t -> string Lwt.t
val get_shuffles : Storage.E.t -> (hash * hash owned * string) list option Lwt.t
val get_sized_encrypted_tally : Storage.E.t -> string option Lwt.t
val get_latest_encrypted_tally : Storage.E.t -> string option Lwt.t
val get_ballot_hashes : Storage.E.t -> (hash * Weight.t) list Lwt.t
val clear_ballot_cache : uuid -> unit

val fold_on_ballots :
  Storage.E.t -> (hash -> string -> 'a -> 'a Lwt.t) -> 'a -> 'a Lwt.t

val with_election :
  Storage.E.t ->
  fallback:(unit -> 'a Lwt.t) ->
  ((module Site_common_sig.ELECTION) -> 'a Lwt.t) ->
  'a Lwt.t
