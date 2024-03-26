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

val get_roots : uuid -> roots Lwt.t
val get_data : uuid -> hash -> string option Lwt.t
val get_trustees : uuid -> string Lwt.t
val get_election : uuid -> string option Lwt.t
val get_partial_decryptions : uuid -> string owned list Lwt.t
val get_result : uuid -> string option Lwt.t
val get_public_creds : uuid -> public_credentials Lwt.t
val get_ballot_by_hash : uuid -> string -> string option Lwt.t
val get_nh_ciphertexts : uuid -> string Lwt.t
val get_shuffles : uuid -> (hash * hash owned * string) list option Lwt.t
val get_sized_encrypted_tally : uuid -> string option Lwt.t
val get_ballot_hashes : uuid -> (string * Weight.t) list Lwt.t
val clear_ballot_cache : uuid -> unit

val fold_on_ballots :
  uuid -> (hash -> string -> 'a -> 'a Lwt.t) -> 'a -> 'a Lwt.t

val with_election :
  uuid ->
  fallback:(unit -> 'a Lwt.t) ->
  ((module Site_common_sig.ELECTION) -> 'a Lwt.t) ->
  'a Lwt.t
