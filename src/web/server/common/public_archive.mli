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
open Belenios_server_core

val get_roots : roots Lwt.t Storage.u
val get_data : (hash -> string option Lwt.t) Storage.u
val get_trustees : string Lwt.t Storage.u
val get_election : string option Lwt.t Storage.u
val get_partial_decryptions : string owned list Lwt.t Storage.u
val get_result : string option Lwt.t Storage.u
val get_public_creds : public_credentials Lwt.t Storage.u
val get_ballot_by_hash : (string -> string option Lwt.t) Storage.u
val get_nh_ciphertexts : string Lwt.t Storage.u
val get_shuffles : (hash * hash owned * string) list option Lwt.t Storage.u
val get_sized_encrypted_tally : string option Lwt.t Storage.u
val get_latest_encrypted_tally : string option Lwt.t Storage.u
val get_ballot_hashes : (string * Weight.t) list Lwt.t Storage.u
val clear_ballot_cache : uuid -> unit

val fold_on_ballots :
  ((hash -> string -> 'a -> 'a Lwt.t) -> 'a -> 'a Lwt.t) Storage.u

val with_election :
  (fallback:(unit -> 'a Lwt.t) ->
  ((module Site_common_sig.ELECTION) -> 'a Lwt.t) ->
  'a Lwt.t)
  Storage.u
