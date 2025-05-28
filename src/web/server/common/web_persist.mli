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
open Belenios_storage_api
open Belenios_server_core
open Web_common

(** {1 Dynamically updated election data} *)

val get_election_state : election_state Lwt.t Storage.u

val update_election_state :
  (election_state updatable, 'r) with_lwt_cont Storage.u

(** {1 Typed election data from storage} *)

val get_election_dates : Belenios_storage_api.election_dates Lwt.t Storage.u

val update_election_dates :
  (Belenios_storage_api.election_dates updatable, 'r) with_lwt_cont Storage.u

val get_election_metadata : metadata Lwt.t Storage.u
val seal_election : (bool -> unit Lwt.t) Storage.u
val get_audit_cache : audit_cache option Lwt.t Storage.u

val get_election_automatic_dates :
  Belenios_web_api.election_auto_dates Lwt.t Storage.u

val set_election_automatic_dates :
  (Belenios_web_api.election_auto_dates -> unit Lwt.t) Storage.u

(** {1 Voter-specific stuff} *)

val get_all_voters : Voter.t list Lwt.t Storage.u
val get_draft_public_credentials : string option Lwt.t Storage.u
val get_records : Belenios_storage_api.election_records option Lwt.t Storage.u
val get_voter : (string -> Voter.t option Lwt.t) Storage.u

val check_password :
  (user:string -> password:string -> (string * string option) option Lwt.t)
  Storage.u

val regen_password : (admin_id:int -> string -> bool Lwt.t) Storage.u

(** {1 Derived election data} *)

val get_has_explicit_weights : bool Lwt.t Storage.u
val get_username_or_address : [ `Username | `Address ] Lwt.t Storage.u

(** {1 Election actions} *)

val validate_election :
  admin_id:int ->
  (draft_election updatable -> Belenios_web_api.draft_status -> unit Lwt.t)
  Storage.u

type precast_data = {
  credential : string;
  credential_record : credential_record;
}

val precast_ballot :
  (ballot:string -> (precast_data, cast_error) result Lwt.t) Storage.u

val cast_ballot :
  (ballot:string ->
  user:string ->
  weight:Weight.t ->
  float ->
  precast_data:precast_data ->
  (hash * bool, cast_error) result Lwt.t)
  Storage.u

val append_to_shuffles :
  Storage.t ->
  (module Site_common_sig.ELECTION) ->
  int ->
  string ->
  string option Lwt.t

val add_partial_decryption : (int * string -> unit Lwt.t) Storage.u
val release_tally : unit Lwt.t Storage.u
val create_draft : (draft_election -> unit Lwt.t) Storage.u
val compute_encrypted_tally : bool Lwt.t Storage.u
val finish_shuffling : bool Lwt.t Storage.u
val open_election : bool Lwt.t Storage.u
val close_election : bool Lwt.t Storage.u

(** {1 Misc} *)

type credentials_status = [ `None | `Pending of int | `Done ]

val generate_credentials_on_server_async : uuid -> draft_election -> unit
val get_credentials_status : uuid -> draft_election -> credentials_status
