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

val get_election_state : Storage.E.t -> election_state Lwt.t

val update_election_state :
  Storage.E.t -> (election_state updatable, 'r) with_lwt_cont

(** {1 Typed election data from storage} *)

val get_election_dates :
  Storage.E.t -> Belenios_storage_api.election_dates Lwt.t

val update_election_dates :
  Storage.E.t ->
  (Belenios_storage_api.election_dates updatable, 'r) with_lwt_cont

val get_election_metadata : Storage.E.t -> metadata Lwt.t
val seal_election : Storage.E.t -> bool -> unit Lwt.t
val get_audit_cache : Storage.E.t -> audit_cache option Lwt.t

val get_election_automatic_dates :
  Storage.E.t -> Belenios_web_api.election_auto_dates Lwt.t

val set_election_automatic_dates :
  Storage.E.t -> Belenios_web_api.election_auto_dates -> unit Lwt.t

(** {1 Voter-specific stuff} *)

val get_all_voters : Storage.E.t -> Voter.t list Lwt.t
val get_draft_public_credentials : Storage.E.t -> string option Lwt.t

val get_records :
  Storage.E.t -> Belenios_storage_api.election_records option Lwt.t

val get_voter : Storage.E.t -> string -> Voter.t option Lwt.t

val check_password :
  Storage.E.t ->
  user:string ->
  password:string ->
  (string * string option) option Lwt.t

val regen_password : Storage.E.t -> admin_id:int -> string -> bool Lwt.t

(** {1 Derived election data} *)

val get_has_explicit_weights : Storage.E.t -> bool Lwt.t
val get_username_or_address : Storage.E.t -> [ `Username | `Address ] Lwt.t

(** {1 Election actions} *)

val validate_election :
  admin_id:int ->
  Storage.E.t ->
  draft_election updatable_with_billing ->
  Belenios_web_api.draft_status ->
  unit Lwt.t

type precast_data = {
  credential : string;
  credential_record : credential_record;
}

val precast_ballot :
  Storage.E.t -> ballot:string -> (precast_data, cast_error) result Lwt.t

val cast_ballot :
  Storage.E.t ->
  ballot:string ->
  user:string ->
  weight:Weight.t ->
  float ->
  precast_data:precast_data ->
  (hash * bool, cast_error) result Lwt.t

val append_to_shuffles :
  Storage.E.t ->
  (module Site_common_sig.ELECTION) ->
  int ->
  string ->
  string option Lwt.t

val add_partial_decryption : Storage.E.t -> int * string -> unit Lwt.t
val release_tally : Storage.E.t -> unit Lwt.t
val create_draft : Storage.E.t -> draft_election -> unit Lwt.t
val compute_encrypted_tally : Storage.E.t -> bool Lwt.t
val finish_shuffling : Storage.E.t -> bool Lwt.t
val open_election : Storage.E.t -> bool Lwt.t
val close_election : Storage.E.t -> bool Lwt.t

(** {1 Misc} *)

type credentials_status = [ `None | `Pending of int | `Done ]

val generate_credentials_on_server_async : uuid -> draft_election -> unit
val get_credentials_status : uuid -> draft_election -> credentials_status
