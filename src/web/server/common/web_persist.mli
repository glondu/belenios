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
open Web_common

(** {1 Spool version} *)

val get_spool_version : unit -> int Lwt.t

(** {1 Dynamically updated election data} *)

val get_election_state : election_state Lwt.t Storage.u
val update_election_state : election_state updatable Lwt.t Storage.u

(** {1 Typed election data from storage} *)

val get_election_dates : election_dates Lwt.t Storage.u
val update_election_dates : election_dates updatable Lwt.t Storage.u
val get_election_metadata : metadata Lwt.t Storage.u
val get_election_result_hidden : datetime option Lwt.t Storage.u
val set_election_result_hidden : (datetime option -> unit Lwt.t) Storage.u
val get_private_creds_downloaded : bool Lwt.t Storage.u
val set_private_creds_downloaded : unit Lwt.t Storage.u
val get_audit_cache : audit_cache Lwt.t Storage.u

val get_election_automatic_dates :
  Belenios_api.Serializable_t.election_auto_dates Lwt.t Storage.u

val set_election_automatic_dates :
  (Belenios_api.Serializable_t.election_auto_dates -> unit Lwt.t) Storage.u

(** {1 Voter-specific stuff} *)

val get_all_voters : Voter.t list Lwt.t Storage.u
val get_draft_public_credentials : string option Lwt.t Storage.u
val get_records : string list option Lwt.t Storage.u
val get_salt : (int -> Yojson.Safe.t salt option Lwt.t) Storage.u
val get_voter : (string -> Voter.t option Lwt.t) Storage.u

val check_password :
  (user:string -> password:string -> (string * string option) option Lwt.t)
  Storage.u

val regen_password : (metadata -> string -> bool Lwt.t) Storage.u

(** {1 Derived election data} *)

val get_has_explicit_weights : bool Lwt.t Storage.u
val get_username_or_address : [ `Username | `Address ] Lwt.t Storage.u
val is_group_fixed : uuid -> draft_election -> bool

(** {1 Tokens} *)

val gen_shuffle_token :
  (string -> int -> string option -> shuffle_token Lwt.t) Storage.u

val get_shuffle_token : shuffle_token option Lwt.t Storage.u

(** {1 Election actions} *)

val validate_election :
  admin_id:int ->
  (draft_election updatable ->
  Belenios_api.Serializable_t.draft_status ->
  unit Lwt.t)
  Storage.u

val precast_ballot :
  (rawballot:string -> (string * credential_record, cast_error) result Lwt.t)
  Storage.u

val cast_ballot :
  (rawballot:string ->
  user:string ->
  weight:Weight.t ->
  datetime ->
  precast_data:string * credential_record ->
  (string * bool, cast_error) result Lwt.t)
  Storage.u

val append_to_shuffles :
  Storage.t ->
  (module Site_common_sig.ELECTION) ->
  int ->
  string ->
  string option Lwt.t

val add_partial_decryption : (int * string -> unit Lwt.t) Storage.u
val release_tally : unit Lwt.t Storage.u
val archive_election : unit Lwt.t Storage.u
val delete_election : unit Lwt.t Storage.u
val delete_draft : unit Lwt.t Storage.u
val create_draft : (draft_election -> unit Lwt.t) Storage.u
val compute_encrypted_tally : bool Lwt.t Storage.u
val finish_shuffling : bool Lwt.t Storage.u
val open_election : bool Lwt.t Storage.u
val close_election : bool Lwt.t Storage.u

(** {1 Misc} *)

val get_next_actions :
  unit -> ([> `Archive | `Delete | `Destroy ] * uuid * datetime) list Lwt.t

type credentials_status = [ `None | `Pending of int | `Done ]

val generate_credentials_on_server_async : uuid -> draft_election -> unit
val get_credentials_status : uuid -> draft_election -> credentials_status
