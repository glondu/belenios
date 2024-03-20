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
open Web_common
open Web_serializable_t

val get_spool_version : unit -> int Lwt.t
val get_draft_election : uuid -> draft_election option Lwt.t
val set_draft_election : uuid -> draft_election -> unit Lwt.t
val release_tally : uuid -> unit Lwt.t
val get_election_state : uuid -> election_state Lwt.t
val get_election_dates : uuid -> election_dates Lwt.t
val get_partial_decryptions : uuid -> string owned list Lwt.t
val add_partial_decryption : uuid -> int * string -> unit Lwt.t
val get_decryption_tokens : uuid -> decryption_tokens option Lwt.t
val set_decryption_tokens : uuid -> decryption_tokens -> unit Lwt.t
val get_raw_election : uuid -> string option Lwt.t
val get_election_metadata : uuid -> metadata Lwt.t
val get_election_result : uuid -> string option Lwt.t
val get_election_result_hidden : uuid -> datetime option Lwt.t
val set_election_result_hidden : uuid -> datetime option -> unit Lwt.t

type election_state =
  [ `Draft
  | `Open
  | `Closed
  | `Shuffling
  | `EncryptedTally
  | `Tallied
  | `Archived ]

val get_elections_by_owner :
  int -> (election_state * uuid * datetime * string) list Lwt.t

val clear_elections_by_owner_cache : unit -> unit Lwt.t
val get_passwords : uuid -> (string * string) SMap.t option Lwt.t
val get_private_keys : uuid -> string list option Lwt.t
val get_trustees : uuid -> string Lwt.t
val get_has_explicit_weights : uuid -> bool Lwt.t
val get_username_or_address : uuid -> [ `Username | `Address ] Lwt.t
val get_voter : uuid -> string -> Voter.t option Lwt.t
val get_all_voters : uuid -> Voter.t list Lwt.t
val get_ballot_hashes : uuid -> (string * Weight.t) list Lwt.t
val get_ballot_by_hash : uuid -> string -> string option Lwt.t
val get_shuffles : uuid -> (hash * hash owned * string) list option Lwt.t
val get_sized_encrypted_tally : uuid -> string option Lwt.t

val get_latest_encrypted_tally :
  (module Site_common_sig.ELECTION) -> string option Lwt.t

val get_shuffle_token : uuid -> shuffle_token option Lwt.t

val gen_shuffle_token :
  uuid -> string -> int -> string option -> shuffle_token Lwt.t

val clear_shuffle_token : uuid -> unit Lwt.t
val get_nh_ciphertexts : (module Site_common_sig.ELECTION) -> string Lwt.t

val append_to_shuffles :
  (module Site_common_sig.ELECTION) -> int -> string -> string option Lwt.t

val precast_ballot :
  (module Site_common_sig.ELECTION) ->
  rawballot:string ->
  (string * credential_record, cast_error) result Lwt.t

val cast_ballot :
  (module Site_common_sig.ELECTION) ->
  rawballot:string ->
  user:string ->
  weight:Weight.t ->
  datetime ->
  precast_data:string * credential_record ->
  (string * bool, cast_error) result Lwt.t

val get_audit_cache : uuid -> audit_cache Lwt.t
val remove_audit_cache : uuid -> unit Lwt.t
val archive_election : uuid -> unit Lwt.t
val delete_election : uuid -> unit Lwt.t

val check_password :
  uuid -> user:string -> password:string -> (string * string) option Lwt.t

val regen_password :
  (module Site_common_sig.ELECTION) -> metadata -> string -> bool Lwt.t

val get_private_creds_filename : uuid -> string
val get_private_creds_downloaded : uuid -> bool Lwt.t
val set_private_creds_downloaded : uuid -> unit Lwt.t
val get_election_file : uuid -> election_file -> string

val validate_election :
  admin_id:int ->
  uuid ->
  draft_election ->
  Belenios_api.Serializable_t.draft_status ->
  unit Lwt.t

val delete_draft : uuid -> unit Lwt.t
val create_draft : uuid -> draft_election -> unit Lwt.t
val compute_encrypted_tally : (module Site_common_sig.ELECTION) -> bool Lwt.t
val finish_shuffling : (module Site_common_sig.ELECTION) -> bool Lwt.t
val get_skipped_shufflers : uuid -> string list option Lwt.t
val set_skipped_shufflers : uuid -> string list -> unit Lwt.t

val get_next_actions :
  unit -> ([> `Archive | `Delete | `Destroy ] * uuid * datetime) list Lwt.t

val open_election : uuid -> bool Lwt.t
val close_election : uuid -> bool Lwt.t

val get_election_automatic_dates :
  uuid -> Belenios_api.Serializable_t.election_auto_dates Lwt.t

val set_election_automatic_dates :
  uuid -> Belenios_api.Serializable_t.election_auto_dates -> unit Lwt.t

val set_draft_public_credentials : uuid -> public_credentials -> unit Lwt.t
val get_draft_public_credentials : uuid -> string option Lwt.t
val get_draft_private_credentials : uuid -> string option Lwt.t
val get_records : uuid -> string list option Lwt.t
val get_voters_file : uuid -> string option Lwt.t
val set_salts : uuid -> salts -> unit Lwt.t
val get_salt : uuid -> int -> Yojson.Safe.t salt option Lwt.t

type credentials_status = [ `None | `Pending of int | `Done ]

val generate_credentials_on_server_async : uuid -> draft_election -> unit
val get_credentials_status : uuid -> draft_election -> credentials_status
val is_group_fixed : uuid -> draft_election -> bool
