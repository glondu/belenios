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

open Belenios_core
open Serializable_builtin_t
open Serializable_t
open Common
open Web_serializable_t

val get_draft_election : uuid -> draft_election option Lwt.t
val set_draft_election : uuid -> draft_election -> unit Lwt.t

val get_election_state : uuid -> election_state Lwt.t
val set_election_state : uuid -> election_state -> unit Lwt.t

val get_election_dates : uuid -> election_dates Lwt.t
val set_election_dates : uuid -> election_dates -> unit Lwt.t

val get_partial_decryptions : uuid -> partial_decryptions Lwt.t
val set_partial_decryptions : uuid -> partial_decryptions -> unit Lwt.t

val get_decryption_tokens : uuid -> decryption_tokens option Lwt.t
val set_decryption_tokens : uuid -> decryption_tokens -> unit Lwt.t

val get_raw_election : uuid -> string option Lwt.t
val get_election_metadata : uuid -> metadata Lwt.t
val get_election_result : uuid -> string option Lwt.t

val get_election_result_hidden : uuid -> datetime option Lwt.t
val set_election_result_hidden : uuid -> datetime option -> unit Lwt.t

type election_kind =
  [ `Draft
  | `Validated
  | `Tallied
  | `Archived
  ]
val get_elections_by_owner : int -> (election_kind * uuid * datetime * string) list Lwt.t
val clear_elections_by_owner_cache : unit -> unit Lwt.t

val get_voters : uuid -> string list option Lwt.t
val get_passwords : uuid -> (string * string) SMap.t option Lwt.t
val get_private_key : uuid -> number option Lwt.t
val get_private_keys : uuid -> string list option Lwt.t
val get_trustees : uuid -> string Lwt.t
val convert_trustees : unit -> unit Lwt.t

val get_ballot_hashes : uuid -> (string * Weight.t) list Lwt.t
val get_ballot_by_hash : uuid -> string -> string option Lwt.t
val get_ballot_weight : (module Site_common_sig.ELECTION_LWT) -> string -> Weight.t Lwt.t

val compute_encrypted_tally : (module Site_common_sig.ELECTION_LWT) -> string Lwt.t

val get_shuffles : uuid -> string list option Lwt.t
val get_shuffle_hashes : uuid -> shuffle_hash list option Lwt.t
val add_shuffle_hash : uuid -> shuffle_hash -> unit Lwt.t
val compute_encrypted_tally_after_shuffling : (module Site_common_sig.ELECTION_LWT) -> string option Lwt.t

val get_shuffle_token : uuid -> shuffle_token option Lwt.t
val gen_shuffle_token : uuid -> string -> shuffler -> shuffle_token Lwt.t
val clear_shuffle_token : uuid -> unit Lwt.t

val get_nh_ciphertexts : (module Site_common_sig.ELECTION_LWT) -> string Lwt.t

val append_to_shuffles : (module Site_common_sig.ELECTION_LWT) -> string -> string option Lwt.t

val has_voted : uuid -> user -> bool Lwt.t

val init_credential_mapping : uuid -> string list -> unit Lwt.t

val cast_ballot :
  (module Site_common_sig.ELECTION_LWT) ->
  rawballot:string -> user:string -> weight:Weight.t -> datetime ->
  (string * bool, Signatures.cast_error) result Lwt.t

val get_audit_cache : uuid -> audit_cache Lwt.t
val remove_audit_cache : uuid -> unit Lwt.t
