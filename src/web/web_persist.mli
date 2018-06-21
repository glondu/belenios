(**************************************************************************)
(*                                BELENIOS                                *)
(*                                                                        *)
(*  Copyright © 2012-2018 Inria                                           *)
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

open Serializable_t
open Common
open Web_serializable_t

val get_draft_election : uuid -> draft_election option Lwt.t
val set_draft_election : uuid -> draft_election -> unit Lwt.t

val get_election_state : uuid -> election_state Lwt.t
val set_election_state : uuid -> election_state -> unit Lwt.t

type election_date =
  [ `Creation
  | `Validation
  | `Tally
  | `Archive
  | `LastMail
  ]
val get_election_date : election_date -> uuid -> datetime option Lwt.t
val set_election_date : election_date -> uuid -> datetime -> unit Lwt.t

val get_partial_decryptions : uuid -> partial_decryptions Lwt.t
val set_partial_decryptions : uuid -> partial_decryptions -> unit Lwt.t

val get_auth_config : uuid -> (string * (string * string list)) list Lwt.t

val get_raw_election : uuid -> string option Lwt.t
val get_election_metadata : uuid -> metadata Lwt.t
val get_election_result : uuid -> Yojson.Safe.json result option Lwt.t

type election_kind =
  [ `Draft
  | `Validated
  | `Tallied
  | `Archived
  ]
val get_elections_by_owner : user -> (election_kind * uuid * datetime * string) list Lwt.t

val get_voters : uuid -> string list option Lwt.t
val get_passwords : uuid -> (string * string) SMap.t option Lwt.t
val get_public_keys : uuid -> string list option Lwt.t
val get_private_key : uuid -> number option Lwt.t
val get_private_keys : uuid -> string list option Lwt.t
val get_threshold : uuid -> string option Lwt.t

val get_ballot_hashes : uuid -> string list Lwt.t
val get_ballot_by_hash : uuid -> string -> string option Lwt.t

val add_ballot : uuid -> string -> string Lwt.t
val replace_ballot : uuid -> string -> string -> string Lwt.t
val compute_encrypted_tally : uuid -> (int * string * string) Lwt.t

val find_extended_record : uuid -> string -> (datetime * string) option Lwt.t
val add_extended_record : uuid -> string -> datetime * string -> unit Lwt.t

val has_voted : uuid -> user -> bool Lwt.t

val init_credential_mapping : uuid -> string list -> unit Lwt.t
val find_credential_mapping : uuid -> string -> string option Lwt.t
val add_credential_mapping : uuid -> string -> string option -> unit Lwt.t
val replace_credential : uuid -> string -> string -> unit Lwt.t
