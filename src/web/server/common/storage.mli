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
open Web_serializable_t

(** {1 Type definitions} *)

type election_file =
  | State
  | Hide_result
  | Dates
  | Decryption_tokens
  | Metadata
  | Private_key
  | Private_keys
  | Skipped_shufflers
  | Shuffle_token
  | Audit_cache
  | Last_event
  | Deleted
  | Private_creds_downloaded
  | Draft
  | Public_creds
  | Private_creds
  | Salts (* obsolete as of 2.4 *)
  | Public_archive
  | Passwords
  | Records
  | Voters
  | Confidential_archive
  | Extended_record of string
  | Credential_mapping of string

type t =
  | Spool_version
  | Account_counter (* obsolete as of 2.6 *)
  | Account of int
  | Election of uuid * election_file
  | Auth_db of string

(** {1 Generic operations} *)

val get_as_file : t -> string Lwt.t
val get : t -> string option Lwt.t
val set : t -> string -> unit Lwt.t
val del : t -> unit Lwt.t

(** {1 Global operations} *)

val list_accounts : unit -> int list Lwt.t
val list_elections : unit -> uuid list Lwt.t
val new_election : unit -> uuid option Lwt.t
val cleanup_election : uuid -> unit Lwt.t
val new_account_id : unit -> (int * unit Lwt.u) option Lwt.t

(** {1 Specialized operations} *)

val init_credential_mapping : uuid -> public_credentials Lwt.t

(** {1 Cleaning operations} *)

val delete_sensitive_data : uuid -> unit Lwt.t
val delete_live_data : uuid -> unit Lwt.t

(** {1 Public archive operations} *)

val get_data : uuid -> hash -> string option Lwt.t
val get_event : uuid -> hash -> event option Lwt.t
val get_roots : uuid -> roots Lwt.t

type append_operation = Data of string | Event of event_type * hash option

exception RaceCondition

val append :
  ?lock:bool -> uuid -> ?last:last_event -> append_operation list -> unit Lwt.t
