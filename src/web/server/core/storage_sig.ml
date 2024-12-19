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
open Serializable_t

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
  | Public_archive
  | Passwords
  | Records
  | Voters
  | Confidential_archive
  | Extended_record of string
  | Credential_mapping of string
  | Data of hash
  | Roots
  | Voters_config
  | Voter of string
  | Credential_weight of string
  | Credential_user of string
  | Password of string

type admin_password_file = Username of string | Address of string

type file =
  | Spool_version
  | Account_counter (* obsolete as of 3.0 *)
  | Account of int
  | Election of uuid * election_file
  | Auth_db of string
  | Admin_password of string * admin_password_file

type append_operation = Data of string | Event of event_type * hash option

module type BACKEND_GENERIC = sig
  val get_as_file : file -> string Lwt.t
  val get : file -> string option Lwt.t
  val update : file -> (string * (string -> unit Lwt.t)) option Lwt.t
  val create : file -> string -> unit Lwt.t
  val ensure : file -> string -> unit Lwt.t
  val del : file -> unit Lwt.t
end

module type BACKEND_ARCHIVE = sig
  val append : uuid -> ?last:last_event -> append_operation list -> bool Lwt.t
end

module type BACKEND_ELECTIONS = sig
  val new_election : unit -> uuid option Lwt.t
  val init_credential_mapping : uuid -> public_credentials Lwt.t
  val delete_election : uuid -> unit Lwt.t
  val delete_sensitive_data : uuid -> unit Lwt.t
  val delete_live_data : uuid -> unit Lwt.t
end

module type BACKEND_ACCOUNTS = sig
  val new_account_id : unit -> (int * unit Lwt.u) option Lwt.t
end

module type BACKEND = sig
  include BACKEND_GENERIC
  include BACKEND_ACCOUNTS
  include BACKEND_ELECTIONS
  include BACKEND_ARCHIVE
end

type t = (module BACKEND)
type 'a u = t -> uuid -> 'a

module type S = sig
  val with_transaction : (t -> 'a Lwt.t) -> 'a Lwt.t
  val get_user_id : user -> int option Lwt.t

  val get_elections_by_owner :
    int -> Belenios_api.Serializable_t.summary_list Lwt.t

  val get_next_actions :
    unit -> ([> `Archive | `Delete | `Destroy ] * uuid * datetime) list Lwt.t
end
