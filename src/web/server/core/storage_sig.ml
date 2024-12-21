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

type 'a v = 'a Types.Lopt.t

type _ election_file =
  | State : election_state election_file
  | Dates_full : Belenios_storage_api.election_dates election_file
  | Decryption_tokens : decryption_tokens election_file
  | Metadata : metadata election_file
  | Private_key : Yojson.Safe.t election_file
  | Private_keys : string list election_file
  | Skipped_shufflers : skipped_shufflers election_file
  | Shuffle_token : shuffle_token election_file
  | Audit_cache : audit_cache election_file
  | Last_event : last_event election_file
  | Deleted : deleted_election election_file
  | Private_creds_downloaded : string election_file
  | Draft : Core.draft_election election_file
  | Public_creds : public_credentials election_file
  | Private_creds : private_credentials election_file
  | Public_archive : string election_file
  | Passwords : string election_file
  | Records_new : Belenios_storage_api.election_records election_file
  | Voters : Voter.t list election_file
  | Confidential_archive : string election_file
  | Extended_record : string -> extended_record election_file
  | Credential_mapping : string -> string election_file
  | Data : hash -> string election_file
  | Roots : roots election_file
  | Voters_config : voters_config election_file
  | Voter : string -> Voter.t election_file
  | Credential_weight : string -> Weight.t election_file
  | Credential_user : string -> string election_file
  | Password : string -> password_record election_file

type admin_password_file = Username of string | Address of string

type _ file =
  | Spool_version : int file
  | Account_counter : int file (* obsolete as of 3.0 *)
  | Account : int -> account file
  | Election : uuid * 'a election_file -> 'a file
  | Auth_db : string -> string file
  | Admin_password : string * admin_password_file -> password_record file

type append_operation = Data of string | Event of event_type * hash option

module type BACKEND_GENERIC = sig
  val get_as_file : 'a file -> string Lwt.t
  val get : 'a file -> 'a v Lwt.t
  val update : 'a file -> ('a v * ('a v -> unit Lwt.t)) option Lwt.t
  val create : 'a file -> 'a v -> unit Lwt.t
  val ensure : 'a file -> 'a v -> unit Lwt.t
  val del : 'a file -> unit Lwt.t
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
    unit -> ([> `Archive | `Delete | `Destroy ] * uuid * float) list Lwt.t
end
