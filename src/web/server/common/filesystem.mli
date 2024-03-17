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

open Web_serializable_t

type election_file =
  | Draft
  | State
  | Public_creds
  | Private_creds
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
  | Salts
  | Extended_records
  | Credential_mappings
  | Partial_decryptions
  | Ballots_index
  | Deleted
  | Public_archive
  | Passwords
  | Records
  | Voters
  | Confidential_archive
  | Private_creds_downloaded

type t =
  | Spool_version
  | Account_counter
  | Account of int
  | Election of uuid * election_file
  | Absolute of string

val get_path : t -> string
val files_of_directory : string -> string list Lwt.t
val file_exists : t -> bool Lwt.t
val read_file : t -> string list option Lwt.t
val read_whole_file : t -> string option Lwt.t
val read_whole_file_i18n : lang:string -> string -> string option Lwt.t
val read_file_single_line : t -> string option Lwt.t
val write_file : t -> string list -> unit Lwt.t
val write_whole_file : t -> string -> unit Lwt.t
val mk_election_dir : uuid -> unit Lwt.t
val create_file : t -> ('a -> string) -> 'a list -> unit Lwt.t
val create_whole_file : t -> string -> unit Lwt.t
val append_to_file : t -> string list -> unit Lwt.t
val cleanup_file : t -> unit Lwt.t
val rmdir : string -> unit Lwt.t
val exhaust_file : Ocsigen_multipart.file_info -> string Lwt.t
val get_archive : uuid -> string option Lwt.t
