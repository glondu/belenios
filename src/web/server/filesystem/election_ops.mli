(**************************************************************************)
(*                                BELENIOS                                *)
(*                                                                        *)
(*  Copyright © 2024-2025 Inria                                           *)
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
open Serializable_t

module type BACKEND = sig
  val get_unixfilename : 'a file -> string Lwt.t
  val get : 'a file -> 'a lopt Lwt.t
  val set : 'a file -> ('a, 'b) string_or_value_spec -> 'b -> unit Lwt.t
  val del : 'a file -> unit Lwt.t

  val update :
    'a file ->
    ('a lopt * (('a, 'b) string_or_value_spec -> 'b -> unit Lwt.t) -> 'r Lwt.t) ->
    'r Lwt.t

  val append : uuid -> ?last:last_event -> append_operation list -> bool Lwt.t
  val append_sealing : uuid -> sealing_event -> bool Lwt.t
  val new_election : unit -> uuid option Lwt.t
  val delete_sensitive_data : uuid -> unit Lwt.t
  val delete_live_data : uuid -> unit Lwt.t
  val write_deleted_file : uuid -> deleted_election -> unit Lwt.t
  val delete_draft_election : uuid -> unit Lwt.t
  val init_credential_mapping : uuid -> public_credentials Lwt.t
end

val delete_live_election : (module BACKEND) -> uuid -> roots -> unit Lwt.t
val delete_election : (module BACKEND) -> uuid -> unit Lwt.t
val archive_election : (module BACKEND) -> uuid -> unit Lwt.t

val validate_election :
  (module BACKEND) ->
  uuid ->
  (unit, Belenios_web_api.validation_error) result Lwt.t
