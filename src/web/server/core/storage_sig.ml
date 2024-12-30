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
open Belenios_storage_api
open Serializable_t

(** {1 Type definitions} *)

type 'a v = 'a Types.Lopt.t
type append_operation = Data of string | Event of event_type * hash option

type (_, _) string_or_value_spec =
  | String : ('a, string) string_or_value_spec
  | Value : ('a, 'a) string_or_value_spec

module type BACKEND_GENERIC = sig
  val get_unixfilename : 'a file -> string Lwt.t
  val get : 'a file -> 'a v Lwt.t
  val set : 'a file -> ('a, 'b) string_or_value_spec -> 'b -> unit Lwt.t
  val del : 'a file -> unit Lwt.t

  val update :
    'a file ->
    ('a v * (('a, 'b) string_or_value_spec -> 'b -> unit Lwt.t) -> 'r Lwt.t) ->
    'r Lwt.t
end

module type BACKEND_ARCHIVE = sig
  val append : uuid -> ?last:last_event -> append_operation list -> bool Lwt.t
end

module type BACKEND_ELECTIONS = sig
  val new_election : unit -> uuid option Lwt.t
  val init_credential_mapping : uuid -> public_credentials Lwt.t
  val archive_election : uuid -> unit Lwt.t
  val delete_election : uuid -> unit Lwt.t

  val validate_election :
    uuid -> (unit, Belenios_web_api.validation_error) result Lwt.t
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
  val get_elections_by_owner : int -> Belenios_web_api.summary_list Lwt.t
end
