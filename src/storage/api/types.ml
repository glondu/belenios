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

type 'a election_file = 'a File.u
type 'a file = 'a File.t
type admin_password_file = File.kind
type 'a lopt = 'a Lopt.t
type append_operation = Data of string | Event of event_type * hash option

type (_, _) string_or_value_spec =
  | String : ('a, string) string_or_value_spec
  | Value : ('a, 'a) string_or_value_spec

module type BACKEND_GENERIC = sig
  type t

  val get_unixfilename : t -> 'a file -> string Lwt.t
  val get : t -> 'a file -> 'a lopt Lwt.t
  val set : t -> 'a file -> ('a, 'b) string_or_value_spec -> 'b -> unit Lwt.t
  val del : t -> 'a file -> unit Lwt.t

  val update :
    t ->
    'a file ->
    ('a lopt * (('a, 'b) string_or_value_spec -> 'b -> unit Lwt.t) -> 'r Lwt.t) ->
    'r Lwt.t
end

module type BACKEND_ARCHIVE = sig
  type t

  val append :
    t -> uuid -> ?last:last_event -> append_operation list -> bool Lwt.t

  val append_sealing : t -> uuid -> sealing_event -> bool Lwt.t
end

module type BACKEND_ELECTIONS = sig
  type t

  val new_election : t -> uuid option Lwt.t
  val archive_election : t -> uuid -> unit Lwt.t
  val delete_election : t -> uuid -> unit Lwt.t

  val validate_election :
    t -> uuid -> (unit, Belenios_web_api.validation_error) result Lwt.t
end

module type BACKEND_ACCOUNTS = sig
  type t

  val new_account_id : t -> (int * unit Lwt.u) option Lwt.t
end

module type STORAGE = sig
  type t

  val with_transaction : (t -> 'a Lwt.t) -> 'a Lwt.t
  val get_user_id : user -> int option Lwt.t
  val get_elections_by_owner : int -> Belenios_web_api.summary_list Lwt.t

  include BACKEND_GENERIC with type t := t
  include BACKEND_ARCHIVE with type t := t
  include BACKEND_ELECTIONS with type t := t
  include BACKEND_ACCOUNTS with type t := t
end

(** Scoped transaction tokens. Each kind of token is only produced by the
    corresponding [with_*_transaction] wrapper, so the compiler rejects calls
    that mix incompatible scopes. *)

(** Token for operations on a specific election ([Election (uuid, _)] files,
    [append], [append_sealing], [archive_election], [delete_election],
    [validate_election]). *)
module type ELECTION_TRANSACTION = sig
  type t

  val with_transaction : uuid -> (t -> 'a Lwt.t) -> 'a Lwt.t

  include BACKEND_GENERIC with type t := t
  include BACKEND_ARCHIVE with type t := t

  val archive_election : t -> uuid -> unit Lwt.t
  val delete_election : t -> uuid -> unit Lwt.t

  val validate_election :
    t -> uuid -> (unit, Belenios_web_api.validation_error) result Lwt.t
end

(** Token for creating a new election ([new_election]). *)
module type ELECTIONS_POOL_TRANSACTION = sig
  type t

  val with_transaction : (t -> 'a Lwt.t) -> 'a Lwt.t
  val new_election : t -> uuid option Lwt.t
end

(** Token for account and auth-db operations ([Account _], [Auth_db _],
    [Admin_password _] files, [new_account_id]). *)
module type ACCOUNT_TRANSACTION = sig
  type t

  val with_transaction : (t -> 'a Lwt.t) -> 'a Lwt.t

  include BACKEND_GENERIC with type t := t

  val new_account_id : t -> (int * unit Lwt.u) option Lwt.t
end
