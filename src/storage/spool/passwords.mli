(**************************************************************************)
(*                                BELENIOS                                *)
(*                                                                        *)
(*  Copyright © 2026-2026 Inria                                           *)
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

(** Administrator password databases in the spool of the filesystem storage
    backend. A database that does not exist is empty. *)

type record = {
  username : string;
  salt : string;
  hashed : string;
  address : string option;
}
[@@deriving yojson]

val dir : spool:string -> string -> string
(** [dir ~spool db] is the directory of database [db] ([passwords/db] in
    [spool]). Fails if [db] is not a valid name (empty, starting with a dot, or
    containing a slash). *)

(** {1 Online access} *)

(** The following functions take the directory of a database, as returned by
    {!dir}. Lookups are case-insensitive. *)

val get_by_username : dir:string -> username:string -> record option Lwt.t
val get_by_address : dir:string -> address:string -> record option Lwt.t

val set : dir:string -> record -> unit Lwt.t
(** Stores a record under its username, and updates the links of its old and new
    addresses. *)

(** {1 Offline conversions} *)

val records_of_csv : string -> record list
(** Each line consists of a username, a salt, a hash and an optional address,
    separated by commas; additional fields are ignored. *)

val csv_of_records : record list -> string

val import :
  spool:string -> db:string -> force:bool -> record list -> unit Lwt.t
(** Creates database [db] with the given records, replacing an existing one if
    [force] is set. Fails on duplicate usernames or addresses. *)

val export : spool:string -> db:string -> record list Lwt.t
(** Returns the records of database [db], sorted by username. Fails if the
    database does not exist. *)
