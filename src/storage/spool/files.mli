(**************************************************************************)
(*                                BELENIOS                                *)
(*                                                                        *)
(*  Copyright © 2012-2026 Inria                                           *)
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

(** Generic file operations, shared by the filesystem storage backend and
    offline tools working on its spool. *)

val file_exists : string -> bool Lwt.t
val read_file : string -> string option Lwt.t

val write_file : string -> string -> unit Lwt.t
(** [write_file f x] atomically replaces the contents of [f] with [x], and
    returns once the new contents are on disk. *)

val write_symlink : string -> string -> unit Lwt.t
(** [write_symlink f target] atomically makes [f] a symbolic link to [target],
    and returns once it is on disk. *)

val read_symlink : string -> string option Lwt.t
(** Returns [None] if the file does not exist or is not a symbolic link. *)

val cleanup_file : string -> unit Lwt.t
(** Removes a file, ignoring errors. *)

val mkdir : string -> unit Lwt.t
(** Creates a directory if it does not exist yet, and makes the new entry
    durable. *)

val remove_tree : string -> unit Lwt.t
(** Recursively removes a directory, without following symbolic links. Does
    nothing if it does not exist. *)

val fsync_dir : string -> unit Lwt.t
