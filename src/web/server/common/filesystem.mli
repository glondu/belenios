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

val file_exists : string -> bool Lwt.t
val read_file : ?uuid:uuid -> string -> string list option Lwt.t
val read_whole_file : ?uuid:uuid -> string -> string option Lwt.t
val read_file_single_line : ?uuid:uuid -> string -> string option Lwt.t
val write_file : ?uuid:uuid -> string -> string list -> unit Lwt.t
val write_whole_file : ?uuid:uuid -> string -> string -> unit Lwt.t

val cleanup_file : string -> unit Lwt.t
val rmdir : string -> unit Lwt.t
