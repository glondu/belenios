(**************************************************************************)
(*                                BELENIOS                                *)
(*                                                                        *)
(*  Copyright © 2012-2022 Inria                                           *)
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

open Serializable_t
open Signatures

type data_or_event = Data | Event of event

type record =
  {
    typ : data_or_event;
    hash : hash;
    location : location;
  }

val block_size : int

module type IO_READER = sig
  include MONAD
  type file
  val get_pos : file -> int64 t
  val set_pos : file -> int64 -> unit t
  val read_block : file -> bytes -> unit t
end

module type ARCHIVE_READER = sig
  type 'a m
  type archive
  val read_header : archive -> archive_header m
  val read_record : archive -> record m
end

module MakeReader (M : IO_READER) : ARCHIVE_READER
       with type 'a m := 'a M.t and type archive = M.file

module type IO_WRITER = sig
  include MONAD
  type file
  val get_pos : file -> int64 t
  val write_block : file -> bytes -> unit t
end

module type ARCHIVE_WRITER = sig
  type 'a m
  type archive
  val write_header : archive -> archive_header -> unit m
  val write_record : archive -> timestamp:int64 -> data_or_event -> string -> record m
end

module MakeWriter (M : IO_WRITER) : ARCHIVE_WRITER
       with type 'a m := 'a M.t and type archive = M.file

module type IO_ARCHIVER = sig
  include MONAD
  val get_hash : hash -> string option t
end

module type ARCHIVER = sig
  type 'a m
  type archive
  val write_archive : archive -> archive_header -> event -> unit m
end

module MakeArchiver
         (M : IO_ARCHIVER)
         (W : ARCHIVE_WRITER with type 'a m := 'a M.t)
       : ARCHIVER
       with type 'a m := 'a M.t and type archive := W.archive
