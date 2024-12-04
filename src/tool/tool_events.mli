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

open Belenios

type index

val get_index : file:string -> index Lwt.t
val get_data : index -> hash -> string option Lwt.t
val get_event : index -> hash -> event option Lwt.t
val get_last_event : index -> hash option
val get_roots : index -> roots

val fold_on_event_payload_hashes :
  index -> event_type -> hash -> (hash -> 'a -> 'a Lwt.t) -> 'a -> 'a Lwt.t

val fold_on_event_payloads :
  index -> event_type -> hash -> (string -> 'a -> 'a Lwt.t) -> 'a -> 'a Lwt.t

val fsck : index -> unit Lwt.t
val starts_with : prefix:index -> index -> bool

type append_operation = Data of string | Event of event_type * hash option

val append : index -> append_operation list -> unit Lwt.t

val init :
  file:string ->
  election:string ->
  trustees:string ->
  public_creds:string ->
  index Lwt.t

module LwtMonad : MONAD with type 'a t = 'a Lwt.t

type file = { mutable pos : int64; fd : Lwt_unix.file_descr }

module Writer :
  Archive.ARCHIVE_WRITER with type 'a m := 'a Lwt.t and type archive = file
