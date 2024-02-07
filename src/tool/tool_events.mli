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
open Belenios_core.Archive

type index

val get_index : file:string -> index
val get_data : index -> hash -> string option
val get_event : index -> hash -> event option
val get_roots : index -> roots

val fold_on_event_payload_hashes :
  index -> event_type -> hash -> (hash -> 'a -> 'a) -> 'a -> 'a

val fold_on_event_payloads :
  index -> event_type -> hash -> (string -> 'a -> 'a) -> 'a -> 'a

val fsck : index -> unit
val starts_with : prefix:index -> index -> bool

type append_operation = Data of string | Event of event_type * hash option

val append : index -> append_operation list -> unit

val init :
  file:string ->
  election:string ->
  trustees:string ->
  public_creds:string ->
  index

module DirectMonad : MONAD with type 'a t = 'a

module Writer :
  ARCHIVE_WRITER with type 'a m := 'a and type archive = out_channel
