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

open Belenios_storage_api

val create_account :
  Storage.t ->
  name:string option ->
  email:string option ->
  user ->
  account Lwt.t

val get_account_by_id : Storage.t -> int -> account option Lwt.t

val update_account_by_id :
  Storage.t ->
  int ->
  (account Lopt.t * (account -> unit Lwt.t), 'r) Web_common.with_lwt_cont

val add_update_hook : (account -> unit Lwt.t) -> unit

type capability = Sudo

val has_capability : capability -> account -> bool
val check : account -> int list -> bool
val max_voters : account -> int
