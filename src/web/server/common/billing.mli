(**************************************************************************)
(*                                BELENIOS                                *)
(*                                                                        *)
(*  Copyright © 2023-2023 Inria                                           *)
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

open Belenios_core.Common
open Api_generic

val create : admin_id:int -> uuid:Uuid.t -> nb_voters:int -> string Lwt.t
val check : url:string -> id:string -> bool Lwt.t

val set_get_admin_context :
  (int -> Belenios_api.Serializable_t.billing_context Lwt.t) -> unit

val dispatch :
  token:string option ->
  ifmatch:string option ->
  string list ->
  [ `GET | `POST | `PUT | `DELETE ] ->
  body ->
  result Lwt.t
