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
open Belenios_web_api
open Api_generic

val get_election_status : Storage.E.t -> election_status Lwt.t
val get_records : Storage.E.t -> records Lwt.t

val get_partial_decryptions :
  Storage.E.t -> metadata -> partial_decryptions Lwt.t

val get_shuffles : Storage.E.t -> metadata -> shuffles Lwt.t
val skip_shuffler : Storage.E.t -> string -> unit Lwt.t
val select_shuffler : Storage.E.t -> metadata -> string -> unit Lwt.t

val dispatch :
  token:string option ->
  ifmatch:string option ->
  string list ->
  [ `GET | `POST | `PUT | `DELETE ] ->
  body ->
  result Lwt.t

val state_module : (module Web_auth_sig.STATE) option ref

val cast_ballot :
  (Storage.E.t -> confirmation -> bool Lwt.t) ->
  Storage.E.t ->
  (module Belenios.Election.ELECTION) ->
  ballot:string ->
  user:Web_auth_sig.timestamped_user ->
  precast_data:Web_persist.precast_data ->
  confirmation Lwt.t
