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

open Belenios_server_core
open Web_common
open Belenios_api.Serializable_t
open Api_generic

val get_election_status : election_status Lwt.t Storage.u
val get_records : records Lwt.t Storage.u
val get_partial_decryptions : (metadata -> partial_decryptions Lwt.t) Storage.u
val get_shuffles : (metadata -> shuffles Lwt.t) Storage.u
val skip_shuffler : (string -> unit Lwt.t) Storage.u
val select_shuffler : (metadata -> string -> unit Lwt.t) Storage.u

val post_partial_decryption :
  Storage.t ->
  uuid ->
  (module Belenios.Election.ELECTION) ->
  trustee_id:int ->
  partial_decryption:string ->
  (unit, [> `AlreadyDone | `Invalid ]) Stdlib.result Lwt.t

val dispatch :
  Storage.t ->
  token:string option ->
  ifmatch:string option ->
  string list ->
  [ `GET | `POST | `PUT | `DELETE ] ->
  body ->
  result Lwt.t

val direct_voter_auth : (Yojson.Safe.t -> user Lwt.t) Storage.u ref

val cast_ballot :
  (bool -> string -> string -> weight option -> string -> bool Lwt.t) Storage.u ->
  (rawballot:string ->
  user:user ->
  precast_data:string * credential_record ->
  (user * string * bool * weight * bool) Lwt.t)
  Storage.u
