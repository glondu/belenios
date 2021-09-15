(**************************************************************************)
(*                                BELENIOS                                *)
(*                                                                        *)
(*  Copyright © 2012-2021 Inria                                           *)
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

open Belenios_api.Serializable_t
open Web_serializable_t

exception Error of string

val api_of_draft : draft_election -> draft
val draft_of_api : draft_election -> draft -> draft_election

val delete_draft : uuid -> unit Lwt.t
val post_drafts : account -> draft -> uuid Lwt.t

val get_drafts_voters : draft_election -> voter_list
val put_drafts_voters : uuid -> draft_election -> voter_list -> unit Lwt.t

val get_draft_credentials : uuid -> draft_election -> credentials Lwt.t

type generate_credentials_on_server_error =
  [ `NoVoters
  | `TooManyVoters
  | `Already
  | `NoServer
  ]

val generate_credentials_on_server :
  (recipient:string -> login:string -> weight:weight -> cred:string -> unit Lwt.t) ->
  uuid -> draft_election ->
  (unit, generate_credentials_on_server_error) Stdlib.result Lwt.t

val exn_of_generate_credentials_on_server_error :
  generate_credentials_on_server_error -> exn

val submit_public_credentials : uuid -> draft_election -> credential_list -> unit Lwt.t
