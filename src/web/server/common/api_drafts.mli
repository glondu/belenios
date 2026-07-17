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

open Belenios
open Belenios_web_api
open Belenios_storage_api
open Web_common

val authentication_of_auth_config :
  auth_config list option -> authentication option

val api_of_draft : ('a, 'b) draft_election -> metadata -> draft Lwt.t

val draft_of_api :
  account ->
  uuid ->
  ('a, 'b) draft_election ->
  metadata ->
  draft ->
  ('a, 'b) draft_election * metadata

val post_drafts : account -> draft -> uuid option Lwt.t
val put_draft_voters : Storage.E.t -> voter list -> unit Lwt.t

type generate_credentials_on_server_error =
  [ `NoVoters | `TooManyVoters | `Already | `NoServer ]

val generate_credentials_on_server :
  Storage.E.t ->
  account ->
  uuid ->
  ('a, 'b) draft_election ->
  (unit, generate_credentials_on_server_error) Stdlib.result Lwt.t

val exn_of_generate_credentials_on_server_error :
  generate_credentials_on_server_error -> exn

val submit_public_credentials :
  Storage.E.t ->
  ('a, 'b) group ->
  ('a, 'b) draft_election updatable_with_billing ->
  ?certificate:('a, 'b) credentials_certificate ->
  'a public_credentials_with_id ->
  unit Lwt.t

val get_draft_status :
  Storage.E.t ->
  uuid ->
  ('a, 'b) draft_election ->
  metadata ->
  draft_status Lwt.t

val merge_voters :
  voter list -> voter list -> (voter list * weight, Voter.t) Stdlib.result

val import_voters :
  Storage.E.t ->
  uuid ->
  ('a, 'b) draft_election ->
  Storage.E.t ->
  ( unit,
    [ `Forbidden
    | `NotFound
    | `TotalWeightTooBig of weight
    | `Duplicate of string ] )
  Stdlib.result
  Lwt.t

open Api_generic

val dispatch_draft :
  token:token_user ->
  ifmatch:string option ->
  string list ->
  [ `GET | `POST | `PUT | `DELETE ] ->
  body ->
  Storage.E.t ->
  uuid ->
  wrapped_draft_election updatable_with_billing ->
  metadata updatable ->
  result Lwt.t
