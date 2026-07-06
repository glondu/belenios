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
val get_draft_voters : ('a, 'b) draft_election -> voter_list

val put_draft_voters :
  ('a, 'b) draft_election updatable_with_billing -> voter_list -> unit Lwt.t

type generate_credentials_on_server_error =
  [ `NoVoters | `TooManyVoters | `Already | `NoServer ]

val generate_credentials_on_server :
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

val generate_server_trustee :
  ('a, 'b) group ->
  ('a, 'b) draft_election ->
  ('a, 'b) draft_basic_trustee Lwt.t

val get_draft_trustees :
  is_admin:bool ->
  ('a, 'b) draft_election ->
  ('a, 'b) Belenios_web_api.draft_trustees

val post_draft_trustees :
  ('a, 'b) group ->
  ('a, 'b) draft_election updatable_with_billing ->
  json trustee ->
  unit Lwt.t

val delete_draft_trustee :
  ('a, 'b) draft_election updatable_with_billing -> string -> bool Lwt.t

val set_threshold :
  ('a, 'b) draft_election updatable_with_billing ->
  int ->
  (unit, [ `NoTrustees | `OutOfBounds ]) Stdlib.result Lwt.t

val get_draft_trustees_mode :
  ('a, 'b) draft_election -> [ `Basic | `Threshold of int ]

val put_draft_trustees_mode :
  ('a, 'b) draft_election updatable_with_billing ->
  [ `Basic | `Threshold of int ] ->
  unit Lwt.t

val get_draft_status :
  uuid -> ('a, 'b) draft_election -> metadata -> draft_status Lwt.t

val merge_voters :
  draft_voter list ->
  Voter.t list ->
  (draft_voter list * weight, Voter.t) Stdlib.result

val import_voters :
  uuid ->
  ('a, 'b) draft_election updatable_with_billing ->
  Storage.E.t ->
  ( unit,
    [ `Forbidden
    | `NotFound
    | `TotalWeightTooBig of weight
    | `Duplicate of string ] )
  Stdlib.result
  Lwt.t

val import_trustees :
  ('a, 'b) group ->
  ('a, 'b) draft_election updatable_with_billing ->
  Storage.E.t ->
  metadata ->
  ( [> `Basic | `Threshold ],
    [> `Inconsistent | `Invalid | `MissingPrivateKeys | `None | `Unsupported ]
  )
  Stdlib.result
  Lwt.t

val post_trustee_basic :
  ('a, 'b) group ->
  ('a, 'b) draft_election updatable_with_billing ->
  token:string ->
  string ->
  unit Lwt.t

val post_trustee_threshold :
  ('a, 'b) group ->
  ('a, 'b) draft_election updatable_with_billing ->
  token:string ->
  string ->
  unit Lwt.t

open Api_generic

val handle_trustee :
  token:token_user ->
  [ `GET | `POST | `PUT | `DELETE ] ->
  body ->
  wrapped_draft_election updatable_with_billing ->
  result Lwt.t

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
