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

open Belenios_core.Common
open Belenios_api.Serializable_t
open Web_serializable_t

val api_of_draft : draft_election -> draft Lwt.t
val draft_of_api : account -> uuid -> draft_election -> draft -> draft_election
val post_drafts : account -> draft -> uuid option Lwt.t
val get_draft_voters : draft_election -> voter_list
val put_draft_voters : uuid -> draft_election -> voter_list -> unit Lwt.t

type generate_credentials_on_server_error =
  [ `NoVoters | `TooManyVoters | `Already | `NoServer ]

val generate_credentials_on_server :
  uuid ->
  draft_election ->
  (unit, generate_credentials_on_server_error) Stdlib.result Lwt.t

val exn_of_generate_credentials_on_server_error :
  generate_credentials_on_server_error -> exn

val submit_public_credentials :
  uuid -> draft_election -> public_credentials -> unit Lwt.t

val generate_server_trustee :
  draft_election -> Yojson.Safe.t draft_trustee Lwt.t

val get_draft_trustees :
  is_admin:bool ->
  draft_election ->
  (Yojson.Safe.t, Yojson.Safe.t) Belenios_api.Serializable_t.draft_trustees

val post_draft_trustees :
  uuid -> draft_election -> Yojson.Safe.t trustee -> unit Lwt.t

val delete_draft_trustee : uuid -> draft_election -> string -> bool Lwt.t

val set_threshold :
  uuid ->
  draft_election ->
  int ->
  (unit, [ `NoTrustees | `OutOfBounds ]) Stdlib.result Lwt.t

val get_draft_trustees_mode : draft_election -> [ `Basic | `Threshold of int ]

val put_draft_trustees_mode :
  uuid -> draft_election -> [ `Basic | `Threshold of int ] -> unit Lwt.t

val get_draft_status : uuid -> draft_election -> draft_status Lwt.t

val merge_voters :
  draft_voter list ->
  Voter.t list ->
  (Voter.t -> (string * string) option) ->
  (draft_voter list * weight, Voter.t) Stdlib.result

val import_voters :
  uuid ->
  draft_election ->
  uuid ->
  ( unit,
    [ `Forbidden
    | `NotFound
    | `TotalWeightTooBig of weight
    | `Duplicate of string ] )
  Stdlib.result
  Lwt.t

val import_trustees :
  uuid ->
  draft_election ->
  uuid ->
  metadata ->
  ( [> `Basic | `Threshold ],
    [> `Inconsistent | `Invalid | `MissingPrivateKeys | `None | `Unsupported ]
  )
  Stdlib.result
  Lwt.t

open Api_generic

val dispatch :
  token:string option ->
  ifmatch:string option ->
  string list ->
  [ `GET | `POST | `PUT | `DELETE ] ->
  body ->
  result Lwt.t
