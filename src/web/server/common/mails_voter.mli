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
open Web_serializable_t

val generate_password_email :
  Web_serializable_t.metadata ->
  string list ->
  string ->
  Web_serializable_t.uuid ->
  Voter.t -> bool -> (bulk_email * (string * string)) Lwt.t

val generate_credential_email :
  Web_serializable_t.uuid ->
  Web_serializable_t.draft_election ->
  recipient:string ->
  login:string ->
  weight:Web_serializable_t.weight -> credential:string -> bulk_email Lwt.t

val submit_bulk_emails : bulk_email list -> unit Lwt.t
val process_bulk_emails : unit -> unit Lwt.t

val mail_confirmation :
  (module Belenios_ui.I18n.GETTEXT) ->
  string ->
  string ->
  Web_serializable_t.weight option ->
  string ->
  bool -> string -> string -> string option -> string
