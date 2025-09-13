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
open Belenios_storage_api
open Belenios_server_core
open Belenios_messages

val get_metadata : Storage.t -> admin_id:int -> uuid -> metadata Lwt.t
val format_password_email : material_message -> text_message Lwt.t

val generate_password_email :
  metadata -> Voter.t -> (bulk_email * (string * string)) Lwt.t

val format_credential_email : material_message -> text_message Lwt.t

val generate_credential_email :
  metadata ->
  recipient:string ->
  login:string ->
  weight:weight ->
  credential:string ->
  bulk_email Lwt.t

val mail_confirmation :
  (module Belenios_ui.I18n.GETTEXT) ->
  uuid ->
  title:string ->
  confirmation ->
  string option ->
  text_message

val email_login :
  (module Belenios_ui.I18n.GETTEXT) ->
  recipient:recipient ->
  code:string ->
  text_message
