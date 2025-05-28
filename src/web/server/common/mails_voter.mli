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

val format_password_email :
  Storage.t -> material_message -> Mails_common.text_message Lwt.t

val generate_password_email :
  uuid ->
  admin_id:int ->
  Voter.t ->
  bool ->
  (bulk_email * (string * string)) Lwt.t

val format_credential_email :
  Storage.t -> material_message -> Mails_common.text_message Lwt.t

val generate_credential_email :
  uuid ->
  admin_id:int ->
  draft_election ->
  recipient:string ->
  login:string ->
  weight:weight ->
  credential:string ->
  bulk_email Lwt.t

val mail_confirmation :
  (module Belenios_ui.I18n.GETTEXT) ->
  uuid ->
  title:string ->
  Belenios_web_api.confirmation ->
  string option ->
  Mails_common.text_message

val email_login :
  (module Belenios_ui.I18n.GETTEXT) ->
  recipient:Belenios_web_api.recipient ->
  code:string ->
  Mails_common.text_message
