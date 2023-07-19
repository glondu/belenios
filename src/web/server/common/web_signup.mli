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

type link_kind = [ `CreateAccount | `ChangePassword of string ]

val send_confirmation_link :
  (module Belenios_ui.I18n.GETTEXT) -> service:string -> string -> unit Lwt.t

val send_changepw_link :
  (module Belenios_ui.I18n.GETTEXT) ->
  service:string ->
  address:string ->
  username:string ->
  unit Lwt.t

val confirm_link : string -> (string * string * link_kind) option Lwt.t
val remove_link : string -> unit Lwt.t
val check_password : string -> string option Lwt.t
