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

open Web_common

module type S = sig
  val privacy_notice : privacy_cont -> [> `Html ] Eliom_content.Html.F.elt Lwt.t

  val admin_login :
    (string -> Web_auth_sig.result Lwt.t) ->
    [> `Html ] Eliom_content.Html.F.elt Lwt.t

  val signup_captcha :
    service:string ->
    captcha_error option ->
    string ->
    string ->
    [> `Html ] Eliom_content.Html.F.elt Lwt.t

  val signup_changepw :
    service:string ->
    captcha_error option ->
    string ->
    string ->
    string ->
    [> `Html ] Eliom_content.Html.F.elt Lwt.t

  val signup_login :
    (string, unit) result -> [> `Html ] Eliom_content.Html.F.elt Lwt.t

  val signup :
    string ->
    add_account_error option ->
    string ->
    [> `Html ] Eliom_content.Html.F.elt Lwt.t

  val changepw :
    username:string ->
    address:string ->
    add_account_error option ->
    [> `Html ] Eliom_content.Html.F.elt Lwt.t

  val compute_fingerprint : unit -> [> `Html ] Eliom_content.Html.F.elt Lwt.t
  val set_email : unit -> [> `Html ] Eliom_content.Html.F.elt Lwt.t

  val set_email_confirm :
    (string, unit) result -> [> `Html ] Eliom_content.Html.F.elt Lwt.t

  val sudo : unit -> [> `Html ] Eliom_content.Html.F.elt Lwt.t
end
