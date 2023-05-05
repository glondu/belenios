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

open Web_serializable_t

type result =
  | Html : Html_types.div Eliom_content.Html.elt -> result
  | Redirection : string -> result

module type AUTH_SYSTEM = sig
  val pre_login_handler :
    [ `Username | `Address ] -> state:string -> result Lwt.t

  val direct : Yojson.Safe.t -> string Lwt.t
end

type auth_system = uuid option -> auth_config -> (module AUTH_SYSTEM)

module type S = sig
  type post_login_handler = {
    post_login_handler :
      'a.
      uuid option ->
      auth_config ->
      ((string * string) option -> 'a Lwt.t) ->
      'a Lwt.t;
  }

  val register :
    auth_system:string ->
    auth_system ->
    state:string ->
    post_login_handler ->
    Eliom_registration.Html.result Lwt.t

  val get_site_login_handler : string -> result Lwt.t
  val direct_voter_auth : uuid -> Yojson.Safe.t -> user Lwt.t
end
