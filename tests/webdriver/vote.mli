(**************************************************************************)
(*                                BELENIOS                                *)
(*                                                                        *)
(*  Copyright © 2024-2024 Inria                                           *)
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

module type CONFIG = sig
  val headless : bool
  val webdriver : string
  val belenios : string
  val config : Admin.config
  val election_id : string
  val emails : in_channel
end

type authenticator = Webdriver.helpers -> unit Lwt.t

module Make (Config : CONFIG) : sig
  val auth_password : username:string -> password:string -> authenticator
  val auth_email : username:string -> authenticator

  val vote :
    voter:string -> credential:string -> auth:authenticator -> unit Lwt.t
end
