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

open Belenios_storage_api

type signup_kind = CreateAccount | ChangePassword of { username : string }
type signup_env = { kind : signup_kind; service : string }

module type S = sig
  val site_user : (user * account * string) option Eliom_reference.eref
  val signup_address : string option Eliom_reference.eref
  val signup_env : signup_env option Eliom_reference.eref
  val set_email_env : string option Eliom_reference.eref
  val discard : unit -> unit Lwt.t
  val get_consent_cookie : unit -> bool
  val set_consent_cookie : unit -> unit
end
