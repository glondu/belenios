(**************************************************************************)
(*                                BELENIOS                                *)
(*                                                                        *)
(*  Copyright © 2012-2021 Inria                                           *)
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

open Belenios_core
open Serializable_builtin_t
open Web_serializable_t

module type S = sig

  val show_cookie_disclaimer : bool Eliom_reference.eref

  val site_user : user option Eliom_reference.eref
  val election_user : (uuid * user) option Eliom_reference.eref
  val get_election_user : uuid -> user option Lwt.t

  val ballot : string option Eliom_reference.eref
  val cast_confirmed : (string * Weight.t * bool, Web_common.error) result option Eliom_reference.eref

  val language : string option Eliom_reference.eref

  type link_kind =
    [ `CreateAccount
    | `ChangePassword of string
    ]

  val signup_address : string option Eliom_reference.eref
  val signup_env : (string * link_kind) option Eliom_reference.eref

end
