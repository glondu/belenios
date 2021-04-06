(**************************************************************************)
(*                                BELENIOS                                *)
(*                                                                        *)
(*  Copyright © 2012-2020 Inria                                           *)
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

open Lwt
open Lwt.Syntax

let scope = Eliom_common.default_session_scope

let show_cookie_disclaimer = Eliom_reference.eref ~scope true

let site_user = Eliom_reference.eref ~scope None
let election_user = Eliom_reference.eref ~scope None

let get_election_user uuid =
  let* user = Eliom_reference.get election_user in
  match user with
  | Some (u, x) when u = uuid -> return_some x
  | _ -> return_none

let ballot = Eliom_reference.eref ~scope None
let cast_confirmed = Eliom_reference.eref ~scope None

let language = Eliom_reference.eref ~scope None

type link_kind =
  [ `CreateAccount
  | `ChangePassword of string
  ]

let signup_address = Eliom_reference.eref ~scope None
let signup_env = Eliom_reference.eref ~scope None
