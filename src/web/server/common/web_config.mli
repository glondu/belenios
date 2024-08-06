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

open Belenios_server_core

val prefix : string ref
val site_auth_config : auth_config list ref

val exported_auth_config :
  [ `BuiltinPassword | `BuiltinCAS | `Export of auth_config ] list ref

val locales_dir : string ref
val vendor : string ref
val server_name : string ref
val server_mail : string ref
val return_path : string option ref
val contact_uri : string option ref
val tos : string ref
val tos_last_update : float ref
val warning_file : string option ref
val footer_file : string option ref
val admin_home : string option ref
val success_snippet : string option ref
val source_file : string ref
val logo : (string * string) option ref
val favicon : (string * string) option ref
val sealing : (string * string) option ref
val maxmailsatonce : int ref
val default_group : string ref
val nh_group : string ref
val domain : string ref
val deny_revote : bool ref
val deny_newelection : bool ref
val blacklisted_domains : Belenios.SSet.t ref
val billing : (string * string) option ref
val restricted_mode : bool ref
