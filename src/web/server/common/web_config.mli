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

open Web_serializable_t

val site_auth_config : auth_config list ref
val exported_auth_config :
  [`BuiltinPassword | `BuiltinCAS | `Export of auth_config] list ref
val locales_dir : string ref
val spool_dir : string ref
val accounts_dir : string ref
val server_name : string ref
val server_mail : string ref
val return_path : string option ref
val contact_uri : string option ref
val gdpr_uri : string ref
val warning_file : string option ref
val footer_file : string option ref
val admin_home : string option ref
val source_file : string ref
val maxmailsatonce : int ref
val uuid_length : int option ref
val default_group : string ref
val nh_group : string ref
val domain : string ref
val deny_revote : bool ref
val deny_newelection : bool ref
