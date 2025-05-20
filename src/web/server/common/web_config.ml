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

let prefix = ref ""
let site_auth_config = ref []
let exported_auth_config = ref []
let share_dir = ref "."
let vendor = ref "Belenios"
let server_name = ref "Belenios public server"
let server_mail = ref "noreply@example.org"
let return_path = ref None
let contact_uri = ref None
let tos = ref ""
let tos_last_update = ref (Unix.gettimeofday ())
let warning_file = ref None
let footer_file = ref None
let admin_home = ref None
let success_snippet = ref None
let source_file = ref "belenios.tar.gz"
let logo = ref None
let favicon = ref None
let sealing = ref None
let maxmailsatonce = ref 1000
let default_group = ref ""
let nh_group = ref ""
let domain = ref ""
let deny_revote = ref false
let deny_newelection = ref false
let blacklisted_domains = ref Belenios.SSet.empty
let billing = ref None
let restricted_mode = ref false
let election_sealing = ref false
let send_message = ref None
