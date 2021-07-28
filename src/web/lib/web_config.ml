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

let site_auth_config = ref []
let exported_auth_config = ref []
let locales_dir = ref "."
let spool_dir = ref "."
let server_name = ref "Belenios public server"
let server_mail = ref "noreply@example.org"
let return_path = ref None
let contact_uri = ref None
let gdpr_uri = ref ""
let warning_file = ref None
let footer_file = ref None
let admin_home = ref None
let source_file = ref "belenios.tar.gz"
let maxmailsatonce = ref 1000
let uuid_length = ref None
let default_group = ref ""
let nh_group = ref ""
let domain = ref ""
