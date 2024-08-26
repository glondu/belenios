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

module Make
    (_ : Web_state_sig.S)
    (_ : Web_i18n_sig.S)
    (_ : Web_services_sig.S)
    (_ : Pages_common_sig.S)
    (_ : Belenios_ui.Mails_admin_sig.S) : Pages_admin_sig.S

val mail_confirmation_link :
  (module Belenios_ui.I18n.GETTEXT) -> string -> string -> string * string

val mail_changepw_link :
  (module Belenios_ui.I18n.GETTEXT) -> string -> string -> string * string

val mail_set_email :
  (module Belenios_ui.I18n.GETTEXT) -> string -> string -> string * string
