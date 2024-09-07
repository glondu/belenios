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

module type S = sig
  val lost_ballot :
    Storage.t ->
    (module Site_common_sig.ELECTION) ->
    unit ->
    [> `Html ] Eliom_content.Html.F.elt Lwt.t

  val cast_confirmed :
    (module Site_common_sig.ELECTION) ->
    result:
      ( Belenios_api.Serializable_t.confirmation,
        Belenios_ui.Confirmation.error )
      result ->
    unit ->
    [> `Html ] Eliom_content.Html.F.elt Lwt.t
end
