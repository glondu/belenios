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

open Js_of_ocaml_tyxml
open Tyxml_js.Html5

module U = struct
  let router configuration path =
    let open (val !Belenios_js.I18n.gettext) in
    match path with
    | [ "generate"; uuid; token ] ->
        Generate.generate configuration ~uuid ~token
    | _ -> Lwt.return [ div [ txt @@ s_ "Error" ] ]

  let title () =
    let open (val !Belenios_js.I18n.gettext) in
    s_ "Credential authority management"
end

module _ = Belenios_js.Secondary_ui.Make (U) ()