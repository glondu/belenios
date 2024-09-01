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
open Belenios
open Belenios_js.Secondary_ui

module App (U : UI) = struct
  let component = "admin"

  let router configuration path =
    let open (val !Belenios_js.I18n.gettext) in
    U.set_title @@ s_ "Trustee management";
    match path with
    | [ "generate"; uuid; token ] ->
        Generate.generate configuration (Uuid.wrap uuid) ~token
    | [ "decrypt"; uuid; token ] -> Decrypt.decrypt (Uuid.wrap uuid) ~token
    | [ "shuffle"; uuid; token ] -> Shuffle.shuffle (Uuid.wrap uuid) ~token
    | [ "check" ] -> Check.check ()
    | [ "check"; uuid ] -> Check.check ~uuid ()
    | _ -> Lwt.return [ div [ txt @@ s_ "Error" ] ]
end

module _ = Make (App) ()
