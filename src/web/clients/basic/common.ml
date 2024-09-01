(**************************************************************************)
(*                                BELENIOS                                *)
(*                                                                        *)
(*  Copyright © 2012-2023 Inria                                           *)
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
open Belenios_js.Common

let user = ref (`Admin "" : Belenios_api.Endpoints.admin)

let generic_proceed x handler =
  let msg =
    let open Js_of_ocaml_lwt.XmlHttpRequest in
    match x.code with
    | 200 -> "Success"
    | code -> Printf.sprintf "Error %d: %s" code x.content
  in
  let b = button "Proceed" handler in
  Lwt.return [ div [ txt msg ]; div [ b ] ]
