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

open Js_of_ocaml
open Js_of_ocaml_tyxml

let make_private_key_input () =
  let open (val !Belenios_js.I18n.gettext) in
  let open Tyxml_js.Html in
  let raw_elt =
    input ~a:[ a_id "private_key"; a_input_type `Text; a_size 80 ] ()
  in
  let raw_dom = Tyxml_js.To_dom.of_input raw_elt in
  let file_elt = input ~a:[ a_id "private_key_file"; a_input_type `File ] () in
  let file_dom = Tyxml_js.To_dom.of_input file_elt in
  let onchange _ =
    let ( let& ) x f = Js.Opt.case x (fun () -> Js._false) f in
    let ( let$ ) x f = Js.Optdef.case x (fun () -> Js._false) f in
    let$ files = file_dom##.files in
    let& file = files##item 0 in
    let reader = new%js File.fileReader in
    reader##.onload :=
      Dom.handler (fun _ ->
          let& content = File.CoerceTo.string reader##.result in
          raw_dom##.value := content;
          Js._false);
    reader##readAsText file;
    Js._false
  in
  file_dom##.onchange := Dom_html.handler onchange;
  let elt =
    div
      ~a:[ a_id "input_private_key" ]
      [
        div [ txt @@ s_ "Please enter your private key:"; txt " "; raw_elt ];
        div [ txt @@ s_ "Or load it from a file:"; txt " "; file_elt ];
      ]
  in
  (elt, fun () -> Js.to_string raw_dom##.value)
