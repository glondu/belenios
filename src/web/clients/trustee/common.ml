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
open Belenios
open Belenios_js.Common

let wrap_handler handler x =
  let@ () = Lwt.async in
  Lwt.catch
    (fun () -> handler x)
    (fun _ ->
      let open (val !Belenios_js.I18n.gettext) in
      alert
      @@ s_
           "Error while processing the private key. Did you load the right \
            file?";
      Lwt.return_unit)

let make_private_key_input handler =
  let open (val !Belenios_js.I18n.gettext) in
  let open Tyxml_js.Html in
  let raw_elt = input ~a:[ a_id "private_key"; a_input_type `Hidden ] () in
  let raw_dom = Tyxml_js.To_dom.of_input raw_elt in
  let onchange _ =
    wrap_handler handler (Js.to_string raw_dom##.value);
    Js._false
  in
  raw_dom##.onchange := Dom_html.handler onchange;
  let file_elt = input ~a:[ a_id "private_key_file"; a_input_type `File ] () in
  let file_dom = Tyxml_js.To_dom.of_input file_elt in
  let onchange _ =
    let ( let& ) x f = Js.Opt.case x (fun () -> Js._false) f in
    let ( let$ ) x f = match x with None -> Js._false | Some x -> f x in
    let$ file = Belenios_js.Compat.get_file file_dom in
    let reader = new%js File.fileReader in
    reader##.onload :=
      Dom.handler (fun _ ->
          let& content = File.CoerceTo.string reader##.result in
          wrap_handler handler (String.trim (Js.to_string content));
          Js._false);
    reader##readAsText file;
    Js._false
  in
  file_dom##.onchange := Dom_html.handler onchange;
  div
    ~a:[ a_id "input_private_key" ]
    [
      txt @@ s_ "Please load your private key from a file:";
      txt " ";
      file_elt;
      raw_elt;
    ]
