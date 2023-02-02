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

open Lwt.Syntax
open Js_of_ocaml
open Belenios_core.Common
open Belenios_js.Common

let computed_fingerprint = ref ""

let compute_handler input output _ =
  let open (val !Belenios_js.I18n.gettext) in
  let input = Js.to_string input##.value in
  computed_fingerprint := sha256_b64 input;
  clear_content output;
  Dom.appendChild output (document##createTextNode (Js.string (s_ "Computed fingerprint:")));
  Dom.appendChild output (document##createTextNode (Js.string " "));
  Dom.appendChild output (document##createTextNode (Js.string !computed_fingerprint));
  Js._true

let compare_handler input output _ =
  let open (val !Belenios_js.I18n.gettext) in
  let input = Js.to_string input##.value in
  if input <> "" then (
    let result =
      if input = !computed_fingerprint then
        s_ "The fingerprints match!"
      else
        s_ "The fingerprints differ!"
    in
    clear_content output;
    Dom.appendChild output (document##createTextNode (Js.string result))
  );
  Js._true

let fill_interactivity () =
  let open (val !Belenios_js.I18n.gettext) in
  let$ container = document##getElementById (Js.string "interactivity") in
  let result_div = Dom_html.createDiv document in
  let intro_div = Dom_html.createDiv document in
  Dom.appendChild intro_div (document##createTextNode (Js.string (s_ "Please paste the data for which you want to compute the fingerprint in the text area below:")));
  Dom.appendChild container intro_div;
  let textarea_div = Dom_html.createDiv document in
  let textarea = Dom_html.createTextarea document in
  textarea##.cols := 80;
  textarea##.rows := 25;
  Dom.appendChild textarea_div textarea;
  Dom.appendChild container textarea_div;
  let compute_div = Dom_html.createDiv document in
  let compute = Dom_html.createButton document in
  let compute_label = document##createTextNode (Js.string (s_ "Compute fingerprint")) in
  compute##.onclick := Dom_html.handler (compute_handler textarea result_div);
  Dom.appendChild compute compute_label;
  Dom.appendChild compute_div compute;
  Dom.appendChild container compute_div;
  Dom.appendChild container result_div;
  let input_div = Dom_html.createDiv document in
  Dom.appendChild input_div (document##createTextNode (Js.string (s_ "Expected fingerprint:")));
  Dom.appendChild input_div (document##createTextNode (Js.string " "));
  let input = Dom_html.createInput document in
  input##.size := 50;
  Dom.appendChild input_div input;
  Dom.appendChild container input_div;
  let compare_div = Dom_html.createDiv document in
  let compare = Dom_html.createButton document in
  let compare_span = Dom_html.createB document in
  Dom.appendChild compare (document##createTextNode (Js.string (s_ "Compare")));
  compare##.onclick := Dom_html.handler (compare_handler input compare_span);
  Dom.appendChild compare_div compare;
  Dom.appendChild compare_div (document##createTextNode (Js.string " "));
  Dom.appendChild compare_div compare_span;
  Dom.appendChild container compare_div

let () =
  Lwt.async (fun () ->
      let* _ = Js_of_ocaml_lwt.Lwt_js_events.onload () in
      let* () = Belenios_js.I18n.auto_init "admin" in
      fill_interactivity ();
      Lwt.return_unit
    )
