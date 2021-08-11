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

open Lwt.Syntax
open Js_of_ocaml
open Js_of_ocaml_lwt
open Belenios_platform
open Belenios_core
open Belenios_tool_common
open Belenios_tool_js_common
open Serializable_j
open Tool_js_common
open Tool_tkeygen
open Tool_js_i18n.Gettext

let tkeygen () =
  let module P : PARAMS = struct
    let group = get_textarea "group"
    let version = get_textarea "version" |> int_of_string
  end in
  let module X = Make (P) (LwtJsRandom) () in
  let open X in
  let* {id=_; priv; pub} = trustee_keygen () in
  let hash =
    let pub = trustee_public_key_of_string Yojson.Safe.read_json pub in
    Platform.sha256_b64 (Yojson.Safe.to_string pub.trustee_public_key)
  in
  set_textarea "pk" pub;
  set_content "public_key_fp" hash;
  set_download "private_key" "application/json" "private_key.json" priv;
  set_element_display "submit_form" "inline";
  Lwt.return_unit

let fill_interactivity () =
  let$ e = document##getElementById (Js.string "interactivity") in
  let b = Dom_html.createButton document in
  let t = document##createTextNode (Js.string (s_ "Generate a key")) in
  Lwt_js_events.async (fun () ->
      let* _ = Lwt_js_events.click b in
      tkeygen ()
    );
  Dom.appendChild b t;
  Dom.appendChild e b

let () =
  Lwt.async (fun () ->
      let* _ = Lwt_js_events.onload () in
      let* () = Tool_js_i18n.auto_init "admin" in
      fill_interactivity ();
      Lwt.return_unit
    )
