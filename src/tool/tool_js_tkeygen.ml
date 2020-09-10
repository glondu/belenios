(**************************************************************************)
(*                                BELENIOS                                *)
(*                                                                        *)
(*  Copyright © 2012-2020 Inria                                           *)
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
open Belenios_platform
open Belenios
open Belenios_tool_common
open Belenios_tool_js_common
open Serializable_j
open Tool_js_common
open Tool_tkeygen

let tkeygen _ =
  let module P : PARAMS = struct
    let group = get_textarea "group"
  end in
  let module X = (val make (module P : PARAMS) : S) in
  let open X in
  let {id=_; priv; pub} = trustee_keygen () in
  let hash =
    let pub = trustee_public_key_of_string Yojson.Safe.read_json pub in
    Platform.sha256_b64 (Yojson.Safe.to_string pub.trustee_public_key)
  in
  set_textarea "pk" pub;
  set_content "public_key_fp" hash;
  set_download "private_key" "application/json" "private_key.json" priv;
  set_element_display "submit_form" "inline";
  Js._false

let fill_interactivity _ =
  let () =
    document##getElementById (Js.string "interactivity") >>== fun e ->
    let b = Dom_html.createButton document in
    let t = document##createTextNode (Js.string "Generate a new keypair") in
    b##.onclick := Dom_html.handler tkeygen;
    Dom.appendChild b t;
    Dom.appendChild e b
  in Js._false

let () =
  Dom_html.window##.onload := Dom_html.handler fill_interactivity;
