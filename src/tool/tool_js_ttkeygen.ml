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
open Belenios_platform
open Belenios_core
open Belenios
open Belenios_tool_js_common
open Serializable_j
open Signatures
open Common
open Tool_js_common
open Tool_js_i18n.Gettext

let set_step i =
  let$ e = document##getElementById (Js.string "current_step") in
  clear_content e;
  let t = Printf.sprintf (f_ "Step %d/3") i in
  let t = document##createTextNode (Js.string t) in
  Dom.appendChild e t

let set_explain str =
  let$ e = document##getElementById (Js.string "explain") in
  clear_content e;
  let t = document##createTextNode (Js.string str) in
  Dom.appendChild e t;
  Dom.appendChild e (Dom_html.createBr document)

let gen_cert e _ =
  let version = get_textarea "version" |> int_of_string in
  let group = get_textarea "group" in
  let module G = (val Group.of_string ~version group : GROUP) in
  let module Trustees = (val Trustees.get_by_version version) in
  let module P = Trustees.MakePKI (G) (DirectRandom) in
  let module C = Trustees.MakeChannels (G) (DirectRandom) (P) in
  let module T = Trustees.MakePedersen (G) (DirectRandom) (P) (C) in
  let key, cert = T.step1 () in
  clear_content e;
  set_download "private_key" "text/plain" "private_key.txt" key;
  set_element_display "key_helper" "block";
  let fp = Platform.sha256_b64 cert.s_message in
  let cert = string_of_cert cert in
  set_content "pki_fp" fp;
  set_textarea "data" cert;
  Js._false

let proceed step =
  let version = get_textarea "version" |> int_of_string in
  let group = get_textarea "group" in
  let$ e = document##getElementById (Js.string "compute_private_key") in
  let$ e = Dom_html.CoerceTo.input e in
  let key = Js.to_string e##.value in
  let certs = certs_of_string (get_textarea "certs") in
  let threshold = int_of_string (get_textarea "threshold") in
  let module G = (val Group.of_string ~version group : GROUP) in
  let module Trustees = (val Trustees.get_by_version version) in
  let module P = Trustees.MakePKI (G) (DirectRandom) in
  let module C = Trustees.MakeChannels (G) (DirectRandom) (P) in
  let module T = Trustees.MakePedersen (G) (DirectRandom) (P) (C) in
  match step with
  | 3 ->
     let polynomial = T.step3 certs key threshold in
     set_textarea "compute_data" (string_of_polynomial polynomial)
  | 5 ->
     let vinput = get_textarea "vinput" in
     let vinput = vinput_of_string vinput in
     let voutput = T.step5 certs key vinput in
     set_textarea "compute_data" (string_of_voutput G.write voutput)
  | _ ->
     alert "Unexpected state!"

let fill_interactivity () =
  let$ e = document##getElementById (Js.string "interactivity") in
  let step = int_of_string (get_textarea "step") in
  match step with
  | 0 ->
     set_element_display "data_form" "none";
     let t = document##createTextNode (Js.string (s_ "Waiting for the election administrator to set the threshold... Reload the page to check progress.")) in
     Dom.appendChild e t
  | 2 | 4 ->
     set_step (step / 2);
     set_element_display "data_form" "none";
     let t = document##createTextNode (Js.string (s_ "Waiting for the other trustees... Reload the page to check progress.")) in
     Dom.appendChild e t
  | 6 | 7 ->
     set_step 3;
     set_element_display "data_form" "none";
     let voutput = voutput_of_string Yojson.Safe.read_json (get_textarea "voutput") in
     let pk = Yojson.Safe.to_string voutput.vo_public_key.trustee_public_key in
     let fp = Platform.sha256_b64 pk in
     let msg = Printf.sprintf (f_ "Your job in the key establishment protocol is done! The fingerprint of your verification key is %s. Check that it is published by the server when the election is open. Your private key will be needed to decrypt the election result.") fp in
     let t = document##createTextNode (Js.string msg) in
     Dom.appendChild e t;
     set_element_display "div_instructions" "block"
  | 1 ->
     set_step 1;
     let b = Dom_html.createButton document in
     let t = document##createTextNode (Js.string (s_ "Generate private key")) in
     b##.onclick := Dom_html.handler (gen_cert e);
     Dom.appendChild b t;
     Dom.appendChild e b
  | 3 | 5 ->
     let explain = match step with
       | 3 -> s_ "Now, all the certificates of the trustees have been generated. Proceed to generate your share of the decryption key."
       | 5 -> s_ "Now, all the trustees have generated their secret shares. Proceed to the final checks so that the election can be validated."
       | _ -> failwith "impossible step"
     in
     set_step ((step + 1) / 2);
     set_explain explain;
     set_element_display "compute_form" "block";
     let$ e = document##getElementById (Js.string "compute_button") in
     e##.onclick := Dom_html.handler (fun _ -> proceed step; Js._false)
  | _ ->
     alert "Unexpected state!"

let () =
  Lwt.async (fun () ->
      let* _ = Js_of_ocaml_lwt.Lwt_js_events.onload () in
      let* () = Tool_js_i18n.auto_init "admin" in
      fill_interactivity ();
      Lwt.return_unit
    )
