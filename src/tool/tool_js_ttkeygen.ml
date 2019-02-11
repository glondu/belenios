(**************************************************************************)
(*                                BELENIOS                                *)
(*                                                                        *)
(*  Copyright © 2012-2018 Inria                                           *)
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
open Serializable_j
open Signatures
open Common
open Tool_js_common

let set_step i =
  js_ignore (
      document##getElementById (Js.string "current_step") >>= fun e ->
      e##.innerHTML := Js.string "";
      let t = Printf.sprintf "Step %d/3" i in
      let t = document##createTextNode (Js.string t) in
      Dom.appendChild e t;
      return_unit
    )

let set_explain str =
  js_ignore (
      document##getElementById (Js.string "explain") >>= fun e ->
      e##.innerHTML := Js.string "";
      let t = document##createTextNode (Js.string str) in
      Dom.appendChild e t;
      Dom.appendChild e (Dom_html.createBr document);
      return_unit
    )

let gen_cert e _ =
  let group = get_textarea "group" in
  let module G = (val Group.of_string group : GROUP) in
  let module P = Trustees.MakePKI (G) (DirectRandom) in
  let module C = Trustees.MakeChannels (G) (DirectRandom) (P) in
  let module T = Trustees.MakePedersen (G) (DirectRandom) (P) (C) in
  let key, cert = T.step1 () in
  e##.innerHTML := Js.string "";
  set_download "private_key" "text/plain" "private_key.txt" key;
  set_element_display "key_helper" "block";
  let cert = string_of_cert cert in
  set_textarea "data" cert;
  Js._false

let proceed step =
  let group = get_textarea "group" in
  document##getElementById (Js.string "compute_private_key") >>= fun e ->
  Dom_html.CoerceTo.input e >>= fun e ->
  let key = Js.to_string e##.value in
  let certs = certs_of_string (get_textarea "certs") in
  let threshold = int_of_string (get_textarea "threshold") in
  let module G = (val Group.of_string group : GROUP) in
  let module P = Trustees.MakePKI (G) (DirectRandom) in
  let module C = Trustees.MakeChannels (G) (DirectRandom) (P) in
  let module T = Trustees.MakePedersen (G) (DirectRandom) (P) (C) in
  match step with
  | 3 ->
     let polynomial = T.step3 certs key threshold in
     set_textarea "compute_data" (string_of_polynomial polynomial);
     return_unit
  | 5 ->
     let vinput = get_textarea "vinput" in
     let vinput = vinput_of_string vinput in
     let voutput = T.step5 certs key vinput in
     set_textarea "compute_data" (string_of_voutput G.write voutput);
     return_unit
  | _ ->
     alert "Unexpected state!";
     return_unit

let main () =
  document##getElementById (Js.string "interactivity") >>= fun e ->
  let step = int_of_string (get_textarea "step") in
  match step with
  | 0 ->
     set_element_display "data_form" "none";
     let t = document##createTextNode (Js.string "Waiting for the election administrator to set the threshold... Reload the page to check progress.") in
     Dom.appendChild e t;
     return_unit
  | 2 | 4 ->
     set_step (step / 2);
     set_element_display "data_form" "none";
     let t = document##createTextNode (Js.string "Waiting for the other trustees... Reload the page to check progress.") in
     Dom.appendChild e t;
     return_unit
  | 6 | 7 ->
     set_step 3;
     set_element_display "data_form" "none";
     let t = document##createTextNode (Js.string "Your job in the key establishment protocol is done! Please download your ") in
     Dom.appendChild e t;
     let a = document##createTextNode (Js.string "public key") in
     let t = Dom_html.createA document in
     t##.id := Js.string "public_key";
     Dom.appendChild t a;
     Dom.appendChild e t;
     let t = document##createTextNode (Js.string " and check that it is in the public threshold parameters when the election is open. Your private key will be needed to decrypt the election result.") in
     Dom.appendChild e t;
     let group = get_textarea "group" in
     let module G = (val Group.of_string group : GROUP) in
     let voutput = voutput_of_string G.read (get_textarea "voutput") in
     set_download "public_key" "application/json" "public_key.json" (string_of_group_element G.write voutput.vo_public_key.trustee_public_key);
     return_unit
  | 1 ->
     set_step 1;
     let b = Dom_html.createButton document in
     let t = document##createTextNode (Js.string "Generate private key") in
     b##.onclick := Dom_html.handler (gen_cert e);
     Dom.appendChild b t;
     Dom.appendChild e b;
     return_unit
  | 3 | 5 ->
     let explain = match step with
       | 3 -> "Now, all the certificates of the trustees have been generated. Proceed to generate your share of the decryption key."
       | 5 -> "Now, all the trustees have generated their secret shares. Proceed to the final checks so that the election can be validated."
       | _ -> failwith "impossible step"
     in
     set_step ((step + 1) / 2);
     set_explain explain;
     set_element_display "compute_form" "block";
     document##getElementById (Js.string "compute_button") >>= fun e ->
     e##.onclick := Dom_html.handler (fun _ -> wrap_for_handler (proceed step));
     return_unit
  | _ ->
     alert "Unexpected state!";
     return_unit

let () =
  Dom_html.window##.onload := Dom_html.handler (fun _ -> wrap_for_handler (main ()))
