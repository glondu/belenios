(**************************************************************************)
(*                                BELENIOS                                *)
(*                                                                        *)
(*  Copyright © 2012-2017 Inria                                           *)
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

open Platform
open Serializable_j
open Signatures
open Common
open Tool_js_common

let gen_cert e _ =
  let group = get_textarea "group" in
  let module G = (val Group.of_string group : GROUP) in
  let module P = Trustees.MakePKI (G) (DirectRandom) in
  let module C = Trustees.MakeChannels (G) (DirectRandom) (P) in
  let module T = Trustees.MakePedersen (G) (DirectRandom) (P) (C) in
  let key, cert = T.step1 () in
  let id = sha256_hex cert.s_message in
  e##innerHTML <- Js.string "";
  let t = document##createTextNode (Js.string (Printf.sprintf "Certificate %s has been generated!" id)) in
  Dom.appendChild e t;
  set_download "private_key" "text/plain" "private_key.txt" key;
  set_element_display "key_helper" "block";
  let cert = string_of_cert cert in
  set_textarea "data" cert;
  Js._false

let proceed step e textarea _ =
  let group = get_textarea "group" in
  let key =
    let r = ref "" in
    Js.Opt.iter (Dom_html.CoerceTo.textarea textarea) (fun x -> r := Js.to_string x##value);
    !r
  in
  let certs = certs_of_string (get_textarea "certs") in
  let threshold = int_of_string (get_textarea "threshold") in
  let module G = (val Group.of_string group : GROUP) in
  let module P = Trustees.MakePKI (G) (DirectRandom) in
  let module C = Trustees.MakeChannels (G) (DirectRandom) (P) in
  let module T = Trustees.MakePedersen (G) (DirectRandom) (P) (C) in
  match step with
  | 3 ->
     let polynomial = T.step3 certs key threshold in
     e##innerHTML <- Js.string "";
     set_textarea "data" (string_of_polynomial polynomial);
     Js._false
  | 5 ->
     let vinput = get_textarea "vinput" in
     let vinput = vinput_of_string vinput in
     let voutput = T.step5 certs key vinput in
     e##innerHTML <- Js.string "";
     set_textarea "data" (string_of_voutput G.write voutput);
     Js._false
  | _ ->
     alert "Unexpected state!";
     Js._false

let fill_interactivity _ =
  Js.Opt.iter
    (document##getElementById (Js.string "interactivity"))
    (fun e ->
      let step = int_of_string (get_textarea "step") in
      match step with
      | 0 ->
         hide_element_by_id "data_form";
         let t = document##createTextNode (Js.string "Waiting for the election administrator to set the threshold... Reload the page to check progress.") in
         Dom.appendChild e t
      | 2 | 4 | 6 ->
         hide_element_by_id "data_form";
         let t = document##createTextNode (Js.string "Waiting for the other trustees... Reload the page to check progress.") in
         Dom.appendChild e t
      | 7 ->
         hide_element_by_id "data_form";
         let t = document##createTextNode (Js.string "The key establishment protocol is finished!") in
         Dom.appendChild e t
      | 1 ->
         let b = document##createElement (Js.string "button") in
         let t = document##createTextNode (Js.string "Generate private key") in
         b##onclick <- Dom_html.handler (gen_cert e);
         Dom.appendChild b t;
         Dom.appendChild e b;
      | 3 | 5 ->
         let div = document##createElement (Js.string "div") in
         let t = document##createTextNode (Js.string "Private key: ") in
         Dom.appendChild div t;
         let textarea = Dom_html.createTextarea document in
         textarea##rows <- 1;
         textarea##cols <- 25;
         Dom.appendChild div textarea;
         Dom.appendChild e div;
         let b = document##createElement (Js.string "button") in
         let t = document##createTextNode (Js.string "Proceed") in
         b##onclick <- Dom_html.handler (proceed step e textarea);
         Dom.appendChild b t;
         Dom.appendChild e b;
      | _ ->
         alert "Unexpected state!"
    );
  Js._false

let () =
  Dom_html.window##onload <- Dom_html.handler fill_interactivity;
