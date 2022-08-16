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
open Belenios_core.Serializable_builtin_t
open Belenios_core
open Common
open Belenios
open Belenios_tool_js_common
open Serializable_j
open Signatures
open Tool_js_common
open Tool_js_i18n.Gettext
open Belenios_api.Serializable_j

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

let gen_cert draft e _ =
  let version = draft.draft_version in
  let group = draft.draft_group in
  let module G = (val Group.of_string ~version group : GROUP) in
  let module Trustees = (val Trustees.get_by_version version) in
  let module P = Trustees.MakePKI (G) (LwtJsRandom) in
  let module C = Trustees.MakeChannels (G) (LwtJsRandom) (P) in
  let module T = Trustees.MakePedersen (G) (LwtJsRandom) (P) (C) in
  Lwt.async (fun () ->
      let* key, cert = T.step1 () in
      clear_content e;
      set_download "private_key" "text/plain" "private_key.txt" key;
      set_element_display "key_helper" "block";
      let fp = sha256_b64 cert.s_message in
      let cert = string_of_cert cert in
      set_content "pki_fp" fp;
      set_textarea "data" cert;
      Lwt.return_unit
    );
  Js._false

let proceed draft pedersen =
  let version = draft.draft_version in
  let group = draft.draft_group in
  let$ e = document##getElementById (Js.string "compute_private_key") in
  let$ e = Dom_html.CoerceTo.input e in
  let key = Js.to_string e##.value in
  let certs = {certs = pedersen.pedersen_certs} in
  let threshold = pedersen.pedersen_threshold in
  let module G = (val Group.of_string ~version group : GROUP) in
  let module Trustees = (val Trustees.get_by_version version) in
  let module P = Trustees.MakePKI (G) (LwtJsRandom) in
  let module C = Trustees.MakeChannels (G) (LwtJsRandom) (P) in
  let module T = Trustees.MakePedersen (G) (LwtJsRandom) (P) (C) in
  Lwt.async (fun () ->
      match pedersen.pedersen_step with
      | 3 ->
         let* polynomial = T.step3 certs key threshold in
         set_textarea "compute_data" (string_of_polynomial polynomial);
         Lwt.return_unit
      | 5 ->
         let@ vinput = fun cont ->
           match pedersen.pedersen_vinput with
           | Some x -> cont x
           | None ->
              alert "Unexpected state! (missing vinput)";
              Lwt.return_unit
         in
         let* voutput = T.step5 certs key vinput in
         set_textarea "compute_data" (string_of_voutput G.write voutput);
         Lwt.return_unit
      | _ ->
         alert "Unexpected state!";
         Lwt.return_unit
    )

let ( let& ) x f =
  Js.Opt.case x (fun () -> Lwt.return_unit) f

let fail msg =
  set_content "election_url" msg;
  Lwt.return_unit

let fill_interactivity () =
  let& e = document##getElementById (Js.string "interactivity") in
  let@ uuid, token = fun cont ->
    let hash = Dom_html.window##.location##.hash |> Js.to_string in
    match extract_uuid_and_token hash with
    | Some (uuid, token) -> cont (uuid, token)
    | None -> fail "(uuid error)"
  in
  let@ () = redirect_if_admin "threshold-trustee" uuid token in
  set_form_target "data_form" "submit-threshold-trustee" uuid token;
  set_form_target "data_form_compute" "submit-threshold-trustee" uuid token;
  let href = Dom_html.window##.location##.href |> Js.to_string in
  set_content "election_url" (build_election_url href uuid);
  let@ draft = fun cont ->
    let url = Printf.sprintf "../../api/drafts/%s" uuid in
    let* x = get token draft_of_string url in
    match x with
    | Some x -> cont x
    | None -> fail "(token error)"
  in
  let@ pedersen = fun cont ->
    let url = Printf.sprintf "../../api/drafts/%s/trustees-pedersen" uuid in
    let* x = get token (pedersen_of_string Yojson.Safe.read_json) url in
    match x with
    | Some x -> cont x
    | None -> fail "(pedersen error)"
  in
  let step = pedersen.pedersen_step in
  let@ () = fun cont ->
    cont ();
    Lwt.return_unit
  in
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
     let@ voutput = fun cont ->
       match pedersen.pedersen_voutput with
       | Some x -> cont x
       | None -> alert "Unexpected state! (missing voutput)"
     in
     let pk = Yojson.Safe.to_string voutput.vo_public_key.trustee_public_key in
     let fp = sha256_b64 pk in
     let msg = Printf.sprintf (f_ "Your job in the key establishment protocol is done! The fingerprint of your verification key is %s. Check that it is published by the server when the election is open. Your private key will be needed to decrypt the election result.") fp in
     let t = document##createTextNode (Js.string msg) in
     Dom.appendChild e t;
     set_element_display "div_instructions" "block"
  | 1 ->
     set_step 1;
     let b = Dom_html.createButton document in
     let t = document##createTextNode (Js.string (s_ "Generate private key")) in
     b##.onclick := Dom_html.handler (gen_cert draft e);
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
     e##.onclick := Dom_html.handler (fun _ -> proceed draft pedersen; Js._false)
  | _ ->
     alert "Unexpected state!"

let () =
  Lwt.async (fun () ->
      let* _ = Js_of_ocaml_lwt.Lwt_js_events.onload () in
      let* () = Tool_js_i18n.auto_init "admin" in
      fill_interactivity ()
    )
