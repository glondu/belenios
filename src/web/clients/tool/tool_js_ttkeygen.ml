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
open Belenios
open Belenios_js.Common
open Belenios_api.Serializable_j
open Belenios_api.Common

let set_step i =
  let open (val !Belenios_js.I18n.gettext) in
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

let gen_cert context (Draft (_, draft)) e _ =
  let version = draft.draft_version in
  let group = draft.draft_group in
  let module G = (val Group.of_string ~version group : GROUP) in
  let module Trustees = (val Trustees.get_by_version version) in
  let module P = Trustees.MakePKI (G) (Random) in
  let module C = Trustees.MakeChannels (G) (Random) (P) in
  let module T = Trustees.MakePedersen (G) (Random) (P) (C) in
  Lwt.async (fun () ->
      let key, cert = T.step1 context in
      clear_content e;
      set_download "private_key" "text/plain" "private_key.txt" key;
      set_element_display "key_helper" "block";
      let fp = sha256_b64 cert.s_message in
      let cert = string_of_cert (swrite G.Zq.to_string) cert in
      set_content "pki_fp" fp;
      set_textarea "data" cert;
      Lwt.return_unit);
  Js._false

let proceed (Draft (_, draft)) pedersen =
  let version = draft.draft_version in
  let group = draft.draft_group in
  let module G = (val Group.of_string ~version group : GROUP) in
  let pedersen =
    pedersen
    |> string_of_pedersen Yojson.Safe.write_json Yojson.Safe.write_json
    |> pedersen_of_string (sread G.of_string) (sread G.Zq.of_string)
  in
  let$ e = document##getElementById (Js.string "compute_private_key") in
  let$ e = Dom_html.CoerceTo.input e in
  let key = Js.to_string e##.value in
  let certs = pedersen.pedersen_certs in
  let module Trustees = (val Trustees.get_by_version version) in
  let module P = Trustees.MakePKI (G) (Random) in
  let module C = Trustees.MakeChannels (G) (Random) (P) in
  let module T = Trustees.MakePedersen (G) (Random) (P) (C) in
  Lwt.async (fun () ->
      match pedersen.pedersen_step with
      | 3 ->
          let polynomial = T.step3 certs key in
          set_textarea "compute_data"
            (string_of_polynomial (swrite G.Zq.to_string) polynomial);
          Lwt.return_unit
      | 5 ->
          let@ vinput cont =
            match pedersen.pedersen_vinput with
            | Some x -> cont x
            | None ->
                alert "Unexpected state! (missing vinput)";
                Lwt.return_unit
          in
          let voutput = T.step5 certs key vinput in
          set_textarea "compute_data"
            (string_of_voutput (swrite G.to_string) (swrite G.Zq.to_string)
               voutput);
          Lwt.return_unit
      | _ ->
          alert "Unexpected state!";
          Lwt.return_unit)

let fail msg =
  set_content "election_url" msg;
  Lwt.return_unit

let fill_interactivity () =
  let open (val !Belenios_js.I18n.gettext) in
  let&&* e = document##getElementById (Js.string "interactivity") in
  let@ uuid, token =
   fun cont ->
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
  let@ draft cont =
    let url = Printf.sprintf "../api/drafts/%s" uuid in
    let* x = get draft_of_string url in
    match x with Some x -> cont x | None -> fail "(token error)"
  in
  let@ status cont =
    let url = Printf.sprintf "../api/drafts/%s/trustee" uuid in
    let* x =
      get ~token
        (trustee_status_of_string Yojson.Safe.read_json Yojson.Safe.read_json)
        url
    in
    match x with Some (`Threshold x) -> cont x | _ -> fail "(error)"
  in
  let@ () =
   fun cont ->
    cont ();
    Lwt.return_unit
  in
  let step =
    match status with
    | `Init -> 0
    | `WaitingForCertificate _ -> 1
    | `WaitingForOtherCertificates -> 2
    | `Pedersen p -> p.pedersen_step
  in
  match step with
  | 0 ->
      set_element_display "data_form" "none";
      let t =
        document##createTextNode
          (Js.string
             (s_
                "Waiting for the election administrator to set the \
                 threshold... Reload the page to check progress."))
      in
      Dom.appendChild e t
  | 2 | 4 ->
      set_step (step / 2);
      set_element_display "data_form" "none";
      let t =
        document##createTextNode
          (Js.string
             (s_
                "Waiting for the other trustees... Reload the page to check \
                 progress."))
      in
      Dom.appendChild e t
  | 6 | 7 ->
      set_step 3;
      set_element_display "data_form" "none";
      let@ voutput cont =
        let fail () = alert "Unexpected state! (missing voutput)" in
        match status with
        | `Pedersen pedersen -> (
            match pedersen.pedersen_voutput with
            | Some x -> cont x
            | None -> fail ())
        | _ -> fail ()
      in
      let pk = Yojson.Safe.to_string voutput.vo_public_key.trustee_public_key in
      let fp = sha256_b64 pk in
      let msg =
        Printf.sprintf
          (f_
             "Your job in the key establishment protocol is done! The \
              fingerprint of your verification key is %s. Check that it is \
              published by the server when the election is open. Your private \
              key will be needed to decrypt the election result.")
          fp
      in
      let t = document##createTextNode (Js.string msg) in
      Dom.appendChild e t;
      set_element_display "div_instructions" "block"
  | 1 ->
      set_step 1;
      let b = Dom_html.createButton document in
      let t =
        document##createTextNode (Js.string (s_ "Generate private key"))
      in
      let context =
        match status with
        | `WaitingForCertificate c -> c
        | _ -> failwith "Unexpected state 1"
      in
      b##.onclick := Dom_html.handler (gen_cert context draft e);
      Dom.appendChild b t;
      Dom.appendChild e b
  | 3 | 5 ->
      let explain =
        match step with
        | 3 ->
            s_
              "Now, all the certificates of the trustees have been generated. \
               Proceed to generate your share of the decryption key."
        | 5 ->
            s_
              "Now, all the trustees have generated their secret shares. \
               Proceed to the final checks so that the election can be \
               validated."
        | _ -> failwith "impossible step"
      in
      set_step ((step + 1) / 2);
      set_explain explain;
      set_element_display "compute_form" "block";
      let$ e = document##getElementById (Js.string "compute_button") in
      let pedersen =
        match status with
        | `Pedersen p -> p
        | _ -> Printf.ksprintf failwith "Unexpected state %d" step
      in
      e##.onclick :=
        Dom_html.handler (fun _ ->
            proceed draft pedersen;
            Js._false)
  | _ -> alert "Unexpected state!"

let () =
  Lwt.async (fun () ->
      let* _ = Js_of_ocaml_lwt.Lwt_js_events.onload () in
      let* () = Belenios_js.I18n.auto_init "admin" in
      fill_interactivity ())
