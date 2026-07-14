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

open Lwt.Syntax
open Js_of_ocaml
open Js_of_ocaml_tyxml
open Tyxml_js.Html5
open Belenios
open Belenios_web_api
open Belenios_js.Common
open Belenios_js.Session
open Common

type keypair = {
  private_key : string;
  public_key : json;
  fingerprint : string;
  mime_type : string;
  filename : uuid -> string;
}

let generate_basic (type a b) (w : (a, b) group) ~name ~uuid ~token ~index () =
  let module G = (val w) in
  let module Trustees = (val Trustees.get_by_version G.spec.version) in
  let module KG = Trustees.MakeBasic (G) in
  let seed = generate_token 44 in
  Common.seed := Some seed;
  let public_key' = KG.make ~name seed in
  let public_key = public_key' |> [%yojson_of_group: _ basic_parameters] in
  let fingerprint =
    public_key'
    |> (fun x -> x.cert.message.verification)
    |> G.to_string |> sha256_b64
  in
  let mime_type = "application/json"
  and filename uuid =
    Printf.sprintf "private_key-%s-%d.json" (Uuid.to_string uuid) index
  and private_key =
    !+yojson_of_stored_private_key { uuid; index; token; seed }
  in
  { private_key; public_key; fingerprint; mime_type; filename }

let generate_threshold (type a b) (w : (a, b) group) context ~uuid ~token ~index
    () =
  let module G = (val w) in
  let module Trustees = (val Trustees.get_by_version G.spec.version) in
  let module P = Pki.Make (G) in
  let module C = Pki.MakeChannels (P) in
  let module T = Trustees.MakePedersen (C) in
  let seed, cert = T.step1 context in
  Common.seed := Some seed;
  let fingerprint =
    sha256_b64
    @@ !+(yojson_of_cert_keys !&G.to_string
            (yojson_of_pedersen_context yojson_of_index))
         cert.message
  in
  let public_key = [%yojson_of_group: _ pedersen_cert] cert in
  let mime_type = "application/json"
  and filename uuid =
    Printf.sprintf "private_key-%s-%d.json" (Uuid.to_string uuid) index
  and private_key =
    !+yojson_of_stored_private_key { uuid; index; token; seed }
  in
  { private_key; public_key; fingerprint; mime_type; filename }

let threshold_step (type a b) (w : (a, b) group) (pedersen : (a, b) pedersen)
    ~private_key =
  let module G = (val w) in
  let context = pedersen.context.context in
  let certs = { context; certs = pedersen.certs } in
  let module Trustees = (val Trustees.get_by_version G.spec.version) in
  let module P = Pki.Make (G) in
  let module C = Pki.MakeChannels (P) in
  let module T = Trustees.MakePedersen (C) in
  match pedersen.step with
  | 3 ->
      let* x = T.step3 certs private_key in
      Lwt.return @@ [%yojson_of_group: _ polynomial] x
  | 5 ->
      let@ vinput cont =
        match pedersen.vinput with
        | Some x -> cont x
        | None -> failwith "Unexpected state! (missing vinput)"
      in
      let* x = T.step5 certs private_key vinput in
      Lwt.return @@ [%yojson_of_group: _ voutput] x
  | _ -> failwith "Unexpected state!"

let transient container make_elt =
  let dom = ref None in
  let remove () =
    match !dom with None -> () | Some x -> Dom.removeChild container x
  in
  let elt = make_elt remove in
  let x = Tyxml_js.To_dom.of_node elt in
  dom := Some x;
  Dom.appendChild container x

let generate_key ~uuid ~token ~url generate container =
  let open (val !Belenios_js.I18n.gettext) in
  let@ remove_generate = transient container in
  let@ () = button ~a:[ a_id "generate_key" ] @@ s_ "Generate a key" in
  let@ () = finally Lwt.return_unit in
  let p = generate () in
  remove_generate ();
  let@ remove_submit = transient container in
  let btn_submit =
    let@ () =
      button ~a:[ a_id "submit_public_key"; a_disabled () ]
      @@ s_ "Submit public key"
    in
    let* x = Api.(post url (`Trustee token) p.public_key) in
    match x.code with
    | 200 ->
        remove_submit ();
        appendElements container
          [
            div
              ~a:[ a_id "success" ]
              [ txt @@ s_ "Public key registration succeeded!" ];
          ];
        Lwt.return_unit
    | _ ->
        appendElements container
          [
            div
              ~a:[ a_id "failure" ]
              [
                txt
                @@ s_
                     "Public key registration failed! Please refresh and/or \
                      restart from the beginning.";
              ];
          ];
        Lwt.return_unit
  in
  let download =
    let onclick _ =
      (Tyxml_js.To_dom.of_button btn_submit)##.disabled := Js._false;
      true
    in
    a_data
      ~a:[ a_id "private_key"; a_onclick onclick ]
      ~mime_type:p.mime_type ~filename:(p.filename uuid) ~data:p.private_key
    @@ s_ "private_key"
  in
  div
    [
      div
        [
          txt @@ s_ "Fingerprint of the public key:"; txt " "; txt p.fingerprint;
        ];
      div
        [
          b [ txt @@ s_ "Instructions:" ];
          ol
            [
              li
                [
                  txt (s_ "Download your ");
                  download;
                  txt @@ s_ " and save it to a secure location.";
                  br ();
                  txt @@ s_ "You will use it to decrypt the final result.";
                ];
              li
                [
                  txt @@ s_ "Save the fingerprint above.";
                  br ();
                  txt
                  @@ s_
                       "Once the election is open, you must check that it is \
                        present in the set of public keys published by the \
                        server.";
                ];
              li [ txt @@ s_ "Submit your public key using the button below." ];
            ];
        ];
      div [ btn_submit ];
    ]

let compute_threshold_step ~token ~url w (pedersen : _ pedersen) container =
  let open (val !Belenios_js.I18n.gettext) in
  match pedersen.step with
  | 3 | 5 -> (
      let private_key = Option.get !Common.seed in
      let* data = threshold_step w pedersen ~private_key in
      let* x = Api.(post url (`Trustee token) data) in
      match x.code with
      | 200 ->
          appendElements container
            [
              div
                ~a:[ a_id "success" ]
                [ txt @@ s_ "Data submission succeeded!" ];
              div [ txt @@ s_ "Waiting for the other trustees..." ];
            ];
          Lwt.return_unit
      | _ ->
          appendElements container
            [
              div
                ~a:[ a_id "failure" ]
                [
                  txt
                  @@ s_
                       "Data submission failed! Please refresh and/or restart \
                        from the beginning.";
                ];
            ];
          Lwt.return_unit)
  | 4 ->
      appendElements container
        [ div [ txt @@ s_ "Waiting for the other trustees..." ] ];
      Lwt.return_unit
  | 6 | 7 ->
      appendElements container
        [
          div
            [
              txt
              @@ s_
                   "Your job in the key establishment protocol is done! Your \
                    private key will be needed to decrypt the election result.";
            ];
        ];
      Lwt.return_unit
  | _ ->
      appendElements container [ div [ txt @@ s_ "Inconsistent state!" ] ];
      Lwt.return_unit

let actionable_basic ~uuid ~token ~index ~url w container = function
  | `Init name ->
      generate_key ~uuid ~token ~url
        (generate_basic w ~name ~uuid ~token ~index)
        container
  | `Done vk ->
      let open (val !Belenios_js.I18n.gettext) in
      appendElements container
        [ div [ txt @@ s_ "Your public key has already been registered!" ] ];
      let@ () = load_and_check_private_key w vk container in
      appendElements container
        [ div [ txt @@ s_ "Your private key is valid!" ] ]

let actionable_threshold ~uuid ~token ~index ~url w container set_step =
  function
  | `Init ->
      set_step 0;
      setup_pedersen_notifications uuid ~token;
      let open (val !Belenios_js.I18n.gettext) in
      appendElements container
        [
          div
            [
              txt
              @@ s_
                   "Waiting for the election administrator to set the \
                    threshold...";
            ];
        ]
  | `WaitingForCertificate context ->
      set_step 1;
      setup_pedersen_notifications uuid ~token;
      generate_key ~uuid ~token ~url
        (generate_threshold w context ~uuid ~token ~index)
        container
  | `WaitingForOtherCertificates vk ->
      set_step 2;
      setup_pedersen_notifications uuid ~token;
      let open (val !Belenios_js.I18n.gettext) in
      appendElements container
        [ div [ txt @@ s_ "Your public key has already been registered!" ] ];
      let@ () = load_and_check_private_key w vk container in
      appendElements container
        [
          div [ txt @@ s_ "Your private key is valid!" ];
          div [ txt @@ s_ "Waiting for the other trustees..." ];
        ]
  | `Pedersen (p : _ pedersen) ->
      set_step p.step;
      if p.step < 7 then setup_pedersen_notifications uuid ~token;
      let vk = p.certs.(p.context.index - 1).message.verification in
      let open (val !Belenios_js.I18n.gettext) in
      appendElements container
        [ div [ txt @@ s_ "Your public key has already been registered!" ] ];
      let@ () = load_and_check_private_key w vk container in
      appendElements container
        [ div [ txt @@ s_ "Your private key is valid!" ] ];
      let@ () = Lwt.async in
      compute_threshold_step ~token ~url w p container

let generate uuid ~token ~index (type a b) (w : (a, b) group)
    (status : _ trustee_status_draft) =
  let open (val !Belenios_js.I18n.gettext) in
  let url = Api.trustees_trustee uuid w in
  let actionable = div [] in
  let container = Tyxml_js.To_dom.of_node actionable in
  let header =
    match status with
    | `Basic s ->
        actionable_basic ~uuid ~token ~index ~url w container s;
        h3 [ txt @@ s_ "Trustee key generation" ]
    | `Threshold s ->
        let h, set_step =
          let h = h3 [] in
          let container = Tyxml_js.To_dom.of_h3 h in
          ( h,
            fun step ->
              let step = Printf.sprintf (f_ "Step %d/7") step in
              container##.textContent :=
                Js.some @@ Js.string
                @@ s_ "Collaborative key generation"
                ^^^ step )
        in
        actionable_threshold ~uuid ~token ~index ~url w container set_step s;
        h
  in
  Lwt.return [ header; hr (); actionable ]
