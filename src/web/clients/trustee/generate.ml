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
open Belenios_api.Serializable_j
open Belenios
open Belenios_api
open Belenios_js.Common
open Belenios_js.Session

type keypair = {
  private_key : string;
  public_key : string;
  fingerprint : string;
  mime_type : string;
  filename : string;
}

let generate_basic (Draft (_, draft)) () =
  let version = draft.draft_version in
  let group = draft.draft_group in
  let module G = (val Group.of_string ~version group : GROUP) in
  let module Trustees = (val Trustees.get_by_version version) in
  let module KG = Trustees.MakeSimple (G) (Random) in
  let private_key = KG.generate () in
  let public_key = KG.prove private_key in
  let private_key = private_key |> G.Zq.to_Z |> string_of_number in
  let public_key =
    public_key
    |> string_of_trustee_public_key (swrite G.to_string) (swrite G.Zq.to_string)
  in
  let fingerprint =
    public_key
    |> trustee_public_key_of_string Yojson.Safe.read_json Yojson.Safe.read_json
    |> (fun x -> x.trustee_public_key)
    |> Yojson.Safe.to_string |> sha256_b64
  in
  let mime_type = "application/json" and filename = "private_key.json" in
  Lwt.return { private_key; public_key; fingerprint; mime_type; filename }

let generate_threshold (Draft (_, draft)) context () =
  let version = draft.draft_version in
  let group = draft.draft_group in
  let module G = (val Group.of_string ~version group : GROUP) in
  let module Trustees = (val Trustees.get_by_version version) in
  let module P = Trustees.MakePKI (G) (Random) in
  let module C = Trustees.MakeChannels (G) (Random) (P) in
  let module T = Trustees.MakePedersen (G) (Random) (P) (C) in
  let private_key, cert = T.step1 context in
  let fingerprint = sha256_b64 cert.s_message in
  let public_key = string_of_cert (swrite G.Zq.to_string) cert in
  let mime_type = "text/plain" and filename = "private_key.txt" in
  Lwt.return { private_key; public_key; fingerprint; mime_type; filename }

let threshold_step (Draft (_, draft)) pedersen ~private_key =
  let version = draft.draft_version in
  let group = draft.draft_group in
  let module G = (val Group.of_string ~version group : GROUP) in
  let pedersen =
    pedersen
    |> string_of_pedersen Yojson.Safe.write_json Yojson.Safe.write_json
    |> pedersen_of_string (sread G.of_string) (sread G.Zq.of_string)
  in
  let certs = pedersen.pedersen_certs in
  let module Trustees = (val Trustees.get_by_version version) in
  let module P = Trustees.MakePKI (G) (Random) in
  let module C = Trustees.MakeChannels (G) (Random) (P) in
  let module T = Trustees.MakePedersen (G) (Random) (P) (C) in
  match pedersen.pedersen_step with
  | 3 ->
      T.step3 certs private_key
      |> string_of_polynomial (swrite G.Zq.to_string)
      |> Lwt.return
  | 5 ->
      let@ vinput cont =
        match pedersen.pedersen_vinput with
        | Some x -> cont x
        | None -> failwith "Unexpected state! (missing vinput)"
      in
      T.step5 certs private_key vinput
      |> string_of_voutput (swrite G.to_string) (swrite G.Zq.to_string)
      |> Lwt.return
  | _ -> failwith "Unexpected state!"

let generate_key ~token ~url generate =
  let open (val !Belenios_js.I18n.gettext) in
  let container = Dom_html.createDiv document in
  let doit = Dom_html.createButton document in
  doit##.id := Js.string "generate_key";
  doit##.textContent := Js.some @@ Js.string @@ s_ "Generate a key";
  let () =
    doit##.onclick :=
      let@ () = lwt_handler in
      Dom.removeChild container doit;
      let* p = generate () in
      let submit =
        let r = Dom_html.createButton document in
        r##.id := Js.string "submit_public_key";
        r##.disabled := Js._true;
        r##.textContent := Js.some @@ Js.string @@ s_ "Submit public key";
        let () =
          r##.onclick :=
            let@ () = lwt_handler in
            Dom.removeChild container r;
            let () = Api.set_token token in
            let* x = Api.(post url p.public_key) in
            let msg =
              match x.code with
              | 200 -> s_ "Public key registration succeeded!"
              | _ ->
                  s_
                    "Public key registration failed! Please refresh and/or \
                     restart from the beginning."
            in
            let element = div ~a:[ a_id "success" ] [ txt msg ] in
            Dom.appendChild container (Tyxml_js.To_dom.of_div element);
            Lwt.return_unit
        in
        r
      in
      let download =
        let href = encode_data_uri ~mime_type:p.mime_type p.private_key in
        let element =
          a ~href
            ~a:[ a_id "private_key"; a_download (Some p.filename) ]
            (s_ "private key")
        in
        let r = Tyxml_js.To_dom.of_a element in
        r##.onclick :=
          Dom_html.handler (fun _ ->
              submit##.disabled := Js._false;
              Js._true);
        element
      in
      let elements =
        [
          div
            [
              txt @@ s_ "Fingerprint of the verification key:";
              txt " ";
              txt p.fingerprint;
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
                           "Once the election is open, you must check that it \
                            is present in the set of public keys published by \
                            the server.";
                    ];
                  li
                    [
                      txt @@ s_ "Submit your public key using the button below.";
                    ];
                ];
            ];
        ]
      in
      List.iter
        (fun x -> Dom.appendChild container (Tyxml_js.To_dom.of_div x))
        elements;
      Dom.appendChild container submit;
      Lwt.return_unit
  in
  Dom.appendChild container doit;
  Lwt.return [ Tyxml_js.Of_dom.of_div container ]

let waiting_for_other_trustees () =
  let open (val !Belenios_js.I18n.gettext) in
  Lwt.return
  @@ [
       div
         [
           txt
           @@ s_
                "Waiting for the other trustees... Reload the page to check \
                 progress.";
         ];
     ]

let error () =
  let open (val !Belenios_js.I18n.gettext) in
  Lwt.return [ txt @@ s_ "Error" ]

let compute_threshold_step ~token ~url draft pedersen =
  let open (val !Belenios_js.I18n.gettext) in
  match pedersen.pedersen_step with
  | 3 | 5 ->
      let container = Dom_html.createDiv document in
      let input_private_key_div = Dom_html.createDiv document in
      let input_private_key, get_private_key =
        input ~a:[ a_id "compute_private_key" ] ""
      in
      let compute = Dom_html.createButton document in
      compute##.id := Js.string "compute_button";
      compute##.textContent := Js.some @@ Js.string @@ s_ "Proceed";
      let () =
        compute##.onclick :=
          let@ () = lwt_handler in
          let private_key = get_private_key () in
          Dom.removeChild container input_private_key_div;
          let* data = threshold_step draft pedersen ~private_key in
          let submit =
            let r = Dom_html.createButton document in
            r##.id := Js.string "submit_data";
            r##.textContent := Js.some @@ Js.string @@ s_ "Submit data";
            let () =
              r##.onclick :=
                let@ () = lwt_handler in
                Dom.removeChild container r;
                let () = Api.set_token token in
                let* x = Api.(post url data) in
                let msg =
                  match x.code with
                  | 200 ->
                      s_ "Data submission succeeded! Please refresh to proceed."
                  | _ ->
                      s_
                        "Data submission failed! Please refresh and/or restart \
                         from the beginning."
                in
                let element = div ~a:[ a_id "success" ] [ txt msg ] in
                Dom.appendChild container (Tyxml_js.To_dom.of_div element);
                Lwt.return_unit
            in
            r
          in
          Dom.appendChild container submit;
          Lwt.return_unit
      in
      let explain =
        match pedersen.pedersen_step with
        | 3 ->
            s_
              "Now, all the certificates of the trustees have been generated. \
               Proceed to generate your share of the decryption key."
        | 5 ->
            s_
              "Now, all the trustees have generated their secret shares. \
               Proceed to the final checks so that the election can be \
               validated."
        | _ -> s_ "Inconsistent state!"
      in
      let elements =
        [
          div [ txt explain ];
          hr ();
          div
            [ txt @@ s_ "Enter your private key:"; txt " "; input_private_key ];
          div [ Tyxml_js.Of_dom.of_button compute ];
        ]
      in
      List.iter
        (fun x ->
          Dom.appendChild input_private_key_div (Tyxml_js.To_dom.of_node x))
        elements;
      Dom.appendChild container input_private_key_div;
      Lwt.return [ Tyxml_js.Of_dom.of_div container ]
  | 4 -> waiting_for_other_trustees ()
  | 6 | 7 ->
      Lwt.return
        [
          div
            [
              txt
              @@ s_
                   "Your job in the key establishment protocol is done! Your \
                    private key will be needed to decrypt the election result.";
            ];
        ]
  | _ -> error ()

let actionable_basic ~token ~url draft = function
  | `Init -> generate_key ~token ~url (generate_basic draft)
  | `Done ->
      let open (val !Belenios_js.I18n.gettext) in
      Lwt.return
        [ div [ txt @@ s_ "Your public key has already been registered!" ] ]

let actionable_threshold ~token ~url draft = function
  | `Init ->
      let open (val !Belenios_js.I18n.gettext) in
      Lwt.return
        [
          div
            [
              txt
              @@ s_
                   "Waiting for the election administrator to set the \
                    threshold... Reload the page to check progress.";
            ];
        ]
  | `WaitingForCertificate context ->
      generate_key ~token ~url (generate_threshold draft context)
  | `WaitingForOtherCertificates -> waiting_for_other_trustees ()
  | `Pedersen p -> compute_threshold_step ~token ~url draft p

let generate configuration uuid ~token =
  let open (val !Belenios_js.I18n.gettext) in
  let@ draft cont =
    let* x = Api.(get ~notoken:true (draft uuid)) in
    match x with Error _ -> error () | Ok (x, _) -> cont x
  in
  let () = Api.set_token token in
  let url = Api.trustee_draft uuid in
  let* status = Api.(get url) in
  let status =
    match status with
    | Error _ -> None
    | Ok (x, _) ->
        Some
          (trustee_status_of_string Yojson.Safe.read_json Yojson.Safe.read_json
             x)
  in
  let* x =
    match status with
    | Some (`Basic s) ->
        let* a = actionable_basic ~token ~url draft s in
        let h = h3 [ txt @@ s_ "Trustee key generation" ] in
        Lwt.return_some (a, h)
    | Some (`Threshold s) ->
        let* a = actionable_threshold ~token ~url draft s in
        let step =
          match s with
          | `Init -> 0
          | `WaitingForCertificate _ | `WaitingForOtherCertificates -> 1
          | `Pedersen p -> (p.pedersen_step + 1) / 2
        in
        let step = Printf.sprintf (f_ "Step %d/3") step in
        let h = h3 [ txt @@ s_ "Collaborative key generation" ^^^ step ] in
        Lwt.return_some (a, h)
    | None -> Lwt.return_none
  in
  match x with
  | None -> error ()
  | Some (actionable, header) ->
      let url =
        Printf.sprintf "%selection#%s" configuration.uris.home
          (Uuid.unwrap uuid)
      in
      [
        header;
        hr ();
        div
          [
            txt @@ s_ "The link to the election will be:"; ul [ li [ txt url ] ];
          ];
        hr ();
      ]
      @ actionable
      |> Lwt.return
