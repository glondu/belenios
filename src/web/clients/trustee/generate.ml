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
  public_key : string;
  fingerprint : string;
  mime_type : string;
  filename : uuid -> string;
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
  let mime_type = "application/json"
  and filename uuid = Printf.sprintf "private_key-%s.json" (Uuid.unwrap uuid) in
  Lwt.return { private_key; public_key; fingerprint; mime_type; filename }

let generate_threshold (Draft (_, draft)) context () =
  let version = draft.draft_version in
  let group = draft.draft_group in
  let module G = (val Group.of_string ~version group : GROUP) in
  let module Trustees = (val Trustees.get_by_version version) in
  let module P = Pki.Make (G) (Random) in
  let module C = Pki.MakeChannels (P) in
  let module T = Trustees.MakePedersen (C) in
  let private_key, cert = T.step1 context in
  let fingerprint = sha256_b64 cert.s_message in
  let public_key = string_of_cert (swrite G.Zq.to_string) cert in
  let mime_type = "text/plain"
  and filename uuid = Printf.sprintf "private_key-%s.txt" (Uuid.unwrap uuid) in
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
  let module P = Pki.Make (G) (Random) in
  let module C = Pki.MakeChannels (P) in
  let module T = Trustees.MakePedersen (C) in
  match pedersen.pedersen_step with
  | 3 ->
      let* x = T.step3 certs private_key in
      Lwt.return @@ string_of_polynomial (swrite G.Zq.to_string) x
  | 5 ->
      let@ vinput cont =
        match pedersen.pedersen_vinput with
        | Some x -> cont x
        | None -> failwith "Unexpected state! (missing vinput)"
      in
      let* x = T.step5 certs private_key vinput in
      Lwt.return
      @@ string_of_voutput (swrite G.to_string) (swrite G.Zq.to_string) x
  | _ -> failwith "Unexpected state!"

let generate_key ~uuid ~token ~url generate continue =
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
            let* x = Api.(post url (`Trustee token) p.public_key) in
            let msg =
              match x.code with
              | 200 -> s_ "Public key registration succeeded!"
              | _ ->
                  s_
                    "Public key registration failed! Please refresh and/or \
                     restart from the beginning."
            in
            let@ () = show_in container in
            [ [ div ~a:[ a_id "success" ] [ txt msg ] ]; continue p ]
            |> List.flatten |> Lwt.return
        in
        r
      in
      let download =
        let element =
          a_data
            ~a:[ a_id "private_key" ]
            ~mime_type:p.mime_type ~filename:(p.filename uuid)
            ~data:p.private_key
          @@ s_ "private_key"
        in
        let r = Tyxml_js.To_dom.of_a element in
        let () =
          r##.onclick :=
            let@ _ = Dom_html.handler in
            submit##.disabled := Js._false;
            Js._true
        in
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

let make_refresh_status_button () =
  let open (val !Belenios_js.I18n.gettext) in
  let t, u = Lwt.task () in
  let button =
    Tyxml_js.Html.button
      ~a:
        [
          a_onclick (fun _ ->
              Lwt.wakeup_later u ();
              true);
        ]
      [ txt @@ s_ "Refresh status" ]
  in
  (button, t)

let wait_for_other_trustees () =
  let open (val !Belenios_js.I18n.gettext) in
  let refresh_status, t = make_refresh_status_button () in
  ( [
      div
        [
          txt @@ s_ "Waiting for the other trustees..."; txt " "; refresh_status;
        ];
    ],
    Some t )

let error () =
  let open (val !Belenios_js.I18n.gettext) in
  Lwt.return [ txt @@ s_ "Error" ]

let compute_threshold_step ~token ~url draft private_key_ref pedersen =
  let open (val !Belenios_js.I18n.gettext) in
  match pedersen.pedersen_step with
  | 3 | 5 ->
      let refresh_status, t = make_refresh_status_button () in
      let container = Dom_html.createDiv document in
      let input_private_key_div = Dom_html.createDiv document in
      let handle_private_key private_key =
        Dom.removeChild container input_private_key_div;
        private_key_ref := Some private_key;
        let* data = threshold_step draft pedersen ~private_key in
        let submit =
          let r = Dom_html.createButton document in
          r##.id := Js.string "submit_data";
          r##.textContent := Js.some @@ Js.string @@ s_ "Submit data";
          let () =
            r##.onclick :=
              let@ () = lwt_handler in
              Dom.removeChild container r;
              let* x = Api.(post url (`Trustee token) data) in
              let msg, continue =
                match x.code with
                | 200 ->
                    (s_ "Data submission succeeded!", [ div [ refresh_status ] ])
                | _ ->
                    ( s_
                        "Data submission failed! Please refresh and/or restart \
                         from the beginning.",
                      [] )
              in
              let@ () = show_in container in
              [ [ div ~a:[ a_id "success" ] [ txt msg ] ]; continue ]
              |> List.flatten |> Lwt.return
          in
          r
        in
        Dom.appendChild container submit;
        Lwt.return_unit
      in
      let input_private_key =
        match !private_key_ref with
        | None -> make_private_key_input handle_private_key
        | Some p ->
            div [ button (s_ "Proceed") (fun () -> handle_private_key p) ]
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
      let* () =
        let@ () = show_in input_private_key_div in
        [ div [ txt explain ]; hr (); input_private_key ] |> Lwt.return
      in
      Dom.appendChild container input_private_key_div;
      Lwt.return ([ Tyxml_js.Of_dom.of_div container ], Some t)
  | 4 ->
      let contents, t = wait_for_other_trustees () in
      Lwt.return (contents, t)
  | 6 | 7 ->
      Lwt.return
        ( [
            div
              [
                txt
                @@ s_
                     "Your job in the key establishment protocol is done! Your \
                      private key will be needed to decrypt the election \
                      result.";
              ];
          ],
          None )
  | _ ->
      let* contents = error () in
      Lwt.return (contents, None)

let actionable_basic ~uuid ~token ~url draft = function
  | `Init -> generate_key ~uuid ~token ~url (generate_basic draft) (fun _ -> [])
  | `Done ->
      let open (val !Belenios_js.I18n.gettext) in
      Lwt.return
        [ div [ txt @@ s_ "Your public key has already been registered!" ] ]

let get_status ~url ~token =
  let* status = Api.(get url (`Trustee token)) in
  match status with
  | Error _ -> Lwt.return_none
  | Ok (x, _) ->
      Lwt.return_some
      @@ trustee_status_of_string Yojson.Safe.read_json Yojson.Safe.read_json x

let actionable_threshold ~uuid ~token ~url draft set_step s =
  let open (val !Belenios_js.I18n.gettext) in
  let container = Dom_html.createDiv document in
  let private_key = ref None in
  let get_contents = function
    | `Init ->
        let refresh_status, t = make_refresh_status_button () in
        Lwt.return
          ( 0,
            [
              div
                [
                  txt
                  @@ s_
                       "Waiting for the election administrator to set the \
                        threshold...";
                  txt " ";
                  refresh_status;
                ];
            ],
            Some t )
    | `WaitingForCertificate context ->
        let refresh_status, t = make_refresh_status_button () in
        let continue p =
          private_key := Some p.private_key;
          [ refresh_status ]
        in
        let* contents =
          generate_key ~uuid ~token ~url
            (generate_threshold draft context)
            continue
        in
        Lwt.return (1, contents, Some t)
    | `WaitingForOtherCertificates ->
        let contents, t = wait_for_other_trustees () in
        Lwt.return (1, contents, t)
    | `Pedersen p ->
        let* contents, t =
          compute_threshold_step ~token ~url draft private_key p
        in
        Lwt.return (p.pedersen_step, contents, t)
  in
  let rec loop s =
    let* step, contents, continue = get_contents s in
    set_step step;
    let* () =
      let@ () = show_in container in
      Lwt.return contents
    in
    match continue with
    | None -> Lwt.return_unit
    | Some t -> (
        let* () = t in
        let* status = get_status ~url ~token in
        match status with
        | Some (`Threshold s) -> loop s
        | _ ->
            let@ () = show_in container in
            [ div [ txt @@ s_ "Inconsistent state. Please refresh the page." ] ]
            |> Lwt.return)
  in
  Lwt.async (fun () -> loop s);
  Lwt.return [ Tyxml_js.Of_dom.of_div container ]

let generate configuration uuid ~token =
  let open (val !Belenios_js.I18n.gettext) in
  let@ draft cont =
    let* x = Api.(get (draft uuid) `Nobody) in
    match x with Error _ -> error () | Ok (x, _) -> cont x
  in
  let url = Api.trustee_draft uuid in
  let* status = get_status ~url ~token in
  let* x =
    match status with
    | Some (`Basic s) ->
        let* a = actionable_basic ~uuid ~token ~url draft s in
        let h = h3 [ txt @@ s_ "Trustee key generation" ] in
        Lwt.return_some (a, h)
    | Some (`Threshold s) ->
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
        let* a = actionable_threshold ~uuid ~token ~url draft set_step s in
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
