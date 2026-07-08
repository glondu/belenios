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
open Belenios_js.Common
open Belenios_js.Session
open Belenios_web_api
open Common

let compute_partial_decryption (type a b) (election : (a, b) Election.u) trustee
    ~encrypted_tally ~private_key =
  let open (val !Belenios_js.I18n.gettext) in
  let module W = (val election) in
  let module G = W.G in
  let encrypted_tally =
    encrypted_tally_of_yojson !$W.G.of_string encrypted_tally
  in
  let module T = (val Trustees.get_by_version W.version) in
  let module P = Pki.Make (W.G) in
  let* sk, pdk =
    match trustee with
    | Some epk ->
        let algorithm = epk.algorithm in
        let epk = epk.private_key in
        let module C = Pki.MakeChannels (P) in
        let module Pedersen = T.MakePedersen (C) in
        let sk = P.derive_sk private_key and dk = P.derive_dk private_key in
        let vk = W.G.(g **~ sk) in
        let* epk = C.recv ~algorithm Pedersen.xch_decryption_key dk vk epk in
        Lwt.return (sk, epk.decryption_key)
    | None ->
        let module KG = T.MakeBasic (W.G) in
        Lwt.return (P.derive_sk private_key, KG.derive private_key)
  in
  W.E.compute_factor encrypted_tally ~sk ~pdk
  |> [%yojson_of_group: _ partial_decryption] |> Lwt.return

let decrypt ~token (type a b) (election : (a, b) Election.u)
    (trustee : (a, b) tally_trustee) =
  let open (val !Belenios_js.I18n.gettext) in
  let module W = (val election) in
  let module G = W.G in
  let fail () =
    Lwt.return [ div [ txt @@ s_ "Error while loading election parameters!" ] ]
  in
  let@ encrypted_tally cont =
    let* x = Api.(get (election_encrypted_tally W.uuid) `Nobody) in
    match x with Ok (x, _) -> cont x | Error _ -> fail ()
  in
  let container = Dom_html.createDiv document in
  let encrypted_tally_hash = sha256_b64 @@ Json.to_string encrypted_tally in
  let partial_decryption = ref `Null in
  let submit =
    let@ () = button ~a:[ a_id "submit_data"; a_disabled () ] @@ s_ "Submit" in
    let* x =
      Api.(
        post
          (election_trustee W.uuid (module G))
          (`Trustee token) !partial_decryption)
    in
    let msg =
      match x.code with
      | 200 -> s_ "Your partial decryption has been received and checked!"
      | 400 -> s_ "The partial decryption didn't pass validation!"
      | 409 -> s_ "You have already submitted a valid partial decryption!"
      | 412 -> s_ "The election result is already published!"
      | _ -> s_ "Partial decryption submission failed unexpectedly!"
    in
    let element = div ~a:[ a_id "success" ] [ txt msg ] in
    Dom.appendChild container (Tyxml_js.To_dom.of_div element);
    Lwt.return_unit
  in
  let handle_private_key private_key =
    let* pd =
      compute_partial_decryption election trustee ~encrypted_tally ~private_key
    in
    partial_decryption := pd;
    let r = Tyxml_js.To_dom.of_button submit in
    r##.disabled := Js._false;
    Lwt.return_unit
  in
  let input_private_key = make_private_key_input handle_private_key in
  let compute = txt @@ s_ "Generate your contribution to decryption" in
  let title = h3 [ txt @@ s_ "Partial decryption" ] in
  let contents =
    [
      p
        [
          txt @@ s_ "It is now time to compute your partial decryption factors.";
        ];
      p
        [
          txt @@ s_ "The fingerprint of the encrypted tally is ";
          b [ span ~a:[ a_id "hash" ] [ txt encrypted_tally_hash ] ];
          txt ".";
        ];
      hr ();
      div
        [
          b [ txt @@ s_ "Instructions:" ];
          ol
            [
              li [ input_private_key; br () ];
              li [ div [ compute ]; br () ];
              li
                [
                  div
                    ~a:[ a_id "pd_done" ]
                    [
                      div
                        ~a:[ a_id "pd_form" ]
                        [
                          div
                            [
                              submit;
                              txt @@ s_ " your contribution to decryption.";
                            ];
                        ];
                    ];
                ];
            ];
        ];
    ]
  in
  List.iter
    (fun x -> Dom.appendChild container (Tyxml_js.To_dom.of_node x))
    contents;
  Lwt.return [ title; hr (); Tyxml_js.Of_dom.of_div container ]
