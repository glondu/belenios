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
open Tyxml_js.Html
open Belenios
open Belenios_js.Common
open Belenios_js.Session
open Belenios_js.Messages

(* We create the worker here, so that its libsodium wasm module
   initializes as soon as possible. *)
let worker = Worker.create !!"static/belenios_worker.js"

let compute_shuffle ~estimation election ciphertexts =
  let open (val !Belenios_js.I18n.gettext) in
  let t1, u1 = Lwt.task () in
  let t2, u2 = Lwt.task () in
  let onmessage2 =
    Dom.handler (fun e ->
        worker##.onmessage := Dom.no_handler;
        Lwt.wakeup_later u2 e##.data;
        Js._true)
  in
  let onmessage1 =
    Dom.handler (fun e ->
        worker##.onmessage := onmessage2;
        Lwt.wakeup_later u1 e##.data;
        Js._true)
  in
  worker##.onmessage := onmessage1;
  worker##postMessage (Shuffle { election; ciphertexts });
  let@ result cont =
    let* x = t1 in
    match x with
    | ShuffleEstimate eta ->
        let start = (new%js Js.date_now)##valueOf in
        let stop = start +. (float_of_int eta *. 1000.) in
        let update () =
          let now = (new%js Js.date_now)##valueOf in
          let eta = max 0 (int_of_float (ceil ((stop -. now) /. 1000.))) in
          estimation##.textContent :=
            Js.some @@ Js.string
            @@ Printf.sprintf (f_ "Estimated remaining time: %d second(s)") eta
        in
        let id = Dom_html.window##setInterval (Js.wrap_callback update) 500. in
        let* x = t2 in
        Dom_html.window##clearInterval id;
        cont x
    | x -> cont x
  in
  match result with
  | ShuffleResult result -> Lwt.return result
  | _ -> failwith "unexpected response from worker"

let shuffle uuid ~token =
  let open (val !Belenios_js.I18n.gettext) in
  let fail () =
    Lwt.return [ div [ txt @@ s_ "Error while loading election parameters!" ] ]
  in
  let@ election cont =
    let* x = Api.(get (election uuid) `Nobody) in
    match x with Ok (x, _) -> cont x | Error _ -> fail ()
  in
  let@ nh_ciphertexts cont =
    let* x = Api.(get (election_nh_ciphertexts uuid) `Nobody) in
    match x with Ok (x, _) -> cont x | Error _ -> fail ()
  in
  let container = Dom_html.createDiv document in
  let () =
    Dom.appendChild container @@ Tyxml_js.To_dom.of_div
    @@ div
         [
           txt
           @@ s_
                "As a trustee, your first role is to shuffle the encrypted \
                 ballots.";
         ]
  in
  let shuffle_div = Dom_html.createDiv document in
  let shuffle_btn =
    let@ () = button @@ s_ "Compute shuffle" in
    Dom.removeChild container shuffle_div;
    let estimation =
      div [ txt @@ s_ "Estimating computation time…" ] |> Tyxml_js.To_dom.of_div
    in
    let wait =
      div
        [
          txt @@ s_ "Please wait… ";
          img ~src:"encrypting.gif" ~alt:(s_ "Please wait…") ();
        ]
      |> Tyxml_js.To_dom.of_div
    in
    Dom.appendChild container estimation;
    Dom.appendChild container wait;
    let* shuffle_data =
      compute_shuffle ~estimation (Js.string election)
        (Js.string nh_ciphertexts)
    in
    let shuffle_data = Js.to_string shuffle_data in
    Dom.removeChild container estimation;
    Dom.removeChild container wait;
    let () =
      Dom.appendChild container @@ Tyxml_js.To_dom.of_node
      @@ div
           [
             div
               [
                 txt @@ s_ "The fingerprint of your shuffle is:";
                 txt " ";
                 b [ txt @@ sha256_b64 shuffle_data ];
                 txt ".";
               ];
             div
               [
                 txt
                 @@ s_
                      "You must record this fingerprint and check that it \
                       appears on the election result page.";
               ];
           ]
    in
    let submit_div = Dom_html.createDiv document in
    let submit_btn =
      let@ () = button @@ s_ "Submit" in
      Dom.removeChild container submit_div;
      let* x =
        Api.(post (trustee_election uuid) (`Trustee token) shuffle_data)
      in
      let msg =
        match x.code with
        | 200 -> s_ "The shuffle has been successfully applied!"
        | _ -> s_ "An error occurred while applying the shuffle."
      in
      let element = div ~a:[ a_id "success" ] [ txt msg ] in
      Dom.appendChild container (Tyxml_js.To_dom.of_div element);
      Lwt.return_unit
    in
    Dom.appendChild submit_div (Tyxml_js.To_dom.of_node submit_btn);
    Dom.appendChild container submit_div;
    Lwt.return_unit
  in
  Dom.appendChild shuffle_div (Tyxml_js.To_dom.of_node shuffle_btn);
  Dom.appendChild container shuffle_div;
  let title = h3 [ txt @@ s_ "Shuffle" ] in
  Lwt.return [ title; hr (); Tyxml_js.Of_dom.of_div container ]
