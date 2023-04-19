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
open Js_of_ocaml_lwt
open Belenios_core
open Common
open Belenios_js.Common
open Belenios_js.Messages

(* We create the worker here, so that its libsodium wasm module
   initializes as soon as possible. *)
let worker = Worker.create "../static/belenios_worker.js"

let shuffle election ciphertexts =
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
          clear_content_by_id "estimation";
          set_content "estimation"
            (Printf.sprintf (f_ "Estimated remaining time: %d second(s)") eta)
        in
        let id = Dom_html.window##setInterval (Js.wrap_callback update) 500. in
        let* x = t2 in
        Dom_html.window##clearInterval id;
        cont x
    | x -> cont x
  in
  clear_content_by_id "estimation";
  match result with
  | ShuffleResult result -> Lwt.return result
  | _ -> Lwt.fail (Failure "unexpected response from worker")

let set_nh_ciphertexts_link uuid =
  let href =
    [ ("uuid", uuid) ] |> Url.encode_arguments |> fun x ->
    Printf.sprintf "nh-ciphertexts?%s" x
  in
  let$ a = document##getElementById (Js.string "nh_ciphertexts_link") in
  let$ a = Dom_html.CoerceTo.a a in
  a##.href := Js.string href

let () =
  Lwt.async (fun () ->
      let* _ = Lwt_js_events.onload () in
      let* () = Belenios_js.I18n.auto_init "admin" in
      let@ uuid, token =
       fun cont ->
        let hash = Dom_html.window##.location##.hash |> Js.to_string in
        match extract_uuid_and_token hash with
        | Some (uuid, token) -> cont (uuid, token)
        | None ->
            alert "Unable to extract UUID and token from URL";
            Lwt.return_unit
      in
      let@ () = redirect_if_admin "shuffle" uuid token in
      set_form_target "submit_form" "submit-shuffle" uuid token;
      set_nh_ciphertexts_link uuid;
      let open Js_of_ocaml_lwt.XmlHttpRequest in
      let* election = get ("../elections/" ^ uuid ^ "/election.json") in
      let election = String.trim election.content in
      let* ciphertexts = get ("../election/nh-ciphertexts?uuid=" ^ uuid) in
      let ciphertexts = ciphertexts.content in
      set_textarea "current_ballots" ciphertexts;
      set_element_display "controls_div" "block";
      set_element_display "wait_div" "none";
      clear_content_by_id "estimation";
      match
        Dom_html.getElementById_coerce "compute_shuffle"
          Dom_html.CoerceTo.button
      with
      | None -> Lwt.return_unit
      | Some btn ->
          let* _ = Lwt_js_events.click btn in
          set_element_display "controls_div" "none";
          set_element_display "wait_div" "block";
          let* shuffle = shuffle (Js.string election) (Js.string ciphertexts) in
          let shuffle = Js.to_string shuffle in
          set_textarea "shuffle" shuffle;
          set_element_display "wait_div" "none";
          set_content "hash" (sha256_b64 shuffle);
          set_element_display "hash_div" "block";
          Lwt.return_unit)
