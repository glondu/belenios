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
open Js_of_ocaml_lwt
open Belenios_core
open Belenios
open Serializable_j
open Common
open Belenios_js.Common

let eta = ref 0

let shuffle election ciphertexts =
  let open (val !Belenios_js.I18n.gettext) in
  let module W = Election.Make (struct let raw_election = election end) (LwtJsRandom) () in
  let ciphertexts = nh_ciphertexts_of_string W.(sread G.of_string) ciphertexts in
  let full_shuffle () =
    let id =
      if !eta > 0 then
        let start = (new%js Js.date_now)##valueOf in
        let stop = start +. float_of_int !eta *. 1000. in
        let update () =
          let now = (new%js Js.date_now)##valueOf in
          let eta = max 0 (int_of_float (ceil ((stop -. now) /. 1000.))) in
          clear_content_by_id "estimation";
          set_content "estimation"
            (Printf.sprintf (f_ "Estimated remaining time: %d second(s)") eta)
        in
        Some (Dom_html.window##setInterval (Js.wrap_callback update) 500.)
      else
        None
    in
    let* shuffle = W.E.shuffle_ciphertexts ciphertexts in
    let r = string_of_shuffle W.(swrite G.to_string) shuffle in
    let () =
      match id with
      | Some x ->
         Dom_html.window##clearInterval x;
         clear_content_by_id "estimation";
      | None -> ()
    in
    Lwt.return r
  in
  let bench_shuffle () =
    let n =
      Array.fold_left (fun accu x -> accu + Array.length x) 0 ciphertexts
    in
    let* x = LwtJsRandom.random W.G.q in
    let start = new%js Js.date_now in
    let _ = W.G.(g **~ x) in
    let stop = new%js Js.date_now in
    set_element_display "controls_div" "block";
    set_element_display "wait_div" "none";
    let delta = (stop##valueOf -. start##valueOf) /. 1000. in
    (* cost is 11n+7 modpows, we add another n for the overhead *)
    eta := int_of_float (ceil (delta *. float_of_int (12 * n + 7)));
    clear_content_by_id "estimation";
    set_content "estimation"
      (Printf.sprintf (f_ "Estimated computation time: %d second(s)") !eta);
    Lwt.return_unit
  in
  Lwt.async bench_shuffle;
  full_shuffle

let set_nh_ciphertexts_link uuid =
  let href =
    ["uuid", uuid]
    |> Url.encode_arguments
    |> (fun x -> Printf.sprintf "nh-ciphertexts?%s" x)
  in
  let$ a = document##getElementById (Js.string "nh_ciphertexts_link") in
  let$ a = Dom_html.CoerceTo.a a in
  a##.href := Js.string href

let () =
  Lwt.async (fun () ->
      let* _ = Lwt_js_events.onload () in
      let* () = Belenios_js.I18n.auto_init "admin" in
      let@ uuid, token = fun cont ->
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
      let* ciphertexts = get ("../election/nh-ciphertexts?uuid=" ^ uuid) in
      set_textarea "current_ballots" ciphertexts.content;
      let full_shuffle = shuffle (String.trim election.content) ciphertexts.content in
      match Dom_html.getElementById_coerce "compute_shuffle" Dom_html.CoerceTo.button with
      | None -> Lwt.return_unit
      | Some btn ->
         let* _ = Lwt_js_events.click btn in
         set_element_display "controls_div" "none";
         set_element_display "wait_div" "block";
         let* shuffle = full_shuffle () in
         set_textarea "shuffle" shuffle;
         set_element_display "wait_div" "none";
         set_content "hash" (sha256_b64 shuffle);
         set_element_display "hash_div" "block";
         Lwt.return_unit
    )
