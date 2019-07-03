(**************************************************************************)
(*                                BELENIOS                                *)
(*                                                                        *)
(*  Copyright © 2012-2019 Inria                                           *)
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

open Js_of_ocaml
open Serializable_j
open Common
open Tool_js_common

let shuffle election ciphertexts =
  let election = Election.(get_group (of_string election)) in
  let module W = (val election) in
  let module E = Election.Make (W) (LwtJsRandom) in
  let ciphertexts = nh_ciphertexts_of_string E.G.read ciphertexts in
  let%lwt shuffle = E.shuffle_ciphertexts ciphertexts () in
  Lwt.return (string_of_shuffle E.G.write shuffle)

let () =
  Lwt.async (fun () ->
      let%lwt _ = Lwt_js_events.onload () in
      match Dom_html.getElementById_coerce "compute_shuffle" Dom_html.CoerceTo.button with
      | None -> Lwt.return_unit
      | Some btn ->
         let%lwt _ = Lwt_js_events.click btn in
         set_element_display "controls_div" "none";
         set_element_display "wait_div" "block";
         let uuid = List.assoc "uuid" (get_params ()) in
         let open Lwt_xmlHttpRequest in
         let%lwt election = get ("../elections/" ^ uuid ^ "/election.json") in
         let%lwt ciphertexts = get ("../election/nh-ciphertexts?uuid=" ^ uuid) in
         let%lwt shuffle = shuffle election.content ciphertexts.content in
         set_textarea "shuffle" shuffle;
         set_element_display "wait_div" "none";
         Lwt.return_unit
    )
