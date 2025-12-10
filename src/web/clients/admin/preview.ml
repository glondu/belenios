(**************************************************************************)
(*                                BELENIOS                                *)
(*                                                                        *)
(*  Copyright © 2022-2023 Inria, CNRS                                     *)
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
open Common

let preview_booth () =
  let open (val !Belenios_js.I18n.gettext) in
  let* res = Cache.sync () in
  match res with
  | Error msg -> popup_failsync msg
  | Ok () ->
      let href =
        Printf.sprintf "vote#uuid=%s&draft=2&lang=%s"
          (Uuid.unwrap (get_current_uuid ()))
          (Language.unwrap lang)
      in
      let onclick () =
        let&&* d = document##getElementById (Js.string "popup") in
        d##.style##.display := Js.string "none";
        Lwt.return_unit
      in
      let link = a ~a:[ a_target "_blank"; a_onclick_lwt onclick ] ~href "Ok" in
      let content =
        [
          div [ txt @@ s_ "Preview will open in a new tab" ];
          div ~a:[ a_id "prev_lk" ] [ link ];
        ]
      in
      Belenios_js.Compat.scroll 0 0;
      let&&* d = document##getElementById (Js.string "popup") in
      d##.style##.display := Js.string "block";
      let&&* container = document##getElementById (Js.string "popup-content") in
      show_in container (fun () -> Lwt.return content)

let goto_mainpage () =
  let open (val !Belenios_js.I18n.gettext) in
  let* res = Cache.sync () in
  match res with
  | Error msg -> popup_failsync msg
  | Ok () ->
      let url = "election#" ^ Uuid.unwrap (get_current_uuid ()) in
      let onclick () =
        let&&* d = document##getElementById (Js.string "popup") in
        d##.style##.display := Js.string "none";
        Lwt.return_unit
      in
      let link =
        a
          ~a:[ a_target "_belenios_mainpage"; a_onclick_lwt onclick ]
          ~href:url "Ok"
      in
      let content =
        [
          div [ txt @@ s_ "Main election page will open in a new tab." ];
          div ~a:[ a_id "prev_lk" ] [ link ];
        ]
      in
      let&&* d = document##getElementById (Js.string "popup") in
      d##.style##.display := Js.string "block";
      let&&* container = document##getElementById (Js.string "popup-content") in
      show_in container (fun () -> Lwt.return content)
