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
open Belenios_core.Common
open Belenios_tool_js_common
open Tool_js_html

let gettext = ref I18n.default

let lwt_handler f =
  Dom_html.handler (fun _ -> Lwt.async f; Js._true)

let show_in container f =
  let* content = f () in
  container##.innerHTML := Js.string "";
  List.iter (Dom.appendChild container) content;
  Lwt.return_unit

let textarea ?(cols = 80) ?(rows = 10) () =
  let r = textarea [] in
  r##.cols := cols;
  r##.rows := rows;
  r

let button label handler =
  let r = button [txt label] in
  r##.onclick := lwt_handler handler;
  r

let a ~href label =
  let r = a [txt label] in
  r##.href := Js.string href;
  r

let a_mailto ~recipient ~subject ~body label =
  let params = Url.encode_arguments ["subject", subject; "body", body] in
  let recipient = recipient |> Js.string |> Js.encodeURIComponent |> Js.to_string in
  let href = Printf.sprintf "mailto:%s?%s" recipient params in
  a ~href label

let api_token = ref ""
let api_root = "../api/"

let get_api_token () =
  let open Js_of_ocaml_lwt.XmlHttpRequest in
  let* x = get "../api-token" in
  if x.code = 200 then (
    api_token := x.content;
    Lwt.return_true
  ) else (
    Lwt.return_false
  )

let delete_with_token url =
  let open Js_of_ocaml_lwt.XmlHttpRequest in
  let headers = ["Authorization", "Bearer " ^ !api_token] in
  Printf.ksprintf (fun x -> perform_raw_url ~headers ~override_method:`DELETE (api_root ^ x)) url

let put_with_token x url =
  let open Js_of_ocaml_lwt.XmlHttpRequest in
  let headers = ["Authorization", "Bearer " ^ !api_token] in
  let contents = `String x in
  Printf.ksprintf (fun x -> perform_raw_url ~headers ~contents ~override_method:`PUT (api_root ^ x)) url

let post_with_token x url =
  let open Js_of_ocaml_lwt.XmlHttpRequest in
  let headers = ["Authorization", "Bearer " ^ !api_token] in
  let contents = `String x in
  Printf.ksprintf (fun x -> perform_raw_url ~headers ~contents ~override_method:`POST (api_root ^ x)) url

let bad_result = Lwt.return (Error `BadResult)

let get of_string url =
  let open Js_of_ocaml_lwt.XmlHttpRequest in
  let headers = ["Authorization", "Bearer " ^ !api_token] in
  Printf.ksprintf
    (fun x ->
      let* x = perform_raw_url ~headers (api_root ^ x) in
      match x.code with
      | 200 ->
         let@ x = Option.unwrap bad_result (Option.wrap of_string x.content) in
         Lwt.return @@ Ok x
      | code -> Lwt.return @@ Error (`BadStatus (code, x.content))
    ) url

let string_of_error = function
  | `BadResult -> "bad result"
  | `BadStatus (code, content) -> Printf.sprintf "bad status %d: %s" code content

let wrap of_string x =
  let open Js_of_ocaml_lwt.XmlHttpRequest in
  let* x = x in
  match x.code with
  | 200 ->
     let@ x = Option.unwrap bad_result (Option.wrap of_string x.content) in
     Lwt.return @@ Ok x
  | code -> Lwt.return @@ Error (`BadStatus (code, x.content))

let generic_proceed x handler =
  let msg =
    let open Js_of_ocaml_lwt.XmlHttpRequest in
    match x.code with
    | 200 -> "Success"
    | code -> Printf.sprintf "Error %d: %s" code x.content
  in
  let b = button "Proceed" handler in
  Lwt.return [node @@ div [txt msg]; node @@ div [node @@ b]]

let with_ok what x f =
  match x with
  | Error e ->
     let msg = Printf.sprintf "Error while retrieving %s: %s" what (string_of_error e) in
     Lwt.return [txt msg]
  | Ok x -> f x

let with_ok_opt what x f =
  match x with
  | Error e ->
     let msg = Printf.sprintf "Error while retrieving %s: %s" what (string_of_error e) in
     Lwt.return ([txt msg], None)
  | Ok x ->
     let* y = f x in
     Lwt.return (y, Some x)
