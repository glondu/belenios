(**************************************************************************)
(*                                BELENIOS                                *)
(*                                                                        *)
(*  Copyright © 2012-2023 Inria                                           *)
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
open Belenios_core.Common
open Belenios_api.Serializable_j
open Tyxml_js.Html5
open Belenios_js.Common

let textarea ?(cols = 80) ?(rows = 10) ?placeholder value =
  let elt = textarea (txt value) in
  let r = Tyxml_js.To_dom.of_textarea elt in
  r##.cols := cols;
  r##.rows := rows;
  let () =
    match placeholder with
    | None -> ()
    | Some x -> r##.placeholder := Js.string x
  in
  (elt, fun () -> Js.to_string r##.value)

let a ?a ~href label =
  let elt = Tyxml_js.Html.a ?a [ txt label ] in
  let r = Tyxml_js.To_dom.of_a elt in
  r##.href := Js.string href;
  elt

let a_mailto ~recipient ~subject ~body label =
  let params = Url.encode_arguments [ ("subject", subject); ("body", body) ] in
  let recipient =
    recipient |> Js.string |> Js.encodeURIComponent |> Js.to_string
  in
  let href = Printf.sprintf "mailto:%s?%s" recipient params in
  a ~href label

let api_token = ref ""
let api_root = "../api/"

let get_api_token hash =
  match hash with
  | `Credentials (_, token) ->
      api_token := token;
      Lwt.return_unit
  | `Error -> Lwt.return_unit
  | _ ->
      let open Js_of_ocaml_lwt.XmlHttpRequest in
      let* x = get "../api-token" in
      if x.code = 200 then (
        api_token := x.content;
        Lwt.return_unit)
      else
        let target =
          match hash with
          | `Election uuid ->
              Printf.sprintf "../login?cont=elections/%s@basic" uuid
          | _ -> "../login?cont=admin@basic"
        in
        Dom_html.window##.location##assign (Js.string target);
        Lwt.return_unit

let delete_with_token ?ifmatch url =
  let open Js_of_ocaml_lwt.XmlHttpRequest in
  let ifmatch =
    match ifmatch with Some x -> [ ("If-Match", x) ] | None -> []
  in
  let headers = ("Authorization", "Bearer " ^ !api_token) :: ifmatch in
  Printf.ksprintf
    (fun x -> perform_raw_url ~headers ~override_method:`DELETE (api_root ^ x))
    url

let put_with_token ~ifmatch x url =
  let open Js_of_ocaml_lwt.XmlHttpRequest in
  let headers =
    [ ("Authorization", "Bearer " ^ !api_token); ("If-Match", ifmatch) ]
  in
  let contents = `String x in
  Printf.ksprintf
    (fun x ->
      perform_raw_url ~headers ~contents ~override_method:`PUT (api_root ^ x))
    url

let post_with_token ?ifmatch x url =
  let open Js_of_ocaml_lwt.XmlHttpRequest in
  let ifmatch =
    match ifmatch with Some x -> [ ("If-Match", x) ] | None -> []
  in
  let headers = ("Authorization", "Bearer " ^ !api_token) :: ifmatch in
  let contents = `String x in
  Printf.ksprintf
    (fun x ->
      perform_raw_url ~headers ~contents ~override_method:`POST (api_root ^ x))
    url

let bad_result = Lwt.return (Error `BadResult)

let get ?(notoken = false) of_string url =
  let open Js_of_ocaml_lwt.XmlHttpRequest in
  let headers =
    if notoken then None else Some [ ("Authorization", "Bearer " ^ !api_token) ]
  in
  Printf.ksprintf
    (fun x ->
      let* x = perform_raw_url ?headers (api_root ^ x) in
      match x.code with
      | 200 ->
          let@ x = Option.unwrap bad_result (Option.wrap of_string x.content) in
          Lwt.return @@ Ok x
      | _ ->
          let x =
            match request_status_of_string x.content with
            | exception _ -> `BadStatus (x.code, x.content)
            | status -> `Error status
          in
          Lwt.return @@ Error x)
    url

let string_of_error = function
  | `BadResult -> "bad result"
  | `BadStatus (code, content) ->
      Printf.sprintf "bad status %d: %s" code content
  | `Error e -> string_of_request_status e

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
  Lwt.return [ div [ txt msg ]; div [ b ] ]

let with_ok what x f =
  match x with
  | Error e ->
      let msg =
        Printf.sprintf "Error while retrieving %s: %s" what (string_of_error e)
      in
      Lwt.return [ txt msg ]
  | Ok x -> f x

let with_ok_opt what x f =
  match x with
  | Error e ->
      let msg =
        Printf.sprintf "Error while retrieving %s: %s" what (string_of_error e)
      in
      Lwt.return ([ txt msg ], None)
  | Ok x ->
      let* y = f x in
      Lwt.return (y, Some x)

let with_ok_not_found what x f =
  match x with
  | Error (`BadStatus (404, _)) -> f None
  | Error e ->
      let msg =
        Printf.sprintf "Error while retrieving %s: %s" what (string_of_error e)
      in
      Lwt.return [ txt msg ]
  | Ok x -> f (Some x)

let compute_ifmatch to_string x =
  match x with Error _ -> None | Ok x -> Some (x |> to_string |> sha256_b64)
