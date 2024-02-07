(**************************************************************************)
(*                                BELENIOS                                *)
(*                                                                        *)
(*  Copyright © 2012-2023 Inria, CNRS                                     *)
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
open Belenios
open Belenios_api.Serializable_j

let api_token = ref ""
let api_root = "../api/"

let init_api_token ~ui hash =
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
              Printf.sprintf "../login?cont=elections/%s@%s" (Uuid.unwrap uuid)
                ui
          | _ -> Printf.sprintf "../login?cont=admin@%s" ui
        in
        Dom_html.window##.location##assign (Js.string target);
        Lwt.return_unit

type xhr_result =
  | BadResult
  | BadStatus of int * string
  | RequestStatus of Belenios_api.Serializable_t.request_status

type ('a, 'b) xhr_helper = ('a, unit, string, 'b Lwt.t) format4 -> 'a

type 'a raw_xhr_helper =
  ('a, Js_of_ocaml_lwt.XmlHttpRequest.http_frame) xhr_helper

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

let bad_result = Lwt.return (Error BadResult)

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
          let ifmatch = sha256_b64 x.content in
          let@ x = Option.unwrap bad_result (Option.wrap of_string x.content) in
          Lwt.return @@ Ok (x, ifmatch)
      | _ ->
          let x =
            match request_status_of_string x.content with
            | exception _ -> BadStatus (x.code, x.content)
            | status -> RequestStatus status
          in
          Lwt.return @@ Error x)
    url

let string_of_error = function
  | BadResult -> "bad result"
  | BadStatus (code, content) -> Printf.sprintf "bad status %d: %s" code content
  | RequestStatus e -> string_of_request_status e

let wrap of_string x =
  let open Js_of_ocaml_lwt.XmlHttpRequest in
  let* x = x in
  match x.code with
  | 200 ->
      let@ x = Option.unwrap bad_result (Option.wrap of_string x.content) in
      Lwt.return @@ Ok x
  | code -> Lwt.return @@ Error (BadStatus (code, x.content))

let with_ok what x f =
  match x with
  | Error e ->
      let msg =
        Printf.sprintf "Error while retrieving %s: %s" what (string_of_error e)
      in
      Lwt.return [ Tyxml_js.Html.txt msg ]
  | Ok x -> f x

let with_ok_opt what x f =
  match x with
  | Error e ->
      let msg =
        Printf.sprintf "Error while retrieving %s: %s" what (string_of_error e)
      in
      Lwt.return ([ Tyxml_js.Html.txt msg ], None)
  | Ok ((a, _) as x) ->
      let* y = f x in
      Lwt.return (y, Some a)

let with_ok_not_found what x f =
  match x with
  | Error (BadStatus (404, _)) -> f None
  | Error e ->
      let msg =
        Printf.sprintf "Error while retrieving %s: %s" what (string_of_error e)
      in
      Lwt.return [ Tyxml_js.Html.txt msg ]
  | Ok x -> f (Some x)

let get_ifmatch x = match x with Error _ -> None | Ok (_, x) -> Some x
