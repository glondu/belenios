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
open Belenios_web_api
open Common

let init_api_token set_api_token ?ui hash =
  match hash with
  | `Credentials (_, token) ->
      set_api_token token;
      Lwt.return_unit
  | `Error -> Lwt.return_unit
  | _ ->
      let open Js_of_ocaml_lwt.XmlHttpRequest in
      let* x = get !!"api-token" in
      if x.code = 200 then (
        set_api_token x.content;
        Lwt.return_unit)
      else
        let ui = match ui with None -> "" | Some x -> "@" ^ x in
        let target =
          match hash with
          | `Election uuid ->
              Printf.sprintf "login?cont=elections/%s%s" (Uuid.unwrap uuid) ui
          | _ -> Printf.sprintf "login?cont=home%s" ui
        in
        Dom_html.window##.location##assign (Js.string target);
        Lwt.return_unit

type xhr_result =
  | BadResult
  | BadStatus of int * string
  | RequestStatus of Belenios_web_api.request_status

let raw_delete_with_token ?ifmatch ~token url =
  let open Js_of_ocaml_lwt.XmlHttpRequest in
  let ifmatch =
    match ifmatch with Some x -> [ ("If-Match", x) ] | None -> []
  in
  let headers =
    match token with
    | None -> ifmatch
    | Some token -> ("Authorization", "Bearer " ^ token) :: ifmatch
  in
  perform_raw_url ~headers ~override_method:`DELETE !/url

let raw_put_with_token ~ifmatch ~token x url =
  let open Js_of_ocaml_lwt.XmlHttpRequest in
  let ifmatch = [ ("If-Match", ifmatch) ] in
  let headers =
    match token with
    | None -> ifmatch
    | Some token -> ("Authorization", "Bearer " ^ token) :: ifmatch
  in
  let contents = `String x in
  perform_raw_url ~headers ~contents ~override_method:`PUT !/url

let raw_put_blob_with_token ~token x url =
  let open Js_of_ocaml_lwt.XmlHttpRequest in
  let headers =
    match token with
    | None -> []
    | Some token -> [ ("Authorization", "Bearer " ^ token) ]
  in
  let contents =
    `Blob (File.blob_from_any ~contentType:"image/png" [ `arrayBuffer x ])
  in
  perform_raw_url ~headers ~contents ~override_method:`PUT !/url

let raw_post_with_token ?lang ?ifmatch ~token x url =
  let open Js_of_ocaml_lwt.XmlHttpRequest in
  let ifmatch =
    match ifmatch with Some x -> [ ("If-Match", x) ] | None -> []
  in
  let lang =
    match lang with Some x -> [ ("Accept-Language", x) ] | None -> []
  in
  let headers = ifmatch @ lang in
  let headers =
    match token with
    | None -> headers
    | Some token -> ("Authorization", "Bearer " ^ token) :: headers
  in
  let contents = `String x in
  perform_raw_url ~headers ~contents ~override_method:`POST !/url

let bad_result = Lwt.return (Error BadResult)

let raw_get_with_token ~token of_string url =
  let open Js_of_ocaml_lwt.XmlHttpRequest in
  let headers =
    match token with
    | None -> None
    | Some token -> Some [ ("Authorization", "Bearer " ^ token) ]
  in
  let* x = perform_raw_url ?headers !/url in
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
      Lwt.return @@ Error x

module Api = struct
  include Belenios_web_api.Endpoints

  let get_token = function
    | `Nobody -> None
    | `Admin token -> Some token
    | `Credauth token -> Some token
    | `Trustee token -> Some token

  let get e u = raw_get_with_token ~token:(get_token u) e.of_string e.path

  let put ~ifmatch e u x =
    raw_put_with_token ~ifmatch ~token:(get_token u) (e.to_string x) e.path

  let put_blob e u x = raw_put_blob_with_token ~token:(get_token u) x e.path

  let post ?lang ?ifmatch e u x =
    raw_post_with_token ?lang ?ifmatch ~token:(get_token u) (e.to_string_post x)
      e.path

  let delete ?ifmatch e u =
    raw_delete_with_token ?ifmatch ~token:(get_token u) e.path
end

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
