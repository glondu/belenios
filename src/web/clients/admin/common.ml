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

let a_data ~filename ~mime_type ~data x =
  let href = Printf.sprintf "data:%s,%s" mime_type (Url.urlencode data) in
  a ~a:[ a_download (Some filename) ] ~href x

let scrollIntoViewById id =
  let&&* d = document##getElementById (Js.string id) in
  let () =
    (Js.Unsafe.coerce d)##scrollIntoView
      (object%js
         val behavior = Js.string "smooth"
      end)
  in
  Lwt.return_unit

let api_token = ref ""
let api_root = "../api/"

let get_api_token hash =
  let open Js_of_ocaml_lwt.XmlHttpRequest in
  let* x = get "../api-token" in
  if x.code = 200 then (
    api_token := x.content;
    Lwt.return_unit)
  else
    let target =
      match hash with
      | `Election uuid ->
          Printf.sprintf "../login?cont=elections/%s@new" (Uuid.unwrap uuid)
      | _ -> "../login?cont=admin@new"
    in
    Dom_html.window##.location##assign (Js.string target);
    Lwt.return_unit

let logout () =
  let url = "../logout?cont=admin@new" in
  Dom_html.window##.location##assign (Js.string url);
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

let get of_string url =
  let open Js_of_ocaml_lwt.XmlHttpRequest in
  let headers = [ ("Authorization", "Bearer " ^ !api_token) ] in
  Printf.ksprintf
    (fun x ->
      let* x = perform_raw_url ~headers (api_root ^ x) in
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
      Lwt.return [ txt msg ]
  | Ok x -> f x

let get_ifmatch x = match x with Error _ -> None | Ok (_, x) -> Some x

(* Global information for remembering which subpage is visited. *)

(* The currently visited election context: first parameter is the uuid;
 * it can be running, still in setup, or finished. The subpage is given by a tab,
 * corresponding to the structure of the menu.
 *)
type tab =
  | Title
  | Questions
  | Voters
  | Dates
  | Language
  | Contact
  | Trustees
  | CredAuth
  | VotersPwd
  | ElectionPage
  | CreateOpenClose
  | Tally
  | Destroy

type status = Draft | Running | Tallied | Archived

type context =
  | Election of { uuid : Uuid.t; status : status; tab : tab }
  | List_draft (* list of elections in preparation *)
  | List_running (* list of elections that are running *)
  | List_old (* list of other elections *)
  | Profile (* subpage to edit user profile *)

(* The global variable *)
let where_am_i : context ref = ref List_draft

let get_current_uuid () =
  (match !where_am_i with Election { uuid; _ } -> uuid | _ -> Uuid.dummy)
  |> Uuid.unwrap

let is_draft () =
  match !where_am_i with Election { status = Draft; _ } -> true | _ -> false

let is_running () =
  match !where_am_i with Election { status = Running; _ } -> true | _ -> false

let is_archived () =
  match !where_am_i with
  | Election { status = Archived; _ } -> true
  | _ -> false

let is_tallied () =
  match !where_am_i with Election { status = Tallied; _ } -> true | _ -> false

let is_finished () = is_archived () || is_tallied ()

let popup_failsync msg =
  alert msg;
  Lwt.return_unit

let url_prefix () =
  let x = Js.to_string Dom_html.window##.location##.href in
  let re = Regexp.regexp "^(.*)/static/admin.html(#.*)?$" in
  match Regexp.string_match re x 0 with
  | None -> "https://fake_link_please_edit"
  | Some mat -> (
      match Regexp.matched_group mat 1 with
      | None -> "https://fake_link_please_edit"
      | Some pr -> pr)
