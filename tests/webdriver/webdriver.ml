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
open Common

type session = { implicit_timeout : int; prefix : string; session : string }
type element = string
type window = string

let json_of_element x =
  `Assoc [ ("element-6066-11e4-a52e-4f735466cecf", `String x) ]

let encode_for_send_keys x =
  let b = Buffer.create (String.length x) in
  String.iter
    (function
      | '0' .. '9' as c ->
          let c = int_of_char c - int_of_char '0' in
          Buffer.add_string b "\xee\x80";
          Buffer.add_char b (char_of_int (0x9a + c))
      | c -> Buffer.add_char b c)
    x;
  Buffer.contents b

let get name = function `Assoc o -> List.assoc_opt name o | _ -> None
let get_value = get "value"

let get_sessionId x =
  let& x = get_value x in
  let& x = get "sessionId" x in
  match x with `String x -> Some x | _ -> None

exception Error of string * int * string

let () =
  Printexc.register_printer (function
    | Error (what, code, x) ->
        Some (Printf.sprintf "Webdriver.Error(%s, %d, %s)" what code x)
    | _ -> None)

let handle_reply what ((response : Cohttp.Response.t), x) =
  let* x = Cohttp_lwt.Body.to_string x in
  match Cohttp.Code.code_of_status response.status with
  | 200 -> (
      match Yojson.Safe.from_string x with
      | x -> Lwt.return x
      | exception _ ->
          Lwt.fail @@ Failure (Printf.sprintf "unexpected body in %s" what))
  | code -> Lwt.fail @@ Error (what, code, x)

let chunked = false (* mandatory for chromedriver *)
let headers = [ ("Content-Type", "application/json") ] |> Cohttp.Header.of_list
let body = "{}" |> Cohttp_lwt.Body.of_string

let start_session ~implicit_timeout ~headless ~url =
  let moz_options =
    let args = if headless then [ `String "-headless" ] else [] in
    `Assoc [ ("args", `List args) ]
  in
  let goog_options =
    let args = if headless then [ `String "headless" ] else [] in
    `Assoc [ ("args", `List args) ]
  in
  let body =
    `Assoc
      [
        ( "capabilities",
          `Assoc
            [
              ( "alwaysMatch",
                `Assoc
                  [
                    ("timeouts", `Assoc [ ("implicit", `Int implicit_timeout) ]);
                    ("moz:firefoxOptions", moz_options);
                    ("goog:chromeOptions", goog_options);
                  ] );
            ] );
      ]
    |> Yojson.Safe.to_string |> Cohttp_lwt.Body.of_string
  in
  let url = Printf.sprintf "%s/session" url |> Uri.of_string in
  let* x = Cohttp_lwt_unix.Client.post ~chunked ~headers ~body url in
  let* x = handle_reply "start_session" x in
  match get_sessionId x with
  | None -> Lwt.fail @@ Failure "unable to get sessionId"
  | Some x -> Lwt.return x

let stop_session ~url session =
  let url = Printf.sprintf "%s/session/%s" url session |> Uri.of_string in
  let* x = Cohttp_lwt_unix.Client.delete url in
  let* _ = handle_reply "stop_session" x in
  Lwt.return_unit

class webdriver { implicit_timeout; prefix; session } =
  let implicit_timeout_s = float_of_int implicit_timeout /. 1000. in
  let element_action action elementId =
    let url =
      Printf.sprintf "%s/session/%s/element/%s/%s" prefix session elementId
        action
      |> Uri.of_string
    in
    let* x = Cohttp_lwt_unix.Client.post ~chunked ~headers ~body url in
    let* _ = handle_reply action x in
    Lwt.return_unit
  in
  let alert_action action =
    let url =
      Printf.sprintf "%s/session/%s/alert/%s" prefix session action
      |> Uri.of_string
    in
    let* x = Cohttp_lwt_unix.Client.post ~chunked ~headers ~body url in
    let* _ = handle_reply action x in
    Lwt_unix.sleep implicit_timeout_s
  in
  let get_elements_generic ~url ~selector =
    let body =
      `Assoc [ ("using", `String "css selector"); ("value", `String selector) ]
      |> Yojson.Safe.to_string |> Cohttp_lwt.Body.of_string
    in
    let* x = Cohttp_lwt_unix.Client.post ~chunked ~headers ~body url in
    let* x = handle_reply "get_elements_generic" x in
    Lwt.return
    @@
    match get_value x with
    | Some (`List l) ->
        List.fold_left
          (fun accu x ->
            match x with
            | `Assoc [ ("element-6066-11e4-a52e-4f735466cecf", `String x) ] ->
                x :: accu
            | _ -> accu)
          [] l
        |> List.rev
    | _ -> []
  in
  object (self)
    method implicit_wait = Lwt_unix.sleep implicit_timeout_s

    method navigate_to url =
      let body =
        `Assoc [ ("url", `String url) ]
        |> Yojson.Safe.to_string |> Cohttp_lwt.Body.of_string
      in
      let url =
        Printf.sprintf "%s/session/%s/url" prefix session |> Uri.of_string
      in
      let* x = Cohttp_lwt_unix.Client.post ~chunked ~headers ~body url in
      let* _ = handle_reply "navigate_to" x in
      self#implicit_wait

    method get_url =
      let url =
        Printf.sprintf "%s/session/%s/url" prefix session |> Uri.of_string
      in
      let* x = Cohttp_lwt_unix.Client.get url in
      let* x = handle_reply "get_url" x in
      match x with
      | `Assoc [ ("value", `String url) ] -> Lwt.return url
      | _ -> Lwt.fail @@ Failure "could not get url"

    method get_elements ~selector =
      let url =
        Printf.sprintf "%s/session/%s/elements" prefix session |> Uri.of_string
      in
      get_elements_generic ~url ~selector

    method get_sub_elements element ~selector =
      let url =
        Printf.sprintf "%s/session/%s/element/%s/elements" prefix session
          element
        |> Uri.of_string
      in
      get_elements_generic ~url ~selector

    method accept = alert_action "accept"
    method click = element_action "click"
    method clear = element_action "clear"

    method send_keys elementId text =
      let body =
        `Assoc [ ("text", `String (encode_for_send_keys text)) ]
        |> Yojson.Safe.to_string |> Cohttp_lwt.Body.of_string
      in
      let url =
        Printf.sprintf "%s/session/%s/element/%s/value" prefix session elementId
        |> Uri.of_string
      in
      let* x = Cohttp_lwt_unix.Client.post ~chunked ~headers ~body url in
      let* _ = handle_reply "send_keys" x in
      Lwt.return_unit

    method get_windows =
      let url =
        Printf.sprintf "%s/session/%s/window/handles" prefix session
        |> Uri.of_string
      in
      let* x = Cohttp_lwt_unix.Client.get url in
      let* x = handle_reply "get_windows" x in
      Lwt.return
      @@
      match get_value x with
      | Some (`List x) ->
          List.filter_map (function `String x -> Some x | _ -> None) x
      | _ -> []

    method switch_to_window window =
      let body =
        `Assoc [ ("handle", `String window) ]
        |> Yojson.Safe.to_string |> Cohttp_lwt.Body.of_string
      in
      let url =
        Printf.sprintf "%s/session/%s/window" prefix session |> Uri.of_string
      in
      let* x = Cohttp_lwt_unix.Client.post ~chunked ~headers ~body url in
      let* _ = handle_reply "switch_to_window" x in
      Lwt.return_unit

    method maximize_window =
      let url =
        Printf.sprintf "%s/session/%s/window/maximize" prefix session
        |> Uri.of_string
      in
      let* x = Cohttp_lwt_unix.Client.post ~chunked ~headers ~body url in
      let* _ = handle_reply "maximize_window" x in
      Lwt.return_unit

    method set_window_rect ?width ?height ?x ?y () =
      let parameters =
        let f what x =
          match x with None -> [] | Some x -> [ (what, `Int x) ]
        in
        [ f "width" width; f "height" height; f "x" x; f "y" y ] |> List.flatten
      in
      let body =
        `Assoc parameters |> Yojson.Safe.to_string |> Cohttp_lwt.Body.of_string
      in
      let url =
        Printf.sprintf "%s/session/%s/window/rect" prefix session
        |> Uri.of_string
      in
      let* x = Cohttp_lwt_unix.Client.post ~chunked ~headers ~body url in
      let* _ = handle_reply "set_window_rect" x in
      Lwt.return_unit

    method close_window =
      let url =
        Printf.sprintf "%s/session/%s/window" prefix session |> Uri.of_string
      in
      let* x = Cohttp_lwt_unix.Client.delete url in
      let* _ = handle_reply "close_window" x in
      Lwt.return_unit

    method execute ~script ~args =
      let body =
        `Assoc [ ("script", `String script); ("args", `List args) ]
        |> Yojson.Safe.to_string |> Cohttp_lwt.Body.of_string
      in
      let url =
        Printf.sprintf "%s/session/%s/execute/sync" prefix session
        |> Uri.of_string
      in
      let* x = Cohttp_lwt_unix.Client.post ~chunked ~headers ~body url in
      let* x = handle_reply "execute" x in
      Lwt.return @@ get_value x
  end

class helpers session =
  object (self)
    inherit webdriver session

    method with_single_element :
        'a. selector:string -> unit -> (element -> 'a Lwt.t) -> 'a Lwt.t =
      fun ~selector () f ->
        let* x = self#get_elements ~selector in
        match x with
        | [ x ] -> f x
        | _ ->
            Printf.ksprintf failwith "not exactly 1 element returned by %s"
              selector

    method fill_with ~selector text =
      let@ element = self#with_single_element ~selector () in
      let* () = self#click element in
      let* () = self#clear element in
      let* () = self#send_keys element text in
      self#implicit_wait

    method click_on ~selector =
      let@ element = self#with_single_element ~selector () in
      let* () = self#click element in
      self#implicit_wait

    method click_on_last_button =
      let* x = self#get_elements ~selector:"button" in
      match List.rev x with
      | x :: _ ->
          let* () = self#click x in
          self#implicit_wait
      | [] -> Lwt.fail @@ Failure "no buttons"
  end

let with_session ?(implicit_timeout = 500) ~headless ~url () f =
  let* session = start_session ~implicit_timeout ~headless ~url in
  let x = { implicit_timeout; prefix = url; session } in
  Lwt.finalize (fun () -> f x) (fun () -> stop_session ~url session)
