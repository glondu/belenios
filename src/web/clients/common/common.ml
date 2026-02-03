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
open Belenios

let relative_root = ref ""
let ( !! ) x = !relative_root ^ x
let ( !/ ) x = !relative_root ^ "api/" ^ x
let ( !!! ) = Tyxml_js.To_dom.of_div
let document = Dom_html.document
let ( let&& ) = Js.Opt.bind
let ( let&&* ) x f = Js.Opt.case x (fun () -> Lwt.return_unit) f
let ( let$ ) = Js.Opt.iter
let ( let&$ ) x f = Option.iter f x
let ( let&|&& ) x f = match x with None -> Js.null | Some x -> f x
let return_unit = Js.some ()
let alert s = Dom_html.window##alert (Js.string s)
let confirm s = Dom_html.window##confirm (Js.string s) |> Js.to_bool

let prompt s =
  let x = Dom_html.window##prompt (Js.string s) (Js.string "") in
  Js.Opt.to_option (Js.Opt.map x Js.to_string)

let get_textarea_opt id =
  Option.map
    (fun x -> Js.to_string x##.value)
    (Dom_html.getElementById_coerce id Dom_html.CoerceTo.textarea)

let get_textarea id =
  match get_textarea_opt id with
  | Some x -> x
  | None -> Printf.ksprintf failwith "<textarea> %s is missing" id

let set_textarea id z =
  Option.iter
    (fun x -> x##.value := Js.string z)
    (Dom_html.getElementById_coerce id Dom_html.CoerceTo.textarea)

let get_input_opt id =
  Option.map
    (fun x -> Js.to_string x##.value)
    (Dom_html.getElementById_coerce id Dom_html.CoerceTo.input)

let get_input id =
  match get_input_opt id with
  | Some x -> x
  | None -> Printf.ksprintf failwith "<input> %s is missing" id

let get_checked_opt id =
  Option.map
    (fun x -> Js.to_bool x##.checked)
    (Dom_html.getElementById_coerce id Dom_html.CoerceTo.input)

let get_checked id =
  match get_checked_opt id with
  | Some x -> x
  | None -> Printf.ksprintf failwith "<input> %s is missing" id

let set_input id z =
  Option.iter
    (fun x -> x##.value := Js.string z)
    (Dom_html.getElementById_coerce id Dom_html.CoerceTo.input)

let get_select_opt id =
  Option.map
    (fun x -> Js.to_string x##.value)
    (Dom_html.getElementById_coerce id Dom_html.CoerceTo.select)

let get_select id =
  match get_select_opt id with
  | Some x -> x
  | None -> Printf.ksprintf failwith "<select> %s is missing" id

let set_select id z =
  Option.iter
    (fun x -> x##.value := Js.string z)
    (Dom_html.getElementById_coerce id Dom_html.CoerceTo.select)

let set_element_display id x =
  let$ e = document##getElementById (Js.string id) in
  e##.style##.display := Js.string x

let set_download id mime_type fn x =
  let x = encode_data_uri ~mime_type x in
  match Dom_html.getElementById_coerce id Dom_html.CoerceTo.a with
  | None -> ()
  | Some e ->
      e##setAttribute (Js.string "download") (Js.string fn);
      e##.href := Js.string x

let get_content x =
  Js.Opt.get
    (let&& e = document##getElementById (Js.string x) in
     let&& x = e##.textContent in
     Js.some (Js.to_string x))
    (fun () -> x)

let set_content id x =
  let$ e = document##getElementById (Js.string id) in
  let t = document##createTextNode (Js.string x) in
  Dom.appendChild e t

let append_with_br e x =
  let rec loop = function
    | [] -> ()
    | x :: xs ->
        Dom.appendChild e (Dom_html.createBr document);
        Dom.appendChild e (document##createTextNode (Js.string x));
        loop xs
  in
  match split_on_br x with
  | [] -> ()
  | x :: xs ->
      Dom.appendChild e (document##createTextNode (Js.string x));
      loop xs

let set_content_with_br id x =
  let$ e = document##getElementById (Js.string id) in
  append_with_br e x

let clear_content (e : #Dom.node Js.t) = e##.innerHTML := Js.string ""

let replace_contents container contents =
  container##.innerHTML := Js.string "";
  List.iter
    (fun x -> Dom.appendChild container (Tyxml_js.To_dom.of_node x))
    contents

let clear_content_by_id id =
  let$ e = document##getElementById (Js.string id) in
  clear_content e

let run_handler handler () =
  try handler ()
  with e ->
    let msg = "Unexpected error: " ^ Printexc.to_string e in
    alert msg

module Random : RANDOM = struct
  open Crypto_primitives

  let prng = lazy (pseudo_rng (random_string secure_rng 16))
  let get_rng () = Lazy.force prng
end

let extract_uuid_and_token x =
  let n = String.length x in
  let i = if n > 1 && x.[0] = '#' then 1 else 0 in
  let& j = String.index_opt x '-' in
  let uuid = String.sub x i (j - i) in
  let token = String.sub x (j + 1) (n - j - 1) in
  Some (uuid, token)

let set_form_target id target uuid token =
  let action =
    [ ("uuid", uuid); ("token", token) ] |> Url.encode_arguments |> fun x ->
    Printf.sprintf "%s?%s" target x
  in
  let$ form = document##getElementById (Js.string id) in
  let$ form = Dom_html.CoerceTo.form form in
  form##.action := Js.string action

let redirect_if_admin target uuid token cont =
  let open Js_of_ocaml_lwt.XmlHttpRequest in
  let* x = get !!"api-token" in
  if x.code = 200 then (
    let url = Printf.sprintf "%s/%s/%s" target uuid token in
    Dom_html.window##.location##replace (Js.string url);
    Lwt.return_unit)
  else (
    set_element_display "initially_hidden_content" "block";
    cont ())

let lwt_handler f =
  let@ _ = Dom_html.handler in
  Lwt.async f;
  Js._true

let a_lwt a_maker f =
  let@ _ = a_maker in
  Lwt.async f;
  true

let a_onclick_lwt = a_lwt Tyxml_js.Html.a_onclick

let show_in container f =
  let* content = f () in
  let content = List.map Tyxml_js.To_dom.of_node content in
  container##.innerHTML := Js.string "";
  List.iter (Dom.appendChild container) content;
  Lwt.return_unit

let input ?a ?onchange ?value () =
  let elt = Tyxml_js.Html.input ?a () in
  let r = Tyxml_js.To_dom.of_input elt in
  Option.iter (fun x -> r##.value := Js.string x) value;
  Option.iter
    (fun h ->
      r##.onchange :=
        let@ _ = Dom_html.handler in
        h r;
        Js._true)
    onchange;
  (elt, fun () -> Js.to_string r##.value)

let select ?a ?onchange ?value options =
  let elt = Tyxml_js.Html.select ?a options in
  let r = Tyxml_js.To_dom.of_select elt in
  Option.iter (fun x -> r##.value := Js.string x) value;
  Option.iter
    (fun h ->
      r##.onchange :=
        let@ _ = Dom_html.handler in
        h r;
        Js._true)
    onchange;
  elt

let button ?a label handler =
  let a = a_onclick_lwt handler :: Option.value ~default:[] a in
  Tyxml_js.Html.button ~a [ Tyxml_js.Html.txt label ]

let button_self ?(a = []) label handler =
  let onclick x =
    let@ () = finally true in
    let@ e = Js.Opt.iter x##.target in
    let@ e = Js.Opt.iter (Dom_html.CoerceTo.button e) in
    let@ () = Lwt.async in
    handler e
  in
  let a = Tyxml_js.Html.a_onclick onclick :: a in
  Tyxml_js.Html.button ~a [ Tyxml_js.Html.txt label ]

let textarea ?a ?(cols = 80) ?(rows = 10) ?placeholder ?onchange value =
  let elt = Tyxml_js.Html.textarea ?a (Tyxml_js.Html.txt value) in
  let r = Tyxml_js.To_dom.of_textarea elt in
  r##.cols := cols;
  r##.rows := rows;
  Option.iter (fun x -> r##.placeholder := Js.string x) placeholder;
  Option.iter
    (fun h ->
      r##.onchange :=
        let@ _ = Dom_html.handler in
        h r;
        Js._true)
    onchange;
  (elt, fun () -> Js.to_string r##.value)

let a ?a ~href label =
  let elt = Tyxml_js.Html.a ?a [ Tyxml_js.Html.txt label ] in
  let r = Tyxml_js.To_dom.of_a elt in
  r##.href := Js.string href;
  elt

let a_mailto ?a:attributes ~recipient ~subject ~body label =
  let params = Url.encode_arguments [ ("subject", subject); ("body", body) ] in
  let recipient =
    recipient |> Js.string |> Js.encodeURIComponent |> Js.to_string
  in
  let href = Printf.sprintf "mailto:%s?%s" recipient params in
  a ?a:attributes ~href label

let a_data ?a:attributes ~filename ~mime_type ~data x =
  let href = encode_data_uri ~mime_type data in
  let attributes =
    Tyxml_js.Html.a_download (Some filename)
    :: Option.value ~default:[] attributes
  in
  a ~a:attributes ~href x

let scrollIntoViewById id =
  let&&* d = document##getElementById (Js.string id) in
  let () =
    (Js.Unsafe.coerce d)##scrollIntoView
      (object%js
         val behavior = Js.string "smooth"
      end)
  in
  Lwt.return_unit

open Js

class type window = object
  inherit Dom_html.window
  method opener : window t opt readonly_prop
  method postMessage : 'a -> js_string t -> unit meth
end

class type messageEvent = object
  inherit Dom_html.event
  method origin : js_string t readonly_prop
  method data : Js.Unsafe.any readonly_prop
  method source : window t readonly_prop
end

module Event = struct
  open Dom_html.Event

  let message : messageEvent Js.t typ = make "message"
end

let coerce_window : Dom_html.window t -> window t = Js.Unsafe.coerce

let make_login_target ~state =
  let params = Url.encode_arguments [ ("state", state) ] in
  !!(Printf.sprintf "actions/voter-login?%s" params)

let compute_prefix () =
  let open Uri in
  let base = of_string (Js.to_string Dom_html.window##.location##.href) in
  let base = with_fragment base None in
  let path = path base in
  let path =
    match String.rindex_opt path '/' with
    | None -> path
    | Some i -> String.sub path 0 i
  in
  path ^ "/" ^ !relative_root |> with_path base |> canonicalize |> to_string

let set_logo uuid =
  let$ e = document##getElementById (Js.string "election_logo") in
  clear_content e;
  let src = !/((Belenios_web_api.Endpoints.election_logo uuid).path) in
  let handler _ =
    clear_content e;
    true
  in
  let img =
    let open Tyxml_js.Html in
    img
      ~a:[ a_onerror handler; a_class [ "page-header__logo__image" ] ]
      ~src ~alt:"election logo" ()
  in
  Dom.appendChild e (Tyxml_js.To_dom.of_img img)

open Belenios_web_api

let dummy_client_configuration = { consent = None; lang = None }
let label_client_configuration = Js.string "belenios.client.configuration"

let get_client_configuration () =
  Js.Optdef.case
    Dom_html.window##.localStorage
    (fun () -> dummy_client_configuration)
    (fun s ->
      Js.Opt.case
        (s##getItem label_client_configuration)
        (fun () -> dummy_client_configuration)
        (fun c ->
          match client_configuration_of_string (Js.to_string c) with
          | exception _ -> dummy_client_configuration
          | x -> x))

let set_client_configuration x =
  Js.Optdef.iter Dom_html.window##.localStorage (fun s ->
      if x = dummy_client_configuration then
        s##removeItem label_client_configuration
      else
        s##setItem label_client_configuration
          (Js.string (string_of_client_configuration x)))

let get_language () =
  let c = get_client_configuration () in
  match c.lang with
  | None -> Language.unwrap Compat.navigator_language
  | Some x -> x
