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
open Belenios_platform
open Belenios_core
open Platform
open Common

let document = Dom_html.document

let ( let&& ) = Js.Opt.bind
let ( let&&* ) x f = Js.Opt.case x (fun () -> Lwt.return_unit) f
let ( let$ ) = Js.Opt.iter
let ( let&$ ) x f = Option.iter f x

let ( let&|&& ) x f =
  match x with
  | None -> Js.null
  | Some x -> f x

let return_unit =
  Js.some ()

let alert s =
  Dom_html.window##alert (Js.string s)

let confirm s =
  Dom_html.window##confirm (Js.string s)
  |> Js.to_bool

let prompt s =
  let x = Dom_html.window##prompt (Js.string s) (Js.string "") in
  Js.Opt.to_option (Js.Opt.map x Js.to_string)

let get_textarea_opt id =
  Option.map (fun x -> Js.to_string x##.value)
    (Dom_html.getElementById_coerce id Dom_html.CoerceTo.textarea)

let get_textarea id =
  match get_textarea_opt id with
  | Some x -> x
  | None -> Printf.ksprintf failwith "<textarea> %s is missing" id

let set_textarea id z =
  Option.iter (fun x -> x##.value := Js.string z)
    (Dom_html.getElementById_coerce id Dom_html.CoerceTo.textarea)

let get_input_opt id =
  Option.map (fun x -> Js.to_string x##.value)
    (Dom_html.getElementById_coerce id Dom_html.CoerceTo.input)

let get_input id =
  match get_input_opt id with
  | Some x -> x
  | None -> Printf.ksprintf failwith "<input> %s is missing" id

let set_input id z =
  Option.iter (fun x -> x##.value := Js.string z)
    (Dom_html.getElementById_coerce id Dom_html.CoerceTo.input)

let get_select_opt id =
  Option.map (fun x -> Js.to_string x##.value)
    (Dom_html.getElementById_coerce id Dom_html.CoerceTo.select)

let get_select id =
  match get_select_opt id with
  | Some x -> x
  | None -> Printf.ksprintf failwith "<select> %s is missing" id

let set_select id z =
  Option.iter (fun x -> x##.value := Js.string z)
    (Dom_html.getElementById_coerce id Dom_html.CoerceTo.select)

let set_element_display id x =
  let$ e = document##getElementById (Js.string id) in
  e##.style##.display := Js.string x

let set_download id mime fn x =
  let x = (Js.string ("data:" ^ mime ^ ","))##concat (Js.encodeURI (Js.string x)) in
  match Dom_html.getElementById_coerce id Dom_html.CoerceTo.a with
  | None -> ()
  | Some e ->
     e##setAttribute (Js.string "download") (Js.string fn);
     e##.href := x

let get_content x =
  Js.Opt.get (
      let&& e = document##getElementById (Js.string x) in
      let&& x = e##.textContent in
      Js.some (Js.to_string x)
    ) (fun () -> x)

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

let clear_content (e : #Dom.node Js.t) =
  while Js.to_bool e##hasChildNodes do
    let$ x = e##.firstChild in
    let _ = e##removeChild x in
    ()
  done

let clear_content_by_id id =
  let$ e = document##getElementById (Js.string id) in
  clear_content e

let run_handler handler () =
  try handler () with
  | e ->
     let msg = "Unexpected error: " ^ Printexc.to_string e in
     alert msg

module Random : Signatures.RANDOM = struct
  let prng = lazy (pseudo_rng (random_string secure_rng 16))

  let random q =
    let size = bytes_to_sample q in
    let r = random_string (Lazy.force prng) size in
    Z.(of_bits r mod q)
end

let get ?token of_string url =
  let open Js_of_ocaml_lwt.XmlHttpRequest in
  let headers =
    let& token in
    Some ["Authorization", "Bearer " ^ token]
  in
  let* x = perform_raw_url ?headers url in
  match x.code with
  | 200 -> Lwt.return @@ Option.wrap of_string x.content
  | _ -> Lwt.return_none

let extract_uuid_and_token x =
  let n = String.length x in
  let i = if n > 1 && x.[0] = '#' then 1 else 0 in
  let& j = String.index_opt x '-' in
  let uuid = String.sub x i (j - i) in
  let token = String.sub x (j + 1) (n - j - 1) in
  Some (uuid, token)

let build_election_url href uuid =
  let base =
    match String.split_on_char '/' href |> List.rev with
    | _ :: _ :: base -> String.concat "/" (List.rev base)
    | _ -> href
  in
  Printf.sprintf "%s/elections/%s/" base uuid

let set_form_target id target uuid token =
  let action =
    ["uuid", uuid; "token", token]
    |> Url.encode_arguments
    |> (fun x -> Printf.sprintf "%s?%s" target x)
  in
  let$ form = document##getElementById (Js.string id) in
  let$ form = Dom_html.CoerceTo.form form in
  form##.action := Js.string action

let redirect_if_admin target uuid token cont =
  let open Js_of_ocaml_lwt.XmlHttpRequest in
  let* x = get "../api-token" in
  if x.code = 200 then (
    let url = Printf.sprintf "%s/%s/%s" target uuid token in
    Dom_html.window##.location##replace (Js.string url);
    Lwt.return_unit
  ) else (
    set_element_display "initially_hidden_content" "block";
    cont ()
  )

let lwt_handler f =
  Dom_html.handler (fun _ -> Lwt.async f; Js._true)

let show_in container f =
  let* content = f () in
  let content = List.map Tyxml_js.To_dom.of_node content in
  container##.innerHTML := Js.string "";
  List.iter (Dom.appendChild container) content;
  Lwt.return_unit

let input ?a value =
  let elt = Tyxml_js.Html.input ?a () in
  let r = Tyxml_js.To_dom.of_input elt in
  r##.value := Js.string value;
  elt, (fun () -> Js.to_string r##.value)

let button ?a label handler =
  let elt = Tyxml_js.Html.button ?a [Tyxml_js.Html.txt label] in
  let r = Tyxml_js.To_dom.of_button elt in
  r##.onclick := lwt_handler handler;
  elt
