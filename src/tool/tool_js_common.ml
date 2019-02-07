(**************************************************************************)
(*                                BELENIOS                                *)
(*                                                                        *)
(*  Copyright © 2012-2018 Inria                                           *)
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
open Common

let document = Dom_html.document

let alert s =
  Dom_html.window##alert (Js.string s)

let prompt s =
  let x = Dom_html.window##prompt (Js.string s) (Js.string "") in
  Js.Opt.to_option (Js.Opt.map x Js.to_string)

let with_element x f =
  Option.iter f (Dom_html.getElementById_opt x)

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

let set_element_display id x =
  with_element id (fun e -> e##.style##.display := Js.string x)

let set_download id mime fn x =
  let x = (Js.string ("data:" ^ mime ^ ","))##concat (Js.encodeURI (Js.string x)) in
  match Dom_html.getElementById_coerce id Dom_html.CoerceTo.a with
  | None -> ()
  | Some e ->
     e##setAttribute (Js.string "download") (Js.string fn);
     e##.href := x

let get_content x =
  let r = ref x in
  with_element x (fun x ->
    Js.Opt.iter (x##.textContent) (fun x -> r := Js.to_string x)
  ); !r

let set_content id x =
  with_element id (fun e ->
    let t = document##createTextNode (Js.string x) in
    Dom.appendChild e t
  )

let run_handler handler () =
  (try handler ()
   with e ->
     let msg = "Unexpected error: " ^ Printexc.to_string e in
     alert msg
  ); Js._false
