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

open Js_of_ocaml
open Tool_js_common

let node x = (x :> Dom.node Js.t)

let txt s =
  node @@ document##createTextNode (Js.string s)

let createNode create ?className (children : Dom.node Js.t list) =
  let container = create document in
  Option.iter (fun s -> container##.className := Js.string s) className;
  List.iter (Dom.appendChild container) children;
  container

let div = createNode Dom_html.createDiv
let span = createNode Dom_html.createSpan
let h1 = createNode Dom_html.createH1
let h2 = createNode Dom_html.createH2
let ul = createNode Dom_html.createUl
let li = createNode Dom_html.createLi
let a = createNode Dom_html.createA
let textarea = createNode Dom_html.createTextarea
let button = createNode Dom_html.createButton
