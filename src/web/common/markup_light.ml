(**************************************************************************)
(*                                BELENIOS                                *)
(*                                                                        *)
(*  Copyright © 2023-2023 Inria                                           *)
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

type 'a rendering_functions = {
  text : int -> string -> 'a;
  bold : int -> 'a list -> 'a;
  italic : int -> 'a list -> 'a;
}

let rec render p xs = List.mapi (render_item p) xs

and render_item p i = function
  | Markup_types.Text s -> p.text i s
  | Bold xs -> p.bold i (render p xs)
  | Italic xs -> p.italic i (render p xs)

exception Unsupported of string

let parse_html x =
  let open Markup in
  parse_html
    ~report:(fun location e ->
      raise @@ Unsupported (Markup.Error.to_string ~location e))
    ~context:(`Fragment "span") (string x)
  |> signals
  |> trees
       ~text:(fun x -> Markup_types.Text (String.concat "" x))
       ~element:(fun (_, name) _attrs children ->
         match name with
         | "br" -> Text " | "
         | "b" -> Bold children
         | "i" -> Italic children
         | name ->
             raise
             @@ Unsupported (Printf.sprintf "unsupported element: %s" name))
  |> to_list

module type BASE = sig
  module Xml : Xml_sigs.NoWrap
  module Svg : Svg_sigs.Make(Xml).T
  module Html : Html_sigs.Make(Xml)(Svg).T
end

module Make (Base : BASE) = struct
  open Base.Html

  let markup x =
    let p : _ rendering_functions =
      {
        bold = (fun _ xs -> span ~a:[ a_class [ "markup-b" ] ] xs);
        text = (fun _ x -> txt x);
        italic = (fun _ xs -> span ~a:[ a_class [ "markup-i" ] ] xs);
      }
    in
    try
      let xs = parse_html x in
      let xs = render p xs in
      span xs
    with _ -> span ~a:[ a_class [ "markup-error" ] ] [ txt x ]
end
