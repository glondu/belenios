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
  link : int -> target:string -> label:string -> 'a;
}

let rec render p xs = List.mapi (render_item p) xs

and render_item p i = function
  | Markup_types.Text s -> p.text i s
  | Bold xs -> p.bold i (render p xs)
  | Italic xs -> p.italic i (render p xs)
  | Link { target; label } -> p.link i ~target ~label

exception Unsupported of string

let rec attr_assoc attr = function
  | [] -> None
  | ((_, name), value) :: _ when name = attr -> Some value
  | _ :: xs -> attr_assoc attr xs

let rec markup_of_xml = function
  | Xml.PCData x -> Markup_types.Text x
  | Element ("br", [], []) -> Text " | "
  | Element ("b", [], children) -> Bold (List.map markup_of_xml children)
  | Element ("i", [], children) -> Italic (List.map markup_of_xml children)
  | Element ("a", attrs, children) ->
      let target =
        match List.assoc_opt "href" attrs with
        | Some x -> x
        | None -> raise @@ Unsupported "missing href attribute in <a>"
      in
      let label =
        children
        |> List.map (function
             | Xml.PCData x -> x
             | _ -> raise @@ Unsupported "forbidden content in <a>")
        |> String.concat ""
      in
      Link { target; label }
  | Element (name, _, _) ->
      raise @@ Unsupported (Printf.sprintf "unsupported element: <%s>" name)

let parse_html x =
  if x = "" then []
  else if x.[0] = '<' then
    match Xml.parse_string x with
    | Xml.Element ("markup", [], children) -> List.map markup_of_xml children
    | _ -> raise @@ Unsupported "invalid markup"
  else [ Text x ]

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
        link =
          (fun _ ~target ~label ->
            a
              ~a:[ a_href @@ Xml.uri_of_string target; a_target "_blank" ]
              [ txt label ]);
      }
    in
    try
      let xs = parse_html x in
      let xs = render p xs in
      span xs
    with _ -> span ~a:[ a_class [ "markup-error" ] ] [ txt x ]
end
