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

type 'a rendering_functions =
  {
    text : int -> string -> 'a;
    br : int -> 'a;
    bold : int -> 'a list -> 'a;
    italic : int -> 'a list -> 'a;
  }

let rec render p xs =
  List.mapi (render_item p) xs

and render_item p i = function
  | Markup_types.Text s -> p.text i s
  | Br -> p.br i
  | Bold xs -> p.bold i (render p xs)
  | Italic xs -> p.italic i (render p xs)
