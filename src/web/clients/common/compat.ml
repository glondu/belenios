(**************************************************************************)
(*                                BELENIOS                                *)
(*                                                                        *)
(*  Copyright © 2025-2025 Inria                                           *)
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

[%%import "../../../platform/config.mlh"]

open Js_of_ocaml

[%%if jsoo_version < (6, 0, 0)]

let navigator_language_raw =
  Js.Optdef.to_option Dom_html.window##.navigator##.language

let scroll x y = Dom_html.window##scroll x y

let get_file (x : #Dom_html.inputElement Js.t) =
  Js.Optdef.case x##.files
    (fun () -> None)
    (fun x -> Js.Opt.to_option (x##item 0))

let log_4 a b c d = Firebug.console##log_4 a b c d

[%%else]

let navigator_language_raw =
  Js.Opt.to_option Dom_html.window##.navigator##.language

let scroll x y =
  Dom_html.window##scroll (Js.float (float x)) (Js.float (float y))

let get_file (x : #Dom_html.inputElement Js.t) =
  let files = x##.files in
  Js.Opt.to_option (files##item 0)

let log_4 a b c d = Console.console##log_4 a b c d

[%%endif]

let navigator_language =
  match navigator_language_raw with
  | None -> "en"
  | Some x -> (
      match String.split_on_char '-' (Js.to_string x) with
      | x :: _ -> x
      | _ -> "en")
