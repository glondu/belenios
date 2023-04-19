(**************************************************************************)
(*                                BELENIOS                                *)
(*                                                                        *)
(*  Copyright © 2012-2022 Inria                                           *)
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

let drop_leading_hash x =
  if x <> "" && x.[0] = '#' then String.sub x 1 (String.length x - 1) else x

let start_handler start _ =
  let hash =
    Dom_html.window##.location##.hash
    |> Js.to_string |> drop_leading_hash |> Url.decode_arguments
  in
  let params =
    match List.assoc_opt "c" hash with
    | None -> []
    | Some c -> [ ("credential", c) ]
  in
  let get id =
    let attribute = Printf.ksprintf Js.string "data-%s" id in
    Js.Opt.case
      (start##getAttribute attribute)
      (fun () -> raise Not_found)
      (fun x -> Js.to_string x)
  in
  let params =
    ("uuid", get "uuid") :: ("lang", get "lang") :: params
    |> Url.encode_arguments
  in
  let href = Printf.sprintf "%s#%s" (get "uri") params in
  Dom_html.window##.location##.href := Js.string href;
  Js._false

let lang_handler _ =
  Option.iter
    (fun x -> x##submit)
    (Dom_html.getElementById_coerce "lang_form" Dom_html.CoerceTo.form);
  Js._false

let onload _ =
  Option.iter
    (fun start -> start##.onclick := Dom_html.handler (start_handler start))
    (Dom_html.getElementById_coerce "start" Dom_html.CoerceTo.button);
  Option.iter
    (fun x -> x##.style##.display := Js.string "none")
    (Dom_html.getElementById_coerce "lang_submit" Dom_html.CoerceTo.input);
  Option.iter
    (fun x -> x##.onchange := Dom_html.handler lang_handler)
    (Dom_html.getElementById_coerce "lang_select" Dom_html.CoerceTo.select);
  Js._true

let () = Dom_html.window##.onload := Dom_html.handler onload
