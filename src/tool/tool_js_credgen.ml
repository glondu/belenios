(**************************************************************************)
(*                                BELENIOS                                *)
(*                                                                        *)
(*  Copyright © 2012-2020 Inria                                           *)
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
open Belenios_platform
open Belenios_tool_common
open Belenios_tool_js_common
open Tool_js_common
open Tool_credgen
open Tool_js_i18n.Gettext

let generate _ =
  let raw = get_textarea "voters" in
  let ids =
    let rec loop i accu =
      if i >= 0 then
        let j =
          match String.rindex_from_opt raw i '\n' with
          | Some x -> x
          | None -> -1
        in
        loop (j-1) (String.sub raw (j+1) (i-j) :: accu)
      else
        accu
    in loop (String.length raw - 2) []
  in
  let module P : PARAMS = struct
    let uuid = get_textarea "uuid"
    let group = get_textarea "group"
  end in
  let module X = (val make (module P : PARAMS) : S) in
  let privs, pubs = X.generate ids in
  let privs =
    List.combine ids privs
    |> List.map (fun (id, priv) -> id ^ " " ^ priv)
  in
  let text_pks = (pubs |> String.concat "\n") ^ "\n" in
  set_textarea "pks" text_pks;
  let hash = Platform.sha256_b64 text_pks in
  set_content "public_creds_fp" hash;
  let text_creds = (privs |> String.concat "\n") ^ "\n" in
  set_download "creds" "text/plain" "creds.txt" text_creds;
  set_download "voters_txt" "text/plain" "voters.txt" raw;
  set_element_display "submit_form" "inline";
  Js._false

let fill_interactivity () =
  document##getElementById (Js.string "interactivity") >>== fun e ->
  let x = Dom_html.createDiv document in
  Dom.appendChild e x;
  let b = Dom_html.createButton document in
  let t = document##createTextNode (Js.string (s_ "Generate")) in
  b##.onclick := Dom_html.handler generate;
  Dom.appendChild b t;
  Dom.appendChild x b

let () =
  Lwt.async (fun () ->
      let%lwt _ = Js_of_ocaml_lwt.Lwt_js_events.onload () in
      let belenios_lang = Js.to_string (Js.Unsafe.pure_js_expr "belenios_lang") in
      let%lwt () = Tool_js_i18n.init "admin" belenios_lang in
      fill_interactivity ();
      Lwt.return_unit
    )
