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

open Lwt.Syntax
open Js_of_ocaml
open Belenios_core.Common

let components = Hashtbl.create 2

module Default  = struct
  let lang = "en_devel"
  let s_ x = x
  let f_ x = x
end

let default = (module Default : Belenios_ui.I18n.GETTEXT)
let gettext = ref default
let dir = ref ""

let build_gettext component lang =
  let open Js_of_ocaml_lwt.XmlHttpRequest in
  let* x = Printf.ksprintf get "%slocales/%s/%s.json" !dir component lang in
  match x.code with
  | 200 ->
     let translations = Js._JSON##parse (Js.string x.content) in
     let module X =
       struct
         let lang = lang
         let s_ str_id =
           let str = Js.Unsafe.get translations (Js.string str_id) in
           Js.Optdef.case str
             (fun () -> str_id)
             (fun x -> Js.to_string (Js.Unsafe.get x 0))
         let f_ str_id =
           let str = s_ (string_of_format str_id) in
           Scanf.format_from_string str str_id
       end
     in
     Lwt.return (module X : Belenios_ui.I18n.GETTEXT)
  | _ -> Lwt.return default

let lang_mutex = Lwt_mutex.create ()

let get ~component ~lang =
  let langs =
    match Hashtbl.find_opt components component with
    | Some x -> x
    | None ->
       let x = Hashtbl.create 10 in
       Hashtbl.add components component x;
       x
  in
  match Hashtbl.find_opt langs lang with
  | Some x -> Lwt.return x
  | None ->
     let@ () = Lwt_mutex.with_lock lang_mutex in
     match Hashtbl.find_opt langs lang with
     | Some x -> Lwt.return x
     | None ->
        let* x = build_gettext component lang in
        Hashtbl.add langs lang x;
        Lwt.return x

let init ~dir:d ~component ~lang =
  dir := d;
  let* x = get ~component ~lang in
  gettext := x;
  Lwt.return_unit

let auto_init component =
  let lang = Js.to_string (Js.Unsafe.pure_js_expr "belenios_lang") in
  let dir = Js.to_string (Js.Unsafe.pure_js_expr "belenios_dir") in
  init ~dir ~component ~lang
