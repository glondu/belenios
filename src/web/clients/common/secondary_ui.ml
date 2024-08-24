(**************************************************************************)
(*                                BELENIOS                                *)
(*                                                                        *)
(*  Copyright © 2024-2024 Inria                                           *)
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
open Tyxml_js.Html5
open Belenios_api.Serializable_j
open Belenios
open Common

module type UI = sig
  val router :
    configuration -> string list -> Html_types.div_content_fun elt list Lwt.t

  val title : unit -> string
end

let drop_leading_hash x =
  let n = String.length x in
  if n > 0 && x.[0] = '#' then String.sub x 1 (n - 1) else x

module Make (U : UI) () = struct
  let main_zone = Dom_html.createDiv document

  let onhashchange configuration =
    let path =
      Dom_html.window##.location##.hash
      |> Js.to_string |> drop_leading_hash |> String.split_on_char '/'
    in
    let@ () = show_in main_zone in
    U.router configuration path

  let rec main configuration lang =
    let* () = I18n.init ~dir:"" ~component:"admin" ~lang in
    let l = !I18n.gettext in
    let title = U.title () in
    document##.title := Js.string title;
    let module UiBase = struct
      module Xml = Tyxml_js.Xml
      module Svg = Tyxml_js.Svg
      module Html = Tyxml_js.Html

      let uris = configuration.uris
    end in
    let module Ui = Belenios_ui.Pages_common.Make (UiBase) in
    let* () =
      let@ () = show_in document##.body in
      let* lang_box = Ui.lang_box l in
      let content = [ Tyxml_js.Of_dom.of_div main_zone ] in
      Ui.base_body l ~lang_box ~full_title:(span [ txt title ]) ~content ()
      |> Lwt.return
    in
    let () =
      Js.Opt.iter
        (Dom_html.document##getElementById (Js.string "lang_select"))
        (fun e ->
          Js.Opt.iter (Dom_html.CoerceTo.select e) (fun e ->
              e##.onchange :=
                let@ () = lwt_handler in
                main configuration (Js.to_string e##.value)))
    in
    onhashchange configuration

  let () =
    Dom_html.window##.onload :=
      let@ () = lwt_handler in
      let@ configuration cont =
        let* x = get configuration_of_string "../api/configuration" in
        match x with
        | None ->
            alert "Could not get server configuration!";
            Lwt.return_unit
        | Some x -> cont x
      in
      let lang =
        Js.Optdef.case
          Dom_html.window##.navigator##.language
          (fun () -> "en")
          (fun x ->
            match String.split_on_char '-' (Js.to_string x) with
            | x :: _ -> x
            | _ -> "en")
      in
      let () =
        Dom_html.window##.onhashchange
        :=
        let@ () = lwt_handler in
        onhashchange configuration
      in
      main configuration lang
end
