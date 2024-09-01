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
open Session

module type UI = sig
  val set_title : string -> unit
  val set_footer : Html_types.div_content_fun elt list -> unit
end

module type ROUTER = sig
  val component : string

  val router :
    configuration -> string list -> Html_types.div_content_fun elt list Lwt.t
end

module type APP = functor (_ : UI) -> ROUTER

let drop_leading_hash x =
  let n = String.length x in
  if n > 0 && x.[0] = '#' then String.sub x 1 (n - 1) else x

let make_cookie_disclaimer configuration =
  let open (val !I18n.gettext) in
  let handler _ =
    let xs = document##getElementsByClassName (Js.string "cookie-disclaimer") in
    for i = 0 to xs##.length - 1 do
      let@ x = Js.Opt.iter (xs##item i) in
      let@ p = Js.Opt.iter x##.parentNode in
      Dom.removeChild p x
    done;
    false
  in
  div
    ~a:[ a_class [ "cookie-disclaimer" ] ]
    [
      txt @@ s_ "By using this site, you accept our ";
      a ~href:configuration.uris.tos (s_ "terms of service");
      txt ". ";
      a ~href:""
        ~a:
          [
            a_class [ "nice-button"; "nice-button--default" ]; a_onclick handler;
          ]
        (s_ "Close");
    ]

module Make (App : APP) () = struct
  let full_title = span [ txt "Belenios" ]
  let main_zone = Dom_html.createDiv document
  let footer = div []

  module Ui = struct
    let title = Tyxml_js.To_dom.of_span full_title
    let footer = Tyxml_js.To_dom.of_div footer

    let set_title x =
      document##.title := Js.string x;
      title##.textContent := Js.some @@ Js.string x

    let set_footer x =
      footer##.innerHTML := Js.string "";
      List.iter (fun x -> Dom.appendChild footer (Tyxml_js.To_dom.of_node x)) x
  end

  module A = App (Ui)

  let onhashchange configuration =
    let path =
      Dom_html.window##.location##.hash
      |> Js.to_string |> drop_leading_hash |> String.split_on_char '/'
    in
    let@ () = show_in main_zone in
    A.router configuration path

  let rec main configuration lang =
    let* () = I18n.init ~dir:"static/" ~component:A.component ~lang in
    let l = !I18n.gettext in
    let module UiBase = struct
      module Xml = Tyxml_js.Xml
      module Svg = Tyxml_js.Svg
      module Html = Tyxml_js.Html

      let uris = configuration.uris
    end in
    let warning = div ~a:[ a_id "banner" ] [] in
    let () =
      let@ () = Lwt.async in
      let url = Printf.sprintf "banner?lang=%s" lang in
      let* x = Js_of_ocaml_lwt.XmlHttpRequest.get url in
      match x.code with
      | 200 ->
          let dom = Tyxml_js.To_dom.of_div warning in
          dom##.innerHTML := Js.string x.content;
          Lwt.return_unit
      | _ -> Lwt.return_unit
    in
    let module Ui = Belenios_ui.Pages_common.Make (UiBase) in
    let* () =
      let@ () = show_in document##.body in
      let* lang_box = Ui.lang_box l in
      let content = [ Tyxml_js.Of_dom.of_div main_zone ] in
      let sticky_footer = make_cookie_disclaimer configuration in
      Ui.base_body l ~lang_box ~full_title ~content ~footer ~warning
        ~sticky_footer ()
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
        let* x = Api.(get configuration `Nobody) in
        match x with
        | Error _ ->
            alert "Could not get server configuration!";
            Lwt.return_unit
        | Ok (x, _) -> cont x
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
