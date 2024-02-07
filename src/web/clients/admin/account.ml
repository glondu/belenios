(**************************************************************************)
(*                                BELENIOS                                *)
(*                                                                        *)
(*  Copyright © 2022-2023 Inria, CNRS                                     *)
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
open Belenios
open Belenios_js.Common
open Common

let rec update_main_zone () =
  let open (val !Belenios_js.I18n.gettext) in
  let* ac = Cache.get Cache.account in
  match ac with
  | Ok acc ->
      let input_name =
        let inp, nameset = input ~a:[ a_id "inpname" ] acc.name in
        let r = Tyxml_js.To_dom.of_input inp in
        r##.onchange :=
          lwt_handler (fun _ ->
              let newname = nameset () in
              Cache.set Cache.account { acc with name = newname };
              let* () =
                let&&* container =
                  document##getElementById (Js.string "nav_username")
                in
                show_in container (fun () -> Lwt.return [ txt newname ])
              in
              update_main ());
        inp
      in
      let input_language =
        let current = Option.value ~default:"" acc.language in
        let options =
          Belenios_ui.Languages.available
          |> List.map (fun (code, label) ->
                 option ~a:[ a_value code ] (txt label))
        in
        let options =
          option ~a:[ a_value "" ] (txt @@ s_ "Navigator's default") :: options
        in
        let select = select ~a:[ a_id "inplang" ] options in
        let r = Tyxml_js.To_dom.of_select select in
        r##.value := Js.string current;
        r##.onchange :=
          lwt_handler (fun _ ->
              let language =
                match Js.to_string r##.value with "" -> None | x -> Some x
              in
              Cache.set Cache.account { acc with language };
              let* () = Belenios_js.I18n.set ~language in
              update_main ());
        select
      in
      let content =
        [
          h2 [ txt @@ s_ "Administrator's profile: " ];
          div
            [
              div [ txt (s_ "ID: " ^ string_of_int acc.id) ];
              div [ txt (s_ "E-mail: " ^ acc.address) ];
              div
                [
                  label ~a:[ a_label_for "inpname" ] [ txt @@ s_ "Name: " ];
                  input_name;
                ];
              div
                [
                  label
                    ~a:[ a_label_for "inplang" ]
                    [ txt @@ s_ "Language for administrator interface:" ];
                  txt " ";
                  input_language;
                ];
            ];
        ]
      in
      let&&* container = document##getElementById (Js.string "main_zone") in
      show_in container (fun () -> Lwt.return content)
  | Error msg ->
      alert msg;
      Lwt.return_unit

(*****************************************************)
(* called from outside, or when we redraw everything *)
and update_main () =
  let open (val !Belenios_js.I18n.gettext) in
  let () = match !where_am_i with Profile -> () | _ -> assert false in
  let&&* container = document##getElementById (Js.string "main") in
  let* () =
    let@ () = show_in container in
    let dec =
      div
        ~a:[ a_class [ "main-menu__item"; "clickable"; "noselect" ] ]
        [ txt @@ s_ "Log out" ]
    in
    let r = Tyxml_js.To_dom.of_div dec in
    r##.onclick := lwt_handler logout;
    Lwt.return
      [
        div
          ~a:[ a_class [ "main-menu" ]; a_id "main_menu" ]
          [
            div ~a:[ a_class [ "main-menu__item-separator" ] ] [];
            div ~a:[ a_class [ "main-menu__item-active" ] ] [];
            div
              ~a:[ a_class [ "main-menu__item"; "noselect"; "active" ] ]
              [ txt @@ s_ "Edit profile" ];
            div ~a:[ a_class [ "main-menu__item-separator" ] ] [];
            div ~a:[ a_class [ "main-menu__doing" ] ] [];
            dec;
            div ~a:[ a_class [ "main-menu__item-separator" ] ] [];
          ];
        div ~a:[ a_class [ "main-zone" ]; a_id "main_zone" ] [];
      ]
  in
  update_main_zone ()
