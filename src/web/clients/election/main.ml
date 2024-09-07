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
open Tyxml_js.Html
open Belenios
open Belenios_js.Secondary_ui

let credential = ref None

module App (U : UI) = struct
  let component = "voter"

  let router configuration path =
    let open (val !Belenios_js.I18n.gettext) in
    U.set_title @@ s_ "Election home";
    match path with
    | [ "" ] -> Lwt.return []
    | [ uuid; "ballots" ] ->
        let* p = Ballots.ballots (Uuid.wrap uuid) in
        U.set_title p.title;
        U.set_footer p.footer;
        Lwt.return p.contents
    | [ uuid; "advanced" ] ->
        let* p = Advanced.advanced (Uuid.wrap uuid) in
        U.set_title p.title;
        U.set_footer p.footer;
        Lwt.return p.contents
    | [ uuid; "advanced"; "submit" ] ->
        let* p = Advanced.submit configuration (Uuid.wrap uuid) in
        U.set_title p.title;
        U.set_footer p.footer;
        Lwt.return p.contents
    | [ uuid; credential_ ] ->
        credential := Some credential_;
        let safe_hash = Printf.sprintf "#%s" uuid in
        Dom_html.window##.location##replace (Js.string safe_hash);
        Lwt.return []
    | [ uuid ] ->
        let credential = !credential in
        let* p = Home.home configuration ?credential (Uuid.wrap uuid) in
        U.set_title p.title;
        U.set_footer p.footer;
        Lwt.return p.contents
    | _ -> Lwt.return [ div [ txt @@ s_ "Error" ] ]
end

module _ = Make (App) ()
