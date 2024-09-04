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

open Js_of_ocaml_tyxml
open Tyxml_js.Html
open Belenios
open Belenios_js.Common

type page = {
  title : string;
  contents : Html_types.div_content_fun elt list;
  footer : Html_types.div_content_fun elt list;
}

let error x = { title = "Error"; contents = [ txt x ]; footer = [] }

let make_audit_footer election =
  let open (val !Belenios_js.I18n.gettext) in
  let open (val election : Election.ELECTION) in
  let uuid = Uuid.unwrap uuid in
  let parameters = !/(Printf.sprintf "elections/%s/election" uuid) in
  let public_data = !/(Printf.sprintf "elections/%s/archive" uuid) in
  let advanced = !!(Printf.sprintf "actions/cast?uuid=%s" uuid) in
  let administer = !!(Printf.sprintf "actions/admin?uuid=%s" uuid) in
  div
    ~a:[ a_style "line-height:1.5em;" ]
    [
      div
        [
          div [ txt @@ s_ "Election fingerprint: "; code [ txt fingerprint ] ];
          div
            [
              txt @@ s_ "Audit data: ";
              a ~href:parameters (s_ "parameters");
              txt ", ";
              a ~href:public_data (s_ "public data");
              txt ". ";
              a ~href:advanced (s_ "Advanced mode");
              txt ". ";
              a ~href:administer
                ~a:[ a_id @@ Printf.sprintf "election_admin_%s" uuid ]
                (s_ "Administer this election");
              txt ".";
            ];
        ];
    ]
