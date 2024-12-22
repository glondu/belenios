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
open Belenios
open Belenios_web_api
open Session

let post_ballot uuid ~ballot =
  let* x = Api.(post (election_ballots uuid) `Nobody ballot) in
  let fail () =
    Firebug.console##log_4
      (Js.string "Submitting ballot")
      (Js.string ballot) (Js.string "returned") x;
    Lwt.return @@ Error `UnexpectedResponse
  in
  match x.code with
  | 401 -> (
      match Yojson.Safe.from_string x.content with
      | `Assoc o -> (
          match List.assoc_opt "state" o with
          | Some (`String state) -> Lwt.return @@ Ok state
          | _ -> fail ())
      | _ | (exception _) -> fail ())
  | 400 -> (
      match Belenios_web_api.request_status_of_string x.content with
      | { error = `CastError e; _ } -> Lwt.return @@ Error e
      | _ | (exception _) -> fail ())
  | _ -> fail ()

let confirmation configuration election result =
  let module B = struct
    module Xml = Tyxml_js.Xml
    module Svg = Tyxml_js.Svg
    module Html = Tyxml_js.Html

    let uris = configuration.uris
  end in
  let module U = Belenios_ui.Pages_common.Make (B) in
  let open B.Html in
  U.confirmation_fragment !I18n.gettext ~snippet:(txt "") ~progress:(txt "")
    election result
