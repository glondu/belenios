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
open Js_of_ocaml_tyxml
open Tyxml_js.Html5
open Belenios
open Belenios_js.Secondary_ui
open Belenios_js.Session

let fail () =
  let open (val !Belenios_js.I18n.gettext) in
  Lwt.return [ div [ txt @@ s_ "Error" ] ]

module App (U : UI) = struct
  let component = "admin"

  let router configuration path =
    let open (val !Belenios_js.I18n.gettext) in
    U.set_title @@ s_ "Trustee management";
    match path with
    | [ "check" ] -> Check.check ()
    | [ "check"; uuid ] -> Check.check ~uuid ()
    | [ uuid; token ] -> (
        let uuid = Uuid.of_string uuid in
        let@ election cont =
          let* x = Api.(get (election uuid) `Nobody) in
          match x with Error _ -> fail () | Ok (x, _) -> cont x
        in
        let module W = (val election) in
        let url = Api.trustee_election uuid (module W.G) in
        let@ trustee_status cont =
          let* x = Api.(get url (`Trustee token)) in
          match x with Error _ -> fail () | Ok (x, _) -> cont x
        in
        match trustee_status with
        | `Draft x ->
            Generate.generate configuration uuid ~token ~url (module W) x
        | `Shuffle -> Shuffle.shuffle uuid ~token
        | `Tally x -> Decrypt.decrypt uuid ~token ~url (module W) x)
    | _ -> fail ()
end

module _ = Make (App) ()
