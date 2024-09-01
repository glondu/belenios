(**************************************************************************)
(*                                BELENIOS                                *)
(*                                                                        *)
(*  Copyright © 2012-2023 Inria                                           *)
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
open Belenios_api.Serializable_j
open Belenios_api
open Tyxml_js.Html5
open Belenios_js.Common
open Belenios_js.Session

let show main uuid =
  let@ () = show_in main in
  let* x = Api.(get ~notoken:true (draft uuid)) in
  match x with
  | Error e ->
      let msg =
        Printf.sprintf "An error occurred while retrieving draft %s: %s"
          (Uuid.unwrap uuid) (string_of_error e)
      in
      Lwt.return [ h1 [ txt "Error" ]; div [ txt msg ] ]
  | Ok (Draft (_, draft), _) ->
      let* voters =
        let* x = Api.(get (draft_voters uuid)) in
        match x with
        | Error e ->
            let msg =
              Printf.sprintf
                "An error occurred while retrieving voters of draft %s: %s"
                (Uuid.unwrap uuid) (string_of_error e)
            in
            Lwt.return @@ div [ txt msg ]
        | Ok (xs, _) ->
            let fingerprint = Voter.list_to_string xs |> sha256_b64 in
            let fingerprint =
              Printf.sprintf
                "The voter list has %d voter(s) and its fingerprint is %s."
                (List.length xs) fingerprint
            in
            let container = div [] |> Tyxml_js.To_dom.of_div in
            let b =
              let@ () = button "Generate credentials" in
              let version = draft.draft_version in
              let module G =
                (val Belenios.Group.of_string ~version draft.draft_group
                    : GROUP)
              in
              let module CMap = Map.Make (G) in
              let module Cred =
                Credential.Make
                  (G)
                  (struct
                    type 'a t = 'a Lwt.t

                    let return = Lwt.return
                    let bind = Lwt.bind
                    let pause = Lwt.pause
                    let uuid = uuid
                    let get_salt _ = Lwt.return_none
                  end)
              in
              let* Credential.{ public_creds; private_creds; _ } =
                Cred.generate xs
              in
              let t, _ =
                textarea (string_of_private_credentials private_creds)
              in
              let button_container = div [] |> Tyxml_js.To_dom.of_div in
              let b =
                let@ () = button "Send public credentials to server" in
                let* x =
                  Api.(post (draft_public_credentials uuid) public_creds)
                in
                let@ () = show_in button_container in
                let msg =
                  match x.code with
                  | 200 -> "Public credentials successfully uploaded!"
                  | code -> Printf.sprintf "Error %d: %s" code x.content
                in
                Lwt.return [ txt msg ]
              in
              Dom.appendChild button_container (Tyxml_js.To_dom.of_button b);
              let@ () = show_in container in
              Lwt.return [ div [ t ]; Tyxml_js.Of_dom.of_div button_container ]
            in
            Dom.appendChild container (Tyxml_js.To_dom.of_button b);
            Lwt.return
            @@ div [ div [ txt fingerprint ]; Tyxml_js.Of_dom.of_div container ]
      in
      Lwt.return
        [
          h1
            [
              txt
                (Printf.sprintf "Credentials for %s"
                   draft.draft_questions.t_name);
            ];
          voters;
        ]
