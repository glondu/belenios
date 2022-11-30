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
open Js_of_ocaml_tyxml
open Belenios_core.Signatures
open Belenios_core.Serializable_builtin_t
open Belenios_core.Common
open Belenios_api.Serializable_j
open Tyxml_js.Html5
open Belenios_js.Common
open Common

module CG = Belenios_core.Credential.MakeGenerate (LwtJsRandom)

let show main uuid =
  let@ () = show_in main in
  let* x = get draft_of_string "drafts/%s" uuid in
  match x with
  | Error e ->
     let msg =
       Printf.sprintf
         "An error occurred while retrieving draft %s: %s"
         uuid (string_of_error e)
     in
     Lwt.return [
         h1 [txt "Error"];
         div [txt msg];
       ]
  | Ok draft ->
     let* voters =
       let* x = get voter_list_of_string "drafts/%s/voters" uuid in
       match x with
       | Error e ->
          let msg =
            Printf.sprintf
              "An error occurred while retrieving voters of draft %s: %s"
              uuid (string_of_error e)
          in
          Lwt.return @@ div [txt msg]
       | Ok xs ->
          let fingerprint =
            String.concat "\n" xs ^ "\n"
            |> sha256_b64
          in
          let fingerprint =
            Printf.sprintf
              "The voter list has %d voter(s) and its fingerprint is %s."
              (List.length xs) fingerprint
          in
          let container = div [] |> Tyxml_js.To_dom.of_div in
          let b =
            let@ () = button "Generate credentials" in
            let uuid_ = Uuid.wrap uuid in
            let show_weight =
              List.exists
                (fun v ->
                  let _, _, weight = split_identity_opt v in
                  weight <> None
                ) xs
            in
            let version = draft.draft_version in
            let module G = (val Belenios.Group.of_string ~version draft.draft_group : GROUP) in
            let module CMap = Map.Make (G) in
            let module CD = Belenios_core.Credential.MakeDerive (G) in
            let* public_creds, private_creds =
              Lwt_list.fold_left_s (fun (public_creds, private_creds) v ->
                  let _, _, weight = split_identity v in
                  let* cred = CG.generate () in
                  let pub_cred =
                    let x = CD.derive uuid_ cred in
                    G.(g **~ x)
                  in
                  Lwt.return (CMap.add pub_cred weight public_creds, (v, cred) :: private_creds)
                ) (CMap.empty, []) xs
            in
            let private_creds = List.rev_map (fun (id, c) -> id ^ " " ^ c) private_creds in
            let public_creds =
              CMap.bindings public_creds
              |> List.map
                   (fun (cred, weight) ->
                     let cred = G.to_string cred in
                     if show_weight then
                       Printf.sprintf "%s,%s" cred (Weight.to_string weight)
                     else cred
                   )
            in
            let op = string_of_credential_list public_creds in
            let t, _ = textarea (String.concat "\n" private_creds ^ "\n") in
            let button_container = div [] |> Tyxml_js.To_dom.of_div in
            let b =
              let@ () = button "Send public credentials to server" in
              let* x = post_with_token op "drafts/%s/credentials" uuid in
              let@ () = show_in button_container in
              let msg =
                match x.code with
                | 200 -> "Public credentials successfully uploaded!"
                | code -> Printf.sprintf "Error %d: %s" code x.content
              in
              Lwt.return [txt msg]
            in
            Dom.appendChild button_container (Tyxml_js.To_dom.of_button b);
            let@ () = show_in container in
            Lwt.return [
                div [t];
                Tyxml_js.Of_dom.of_div button_container;
              ]
          in
          Dom.appendChild container (Tyxml_js.To_dom.of_button b);
          Lwt.return
          @@ div [
                 div [txt fingerprint];
                 Tyxml_js.Of_dom.of_div container;
               ]
     in
     Lwt.return [
         h1 [txt (Printf.sprintf "Credentials for %s" draft.draft_questions.t_name)];
         voters;
       ]
