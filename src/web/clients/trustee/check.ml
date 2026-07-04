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
open Belenios_web_api
open Belenios
open Belenios_js.Common
open Belenios_js.Session
open Common

let show_result name =
  let open (val !Belenios_js.I18n.gettext) in
  let msg =
    match name with
    | None -> s_ "No matching public key was found!"
    | Some None ->
        s_ "This private key is valid but no name is associated with it!"
    | Some (Some x) -> Printf.sprintf (f_ "This is the private key of %s.") x
  in
  alert msg;
  Lwt.return_unit

let do_election uuid (election : Election.t) seed =
  let open (val !Belenios_js.I18n.gettext) in
  let module W = (val election) in
  let module G = W.G in
  let@ trustees cont =
    let* x = Api.(get (election_trustees uuid) `Nobody) in
    match x with
    | Error _ ->
        alert @@ s_ "Could not get trustee parameters for this election!";
        Lwt.return_unit
    | Ok (x, _) -> cont @@ !*[%group_of_yojson: _ trustees] x
  in
  let find_single =
    let module T = (val Trustees.get_by_version W.version) in
    let module KG = T.MakeBasic (W.G) in
    let private_key = KG.derive seed in
    let public_key = W.G.(g **~ private_key) in
    fun (x : _ basic_parameters) ->
      if
        W.G.compare public_key x.verification_key.message.message.public_key = 0
      then Some x.verification_key.message.message.name
      else None
  in
  let find_pedersen =
    try
      let module Trustees = (val Trustees.get_by_version W.version) in
      let module PKI = Pki.Make (W.G) in
      let private_key = PKI.derive_sk seed in
      let public_key = W.G.(g **~ private_key) in
      fun x ->
        Array.find_map
          (fun ({ message = x; _ }, (t : _ threshold_verification_key)) ->
            let { name; _ } : _ raw_trustee_public_key = t.message.message in
            if W.G.compare public_key x.verification = 0 then Some name
            else None)
          (Array.combine x.certs x.verification_keys)
    with _ -> fun _ -> None
  in
  List.find_map
    (function `Single x -> find_single x | `Pedersen x -> find_pedersen x)
    trustees
  |> show_result

let do_draft uuid (draft : raw_draft) private_key =
  let open (val !Belenios_js.I18n.gettext) in
  let version = draft.version in
  let module G = (val Group.of_string ~version draft.group) in
  let@ trustees cont =
    let* x = Api.(get (draft_trustees uuid (module G)) `Nobody) in
    match x with
    | Error _ ->
        alert @@ s_ "Could not get trustee parameters for this election!";
        Lwt.return_unit
    | Ok (x, _) -> cont x
  in
  match trustees with
  | `Basic x ->
      let trustees = x.trustees in
      let name =
        let@ private_key cont =
          try
            match Json.of_string private_key with
            | `String x -> cont (G.Zq.of_string x)
            | _ -> raise Exit
          with _ -> None
        in
        List.find_map
          (fun (x : _ basic_parameters trustee) ->
            let& y = x.key in
            let { public_key = y; _ } : _ raw_trustee_public_key =
              y.verification_key.message.message
            in
            if G.(compare (g **~ private_key) y) = 0 then Some x.name else None)
          trustees
      in
      show_result name
  | `Threshold x ->
      let module Trustees = (val Trustees.get_by_version version) in
      let module PKI = Pki.Make (G) in
      let sk = PKI.derive_sk private_key in
      let vk = G.(g **~ sk) in
      let trustees = x.trustees in
      let name =
        List.find_map
          (fun x ->
            let& { message = y; _ } = x.key in
            if G.compare vk y.verification = 0 then Some x.name else None)
          trustees
      in
      show_result name

let check ?uuid () =
  let open (val !Belenios_js.I18n.gettext) in
  let* body =
    match uuid with
    | None ->
        let uuid_input, get_uuid = input `Text in
        let button =
          let@ () = button @@ s_ "Proceed" in
          let uuid = get_uuid () in
          match Uuid.of_string uuid with
          | exception _ ->
              alert @@ s_ "Invalid election ID!";
              Lwt.return_unit
          | _ ->
              Dom_html.window##.location##.hash
              := Js.string @@ Printf.sprintf "#check/%s" uuid;
              Lwt.return_unit
        in
        Lwt.return
          [
            div [ label [ txt @@ s_ "Election ID:"; txt " "; uuid_input ] ];
            div [ button ];
          ]
    | Some uuid -> (
        match Uuid.of_string uuid with
        | exception _ -> Lwt.return [ div [ txt @@ s_ "Invalid election ID!" ] ]
        | uuid ->
            let handle_private_key private_key =
              let* election = Api.(get (election uuid) `Nobody) in
              match election with
              | Ok (election, _) -> do_election uuid election private_key
              | Error _ -> (
                  let* draft = Api.(get (draft uuid) `Nobody) in
                  match draft with
                  | Error _ ->
                      Printf.ksprintf alert
                        (f_ "There is no election with ID %s on this server!")
                        (Uuid.to_string uuid);
                      Lwt.return_unit
                  | Ok (Draft (_, draft), _) -> do_draft uuid draft private_key)
            in
            let private_key_input = make_private_key_input handle_private_key in
            Lwt.return [ private_key_input ])
  in
  Lwt.return @@ [ h3 [ txt @@ s_ "Check private key ownership" ]; hr () ] @ body
