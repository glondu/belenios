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
open Belenios_js.Common
open Belenios_js.Session

let show_result name =
  let open (val !Belenios_js.I18n.gettext) in
  let msg =
    match name with
    | None -> s_ "No matching public key was found!"
    | Some x -> Printf.sprintf (f_ "This is the private key of %s.") x
  in
  alert msg;
  Lwt.return_unit

let do_election uuid election get_private_key =
  let open (val !Belenios_js.I18n.gettext) in
  let module W = (val election : Election.ELECTION) in
  let@ trustees cont =
    let* x = Api.(get ~notoken:true (election_trustees uuid)) in
    match x with
    | Error _ ->
        alert @@ s_ "Could not get trustee parameters for this election!";
        Lwt.return_unit
    | Ok (x, _) ->
        cont
        @@ trustees_of_string (sread W.G.of_string) (sread W.G.Zq.of_string) x
  in
  let private_key = get_private_key () in
  let find_single =
    try
      match Yojson.Safe.from_string private_key with
      | `String x ->
          let private_key = W.G.Zq.of_string x in
          let public_key = W.G.(g **~ private_key) in
          fun x ->
            if W.G.compare public_key x.trustee_public_key = 0 then
              Some
                (Option.value ~default:(s_ "anonymous trustee") x.trustee_name)
            else None
      | _ -> raise Exit
    with _ -> fun _ -> None
  in
  let find_pedersen =
    try
      let module Trustees = (val Trustees.get_by_version W.version) in
      let module PKI = Trustees.MakePKI (W.G) (Random) in
      let private_key = PKI.derive_sk private_key in
      let public_key = W.G.(g **~ private_key) in
      fun x ->
        Array.find_map
          (fun ({ s_message; _ }, ({ trustee_name; _ } : _ trustee_public_key)) ->
            let x = cert_keys_of_string (sread W.G.of_string) s_message in
            if W.G.compare public_key x.cert_verification = 0 then
              Some (Option.value ~default:(s_ "anonymous trustee") trustee_name)
            else None)
          (Array.combine x.t_certs x.t_verification_keys)
    with _ -> fun _ -> None
  in
  List.find_map
    (function `Single x -> find_single x | `Pedersen x -> find_pedersen x)
    trustees
  |> show_result

let do_draft uuid draft get_private_key =
  let open (val !Belenios_js.I18n.gettext) in
  let version = draft.draft_version in
  let module G = (val Group.of_string ~version draft.draft_group) in
  let@ trustees cont =
    let* x = Api.(get ~notoken:true (draft_trustees uuid)) in
    match x with
    | Error _ ->
        alert @@ s_ "Could not get trustee parameters for this election!";
        Lwt.return_unit
    | Ok (x, _) ->
        cont
        @@ draft_trustees_of_string (sread G.of_string) (sread G.Zq.of_string) x
  in
  let private_key = get_private_key () in
  match trustees with
  | `Basic x ->
      let trustees = x.bt_trustees in
      let name =
        let@ private_key cont =
          try
            match Yojson.Safe.from_string private_key with
            | `String x -> cont (G.Zq.of_string x)
            | _ -> raise Exit
          with _ -> None
        in
        List.find_map
          (fun x ->
            let& { trustee_public_key = y; _ } = x.trustee_key in
            if G.(compare (g **~ private_key) y) = 0 then Some x.trustee_name
            else None)
          trustees
      in
      show_result name
  | `Threshold x ->
      let module Trustees = (val Trustees.get_by_version version) in
      let module PKI = Trustees.MakePKI (G) (Random) in
      let sk = PKI.derive_sk private_key in
      let vk = G.(g **~ sk) in
      let trustees = x.tt_trustees in
      let name =
        List.find_map
          (fun x ->
            let& { s_message = m; _ } = x.trustee_key in
            let y = cert_keys_of_string (sread G.of_string) m in
            if G.compare vk y.cert_verification = 0 then Some x.trustee_name
            else None)
          trustees
      in
      show_result name

let make_private_key_input () =
  let open (val !Belenios_js.I18n.gettext) in
  let open Tyxml_js.Html in
  let raw_elt = input ~a:[ a_input_type `Text; a_size 80 ] () in
  let raw_dom = Tyxml_js.To_dom.of_input raw_elt in
  let file_elt = input ~a:[ a_input_type `File ] () in
  let file_dom = Tyxml_js.To_dom.of_input file_elt in
  let onchange _ =
    let ( let& ) x f = Js.Opt.case x (fun () -> Js._false) f in
    let ( let$ ) x f = Js.Optdef.case x (fun () -> Js._false) f in
    let$ files = file_dom##.files in
    let& file = files##item 0 in
    let reader = new%js File.fileReader in
    reader##.onload :=
      Dom.handler (fun _ ->
          let& content = File.CoerceTo.string reader##.result in
          raw_dom##.value := content;
          Js._false);
    reader##readAsText file;
    Js._false
  in
  file_dom##.onchange := Dom_html.handler onchange;
  let elt =
    div
      [
        div [ txt @@ s_ "Please enter your private key:"; txt " "; raw_elt ];
        div [ txt @@ s_ "Or load it from a file:"; txt " "; file_elt ];
      ]
  in
  (elt, fun () -> Js.to_string raw_dom##.value)

let check ?uuid () =
  let open (val !Belenios_js.I18n.gettext) in
  let* body =
    match uuid with
    | None ->
        let uuid_input, get_uuid = input "" in
        let button =
          let@ () = button @@ s_ "Proceed" in
          let uuid = get_uuid () in
          match Uuid.wrap uuid with
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
        match Uuid.wrap uuid with
        | exception _ -> Lwt.return [ div [ txt @@ s_ "Invalid election ID!" ] ]
        | uuid ->
            let private_key_input, get_private_key =
              make_private_key_input ()
            in
            let button =
              let@ () = button @@ s_ "Check private key" in
              let* election = Api.(get ~notoken:true (election uuid)) in
              match election with
              | Ok (election, _) ->
                  let election = Election.of_string (module Random) election in
                  do_election uuid election get_private_key
              | Error _ -> (
                  let* draft = Api.(get ~notoken:true (draft uuid)) in
                  match draft with
                  | Error _ ->
                      Printf.ksprintf alert
                        (f_ "There is no election with ID %s on this server!")
                        (Uuid.unwrap uuid);
                      Lwt.return_unit
                  | Ok (Draft (_, draft), _) ->
                      do_draft uuid draft get_private_key)
            in
            Lwt.return [ private_key_input; div [ button ] ])
  in
  Lwt.return @@ [ h3 [ txt @@ s_ "Check private key ownership" ]; hr () ] @ body
