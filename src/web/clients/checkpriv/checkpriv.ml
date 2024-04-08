(**************************************************************************)
(*                                BELENIOS                                *)
(*                                                                        *)
(*  Copyright © 2012-2022 Inria                                           *)
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
open Belenios_api.Common
open Belenios
open Belenios_js.Common

let show_result name =
  let open (val !Belenios_js.I18n.gettext) in
  let msg =
    match name with
    | None -> s_ "No matching public key was found!"
    | Some x -> Printf.sprintf (f_ "This is the private key of %s.") x
  in
  alert msg;
  Lwt.return_unit

let perform what f url cont =
  let open (val !Belenios_js.I18n.gettext) in
  let open Js_of_ocaml_lwt.XmlHttpRequest in
  let* x = get url in
  match x.code with
  | 200 -> cont (f x.content)
  | code ->
      Printf.ksprintf alert (f_ "Could not get %s (code %d)!") what code;
      Lwt.return_unit

let try_perform what f url cont =
  let open (val !Belenios_js.I18n.gettext) in
  let open Js_of_ocaml_lwt.XmlHttpRequest in
  let* x = get url in
  match x.code with
  | 200 -> cont (Some (f x.content))
  | 404 -> cont None
  | code ->
      Printf.ksprintf alert (f_ "Could not get %s (code %d)!") what code;
      Lwt.return_unit

let compute_prefix url =
  let l = String.length url in
  if l > 0 then
    let rec loop n i =
      if n > 0 then
        match String.rindex_from_opt url i '/' with
        | None -> String.sub url 0 (i + 1)
        | Some j -> loop (n - 1) (j - 1)
      else String.sub url 0 (i + 1)
    in
    loop 2 (l - 1)
  else url

let extract_uuid hash =
  let l = String.length hash in
  if l > 0 then
    let i = if hash.[0] = '#' then 1 else 0 in
    let n = l - i in
    if n > 0 then Some (String.sub hash i n) else None
  else None

let prefix = Dom_html.window##.location##.href |> Js.to_string |> compute_prefix
let uuid = Dom_html.window##.location##.hash |> Js.to_string |> extract_uuid

let do_election uuid election get_private_key =
  let open (val !Belenios_js.I18n.gettext) in
  let module W = (val election : ELECTION) in
  let@ trustees =
    perform (s_ "trustee parameters")
      (trustees_of_string (sread W.G.of_string) (sread W.G.Zq.of_string))
      (Printf.sprintf "../api/elections/%s/trustees" uuid)
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
  let@ trustees =
    perform (s_ "trustee parameters")
      (draft_trustees_of_string (sread G.of_string) (sread G.Zq.of_string))
      (Printf.sprintf "../api/drafts/%s/trustees" uuid)
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

let onload () =
  let lang =
    Js.Optdef.case
      Dom_html.window##.navigator##.language
      (fun () -> "en")
      Js.to_string
  in
  let* () = Belenios_js.I18n.init ~dir:"" ~component:"admin" ~lang in
  let open (val !Belenios_js.I18n.gettext) in
  let title = s_ "Check private key ownership" in
  document##.title := Js.string title;
  let@ configuration =
    perform
      (s_ "server configuration")
      configuration_of_string "../api/configuration"
  in
  let module UiBase = struct
    module Xml = Tyxml_js.Xml
    module Svg = Tyxml_js.Svg
    module Html = Tyxml_js.Html

    module Uris = struct
      let home = prefix ^ "/"
      let logo = prefix ^ "/LOGO"
      let belenios = Belenios_ui.Links.belenios
      let source_code = prefix ^ "/belenios.tar.gz"
      let tos = configuration.tos
    end
  end in
  let module Ui = Belenios_ui.Pages_common.Make (UiBase) in
  let@ () = show_in document##.body in
  let uuid, get_uuid = input (Option.value ~default:"" uuid) in
  let private_key, get_private_key = make_private_key_input () in
  let button =
    let@ () = button @@ s_ "Check private key" in
    let uuid = get_uuid () in
    match Uuid.wrap uuid with
    | exception _ ->
        alert @@ s_ "The UUID is invalid!";
        Lwt.return_unit
    | _ -> (
        let parse_election x =
          let module X =
            Election.Make
              (struct
                let raw_election = x
              end)
              (Random)
              ()
          in
          (module X : ELECTION)
        in
        let@ election =
          try_perform (s_ "election parameters") parse_election
            (Printf.sprintf "../api/elections/%s/election" uuid)
        in
        match election with
        | Some election -> do_election uuid election get_private_key
        | None -> (
            let@ draft =
              try_perform
                (s_ "draft election parameters")
                draft_of_string
                (Printf.sprintf "../api/drafts/%s" uuid)
            in
            match draft with
            | None ->
                Printf.ksprintf alert
                  (f_ "There is no election with UUID %s on this server!")
                  uuid;
                Lwt.return_unit
            | Some (Draft (_, draft)) -> do_draft uuid draft get_private_key))
  in
  let content =
    [
      div [ label [ txt @@ s_ "UUID of the election:"; txt " "; uuid ] ];
      private_key;
      div [ button ];
    ]
  in
  let administer = a ~href:(prefix ^ "/admin") @@ s_ "Administer elections" in
  Lwt.return
    (Ui.base_body !Belenios_js.I18n.gettext
       ~full_title:(span [ txt title ])
       ~content ~administer ())

let () = Dom_html.window##.onload := lwt_handler onload
