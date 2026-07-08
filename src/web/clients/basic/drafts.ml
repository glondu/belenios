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
open Belenios_web_api
open Tyxml_js.Html5
open Belenios_js.Common
open Belenios_js.Session
open Common

let show_draft_main show_all uuid draft container =
  let@ () = show_in container in
  let draft_str = !+yojson_of_draft draft in
  let t, tget = textarea draft_str in
  let ifmatch = sha256_b64 draft_str in
  let button_save =
    let@ () = button "Save changes" in
    let* x =
      Api.(put ~ifmatch (draft uuid) !user (!*draft_of_yojson (tget ())))
    in
    let@ () = show_in container in
    generic_proceed x show_all
  in
  let button_delete =
    let@ () = button "Delete draft" in
    if confirm "Are you sure?" then (
      let* x = Api.(delete (election_status uuid) !user) in
      let@ () = show_in container in
      let@ () = generic_proceed x in
      Dom_html.window##.location##.hash := Js.string "";
      Lwt.return_unit)
    else Lwt.return_unit
  in
  Lwt.return [ div [ t ]; div [ button_save ]; div [ button_delete ] ]

let rec show_draft_voters uuid draft container =
  let@ () = show_in container in
  let* x = Api.(get (draft_voters uuid) !user) in
  let@ voters, ifmatch = with_ok "voters" x in
  let voters_str = !+yojson_of_voter_list voters in
  let t, tget = textarea voters_str in
  let b =
    let@ () = button "Save changes" in
    let* x =
      Api.(
        put ~ifmatch (draft_voters uuid) !user
          (!*voter_list_of_yojson (tget ())))
    in
    let@ () = show_in container in
    generic_proceed x (fun () -> show_draft_voters uuid draft container)
  in
  let import =
    let i, iget = input `Text in
    let b =
      let@ () = button "Import voters" in
      let r = `Import (Uuid.of_string (iget ())) in
      let* x = Api.(post ~ifmatch (draft_voters uuid) !user r) in
      let@ () = show_in container in
      generic_proceed x (fun () -> show_draft_voters uuid draft container)
    in
    div [ i; b ]
  in
  Lwt.return [ div [ t ]; div [ b ]; import ]

let rec show_draft_credentials : 'a 'b. uuid -> ('a, 'b) group -> _ -> _ =
 fun uuid (type a b) (w : (a, b) group) container ->
  let@ () = show_in container in
  let* x = Api.(get (draft_public_credentials uuid w) !user) in
  let@ x = with_ok_not_found "credentials" x in
  match x with
  | None -> (
      let* x = Api.(get (draft_credentials_token uuid) !user) in
      let@ x = with_ok_not_found "token" x in
      match x with
      | None ->
          let b =
            let@ () = button "Generate on server" in
            let* x = Api.(post (draft_public_credentials uuid w) !user []) in
            let@ () = show_in container in
            generic_proceed x (fun () ->
                show_draft_credentials uuid w container)
          in
          Lwt.return [ b ]
      | Some (token, _) ->
          let link =
            Js.to_string Dom_html.window##.location##.href ^ "@" ^ token
          in
          let module X = Belenios_ui.Mails_admin.Make (Belenios_js.I18n) in
          let subject, body =
            X.mail_credential_authority !Belenios_js.I18n.gettext link
          in
          Lwt.return
            [
              a_mailto ~recipient:"" ~subject ~body
                "Send an e-mail to the credential authority";
              txt " ";
              txt "or send the following link manually:";
              txt " ";
              txt link;
            ])
  | Some (x, _) ->
      let module G = (val w) in
      let t, _ = textarea (!+(yojson_of_public_credentials !&G.to_string) x) in
      Lwt.return [ t ]

let rec show_draft_status uuid container =
  let@ () = show_in container in
  let* x = Api.(get (draft_status uuid) !user) in
  let@ status, _ = with_ok "status" x in
  let t, _ = textarea (!+yojson_of_draft_status status) in
  let b label r =
    let@ () = button label in
    let* x = Api.(post (draft uuid) !user r) in
    let@ () = show_in container in
    let@ () = generic_proceed x in
    match (r, x.code) with
    | `ValidateElection, 200 ->
        let new_hash = Printf.sprintf "#elections/%s" (Uuid.to_string uuid) in
        Dom_html.window##.location##.hash := Js.string new_hash;
        Lwt.return_unit
    | _ -> show_draft_status uuid container
  in
  let buttons =
    div
      [
        b "Set downloaded" `SetDownloaded;
        b "Validate election" `ValidateElection;
      ]
  in
  Lwt.return [ div [ t ]; buttons ]

let suffix_and_label_of_draft_tab = function
  | `Draft -> ("", "Draft")
  | `Voters -> ("/voters", "Voters")
  | `Credentials -> ("/credentials", "Credentials")
  | `Trustees -> ("/trustees", "Trustees")
  | `Status -> ("/status", "Status")

let show_draft show_all uuid draft title container tab =
  container##.innerHTML := Js.string "Loading...";
  let _, label = suffix_and_label_of_draft_tab tab in
  let* () =
    let@ () = show_in title in
    Lwt.return [ txt label ]
  in
  match tab with
  | `Draft -> show_draft_main show_all uuid draft container
  | `Voters -> show_draft_voters uuid draft container
  | `Credentials ->
      let (Draft (_, draft)) = draft in
      let { version; group; _ } : raw_draft = draft in
      let module G = (val Group.make { version; group }) in
      show_draft_credentials uuid (module G) container
  | `Status -> show_draft_status uuid container

let a_draft_tab uuid tab =
  let suffix, label = suffix_and_label_of_draft_tab tab in
  let href = Printf.sprintf "#drafts/%s%s" (Uuid.to_string uuid) suffix in
  a ~href label

let show main uuid tab context =
  let rec show_all () =
    let* x = Api.(get (draft uuid) `Nobody) in
    match x with
    | Error e ->
        let@ () = show_in main in
        let msg =
          Printf.sprintf "An error occurred while retrieving draft %s: %s"
            (Uuid.to_string uuid) (string_of_error e)
        in
        Lwt.return [ h1 [ txt "Error" ]; div [ txt msg ] ]
    | Ok ((Draft (_, d) as draft), _) ->
        let title = h2 [] in
        let container = div [] in
        let* () =
          let@ () = show_in main in
          let tabs =
            ul
              [
                li [ a_draft_tab uuid `Draft ];
                li [ a_draft_tab uuid `Voters ];
                li [ a_draft_tab uuid `Credentials ];
                li [ a_draft_tab uuid `Trustees ];
                li [ a_draft_tab uuid `Status ];
              ]
          in
          Lwt.return
            [
              div [ a ~href:"#" "Home" ];
              h1 [ txt d.questions.name ];
              tabs;
              title;
              container;
            ]
        in
        let title = Tyxml_js.To_dom.of_h2 title in
        let container = Tyxml_js.To_dom.of_div container in
        context := `Draft (draft, title, container);
        show_draft show_all uuid draft title container tab
  in
  match !context with
  | `Draft (draft, title, container) ->
      show_draft show_all uuid draft title container tab
  | _ -> show_all ()
