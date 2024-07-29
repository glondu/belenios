(**************************************************************************)
(*                                BELENIOS                                *)
(*                                                                        *)
(*  Copyright © 2012-2023 Inria, CNRS                                     *)
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

open Js_of_ocaml
open Belenios
open Belenios_js.Common

let token = ref None
let user = ref (`Admin "")

let logout () =
  let url = "logout?cont=home" in
  Dom_html.window##.location##assign (Js.string url);
  Lwt.return_unit

(* Global information for remembering which subpage is visited. *)

(* The currently visited election context: first parameter is the uuid;
 * it can be running, still in setup, or finished. The subpage is given by a tab,
 * corresponding to the structure of the menu.
 *)
type tab =
  | Title
  | Questions
  | Voters
  | Dates
  | Language
  | Contact
  | Trustees
  | CredAuth
  | VotersPwd
  | ElectionPage
  | CreateOpenClose
  | Tally
  | Status
  | Destroy

type status = Draft | Running | Tallied | Archived

type context =
  | Election of { uuid : Uuid.t; status : status; tab : tab }
  | List_draft (* list of elections in preparation *)
  | List_running (* list of elections that are running *)
  | List_old (* list of other elections *)
  | Profile (* subpage to edit user profile *)

(* The global variable *)
let where_am_i : context ref = ref List_draft

let get_current_uuid () =
  match !where_am_i with Election { uuid; _ } -> uuid | _ -> Uuid.dummy

let is_draft () =
  match !where_am_i with Election { status = Draft; _ } -> true | _ -> false

let is_running () =
  match !where_am_i with Election { status = Running; _ } -> true | _ -> false

let is_archived () =
  match !where_am_i with
  | Election { status = Archived; _ } -> true
  | _ -> false

let is_tallied () =
  match !where_am_i with Election { status = Tallied; _ } -> true | _ -> false

let is_finished () = is_archived () || is_tallied ()

let popup_failsync msg =
  alert msg;
  Lwt.return_unit

let default_version = List.hd Belenios.Election.supported_crypto_versions

open Lwt.Syntax
open Js_of_ocaml_tyxml.Tyxml_js.Html
open Belenios_api.Serializable_j
open Belenios_js.Common
open Belenios_js.Session

(* open a popup that allows to choose an election uuid from which to
 * import something. handler takes an uuid (as raw_string) in input
 *)

let popup_choose_elec handler =
  let open (val !Belenios_js.I18n.gettext) in
  let* x = Api.(get elections !user) in
  match x with
  | Error e ->
      let msg =
        Printf.sprintf
          (f_ "An error occurred while retrieving elections: %s")
          (string_of_error e)
      in
      alert msg;
      Lwt.return_unit
  | Ok (elections, _) ->
      let@ elections cont =
        let* x = Api.(get drafts !user) in
        match x with
        | Error e ->
            let msg =
              Printf.sprintf
                (f_ "An error occurred while retrieving drafts: %s")
                (string_of_error e)
            in
            alert msg;
            Lwt.return_unit
        | Ok (drafts, _) -> cont (drafts @ elections)
      in
      let name_uuids =
        elections
        |> List.map (fun (x : summary) ->
               let but =
                 let@ () =
                   button
                   @@ Printf.sprintf "%s (%s)" x.name (Uuid.unwrap x.uuid)
                 in
                 let* () =
                   let&&* d = document##getElementById (Js.string "popup") in
                   Lwt.return (d##.style##.display := Js.string "none")
                 in
                 handler x.uuid
               in
               li [ but ])
      in
      let cancel_but =
        let@ () = button @@ s_ "Cancel" in
        let* () =
          let&&* d = document##getElementById (Js.string "popup") in
          Lwt.return (d##.style##.display := Js.string "none")
        in
        Lwt.return_unit
      in
      let content =
        [
          div
            [
              txt
              @@ s_
                   "Please select the election from which you want to import \
                    data:";
            ];
          ul name_uuids;
          cancel_but;
        ]
      in
      let* () =
        let&&* container =
          document##getElementById (Js.string "popup-content")
        in
        show_in container (fun () -> Lwt.return content)
      in
      let* () =
        let&&* d = document##getElementById (Js.string "popup") in
        Lwt.return (d##.style##.display := Js.string "block")
      in
      Lwt.return_unit
