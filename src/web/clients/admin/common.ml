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
open Belenios_core.Common
open Belenios_js.Common

let logout () =
  let url = "../logout?cont=admin@new" in
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
  | Export
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
  (match !where_am_i with Election { uuid; _ } -> uuid | _ -> Uuid.dummy)
  |> Uuid.unwrap

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

let url_prefix () =
  let x = Js.to_string Dom_html.window##.location##.href in
  let re = Regexp.regexp "^(.*)/static/admin.html(#.*)?$" in
  match Regexp.string_match re x 0 with
  | None -> "https://fake_link_please_edit"
  | Some mat -> (
      match Regexp.matched_group mat 1 with
      | None -> "https://fake_link_please_edit"
      | Some pr -> pr)

let default_version = Belenios.Election.(Version V1)

let read_full file =
  let t, u = Lwt.task () in
  let reader = new%js File.fileReader in
  reader##.onload :=
    Dom.handler (fun _ ->
        let () =
          let$ text = File.CoerceTo.string reader##.result in
          Lwt.wakeup_later u text
        in
        Js._false);
  reader##readAsText file;
  t
