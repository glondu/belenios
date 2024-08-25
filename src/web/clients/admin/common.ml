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

let logout () =
  let url = "logout?cont=home@new" in
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

let default_version = List.hd Belenios.Election.supported_crypto_versions
