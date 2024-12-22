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

open Belenios
open Belenios_web_api

(** Session management *)

val token : string option ref
val user : Belenios_web_api.Endpoints.admin ref
val logout : unit -> unit Lwt.t

(** Context *)

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
  | List_draft
  | List_running
  | List_old
  | Profile

val server_configuration : configuration option ref
val where_am_i : context ref
val get_current_uuid : unit -> uuid
val is_draft : unit -> bool
val is_running : unit -> bool
val is_archived : unit -> bool
val is_finished : unit -> bool

(** Misc *)

val popup_failsync : string -> unit Lwt.t
val default_version : Belenios.Election.some_version
val popup_choose_elec : uuid -> (uuid -> unit Lwt.t) -> unit Lwt.t
