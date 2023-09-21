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

open Js_of_ocaml_tyxml.Tyxml_js.Html5
open Belenios_core.Common

(** Session management *)

val get_api_token : [> `Election of Uuid.t ] -> unit Lwt.t
val logout : unit -> unit Lwt.t

(** XHR helpers *)

type xhr_result =
  | BadResult
  | BadStatus of int * string
  | RequestStatus of Belenios_api.Serializable_t.request_status

type ('a, 'b) xhr_helper = ('a, unit, string, 'b Lwt.t) format4 -> 'a

type 'a raw_xhr_helper =
  ('a, Js_of_ocaml_lwt.XmlHttpRequest.http_frame) xhr_helper

val delete_with_token : ?ifmatch:string -> 'a raw_xhr_helper
val post_with_token : ?ifmatch:string -> string -> 'a raw_xhr_helper
val put_with_token : ifmatch:string -> string -> 'a raw_xhr_helper
val get : (string -> 'a) -> ('b, ('a * string, xhr_result) result) xhr_helper

(** Error handling *)

val string_of_error : xhr_result -> string

val wrap :
  (string -> 'b) ->
  Js_of_ocaml_lwt.XmlHttpRequest.http_frame Lwt.t ->
  ('b, xhr_result) result Lwt.t

val with_ok :
  string ->
  ('a, xhr_result) result ->
  ('a -> ([> Html_types.txt ] as 'b) elt list Lwt.t) ->
  'b elt list Lwt.t

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
  | Destroy

type status = Draft | Running | Tallied | Archived

type context =
  | Election of { uuid : Uuid.t; status : status; tab : tab }
  | List_draft
  | List_running
  | List_old
  | Profile

val where_am_i : context ref
val get_current_uuid : unit -> string
val is_draft : unit -> bool
val is_running : unit -> bool
val is_archived : unit -> bool
val is_finished : unit -> bool

(** Misc *)

val get_ifmatch : ('a * string, 'b) result -> string option
val popup_failsync : string -> unit Lwt.t
val url_prefix : unit -> string
