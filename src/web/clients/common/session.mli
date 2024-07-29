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
open Belenios

(** Session management *)

val init_api_token :
  (string -> unit) ->
  ?ui:string ->
  [> `Credentials of 'a * string | `Election of Uuid.t | `Error ] ->
  unit Lwt.t

(** XHR helpers *)

type xhr_result =
  | BadResult
  | BadStatus of int * string
  | RequestStatus of Belenios_api.Serializable_t.request_status

module Api : sig
  include module type of Belenios_api.Endpoints

  val get :
    (([< user ] as 'user), 'a, 'b) t ->
    'user ->
    ('a * string, xhr_result) result Lwt.t

  val put :
    ifmatch:string ->
    (([< user ] as 'user), 'a, 'b) t ->
    'user ->
    'a ->
    Js_of_ocaml_lwt.XmlHttpRequest.http_frame Lwt.t

  val post :
    ?ifmatch:string ->
    (([< user ] as 'user), 'a, 'b) t ->
    'user ->
    'b ->
    Js_of_ocaml_lwt.XmlHttpRequest.http_frame Lwt.t

  val delete :
    ?ifmatch:string ->
    (([< user ] as 'user), 'a, 'b) t ->
    'user ->
    Js_of_ocaml_lwt.XmlHttpRequest.http_frame Lwt.t
end

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

val with_ok_opt :
  string ->
  ('a * 'b, xhr_result) result ->
  ('a * 'b -> ([> Html_types.txt ] as 'c) elt list Lwt.t) ->
  ('c elt list * 'a option) Lwt.t

val with_ok_not_found :
  string ->
  ('a, xhr_result) result ->
  ('a option -> ([> Html_types.txt ] as 'b) elt list Lwt.t) ->
  'b elt list Lwt.t

(** Misc *)

val get_ifmatch : ('a * string, 'c) result -> string option
