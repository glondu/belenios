(**************************************************************************)
(*                                BELENIOS                                *)
(*                                                                        *)
(*  Copyright © 2012-2021 Inria                                           *)
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

open Web_serializable_t
open Belenios_api.Serializable_t

val new_token : account -> string Lwt.t
val lookup_token : string -> account option
val invalidate_token : string -> unit

exception Error of string

type result = int * string

type body = {
    run : 'a. (string -> 'a) -> ('a -> result Lwt.t) -> result Lwt.t
}

val ok : result Lwt.t
val bad_request : result Lwt.t
val unauthorized : result Lwt.t
val forbidden : result Lwt.t
val not_found  : result Lwt.t
val method_not_allowed : result Lwt.t

val handle_ifmatch :
  string option -> (unit -> string Lwt.t) ->
  (unit -> result Lwt.t) -> result Lwt.t

val handle_generic_error :
  (unit -> result Lwt.t) -> result Lwt.t

val handle_get :
  (unit -> string Lwt.t) -> result Lwt.t

val get_configuration : unit -> configuration

val get_account : account -> api_account
val put_account : account -> api_account -> unit Lwt.t
