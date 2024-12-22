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

open Belenios_storage_api
open Belenios_server_core

type data = ..
type state = { ballot : string; precast_data : Web_persist.precast_data }

type result =
  | Html : Html_types.div Eliom_content.Html.elt -> result
  | Redirection : string -> result

module type AUTH_SYSTEM = sig
  val pre_login_handler :
    [ `Username | `Address ] -> state:string -> (result * data) Lwt.t

  val direct : Yojson.Safe.t -> string Lwt.t
end

type auth_system = {
  handler : uuid option -> auth_config -> (module AUTH_SYSTEM);
  extern : bool;
}

type env = { uuid : uuid; state : state option; user : user option }

module type STATE = sig
  val create : Storage.t -> uuid -> state -> string option Lwt.t
  val get : state:string -> env option
  val del : state:string -> unit

  val get_result :
    state:string -> Belenios_api.Serializable_t.cast_result option

  val set_result :
    state:string -> Belenios_api.Serializable_t.cast_result -> unit
end

module type S = sig
  type data += No_data

  type post_login_handler = {
    post_login_handler :
      'a.
      data:data ->
      uuid option ->
      auth_config ->
      ((string * string option) option -> 'a Lwt.t) ->
      'a Lwt.t;
  }

  val register :
    auth_system:string ->
    auth_system ->
    state:string ->
    post_login_handler ->
    Eliom_registration.Html.result Lwt.t

  val get_site_login_handler : string -> result Lwt.t
  val direct_voter_auth : (Yojson.Safe.t -> user Lwt.t) Storage.u

  module State : STATE
end
