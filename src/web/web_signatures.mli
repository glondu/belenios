(**************************************************************************)
(*                                BELENIOS                                *)
(*                                                                        *)
(*  Copyright © 2012-2014 Inria                                           *)
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

open Serializable_builtin_t
open Serializable_t
open Signatures
open Common
open Web_serializable_t

module type AUTH_SERVICES = sig

  val auth_realm : string

  val get_auth_systems : unit -> string list
  val get_user : unit -> user option Lwt.t

end

module type AUTH_LINKS = sig

  val login :
    string option ->
    (unit, unit,
     [> `Attached of
          ([> `Internal of [> `Service ] ], [> `Get ])
          Eliom_service.a_s ],
     [ `WithoutSuffix ], unit, unit,
     [< Eliom_service.registrable > `Unregistrable ],
     [> Eliom_service.http_service ])
    Eliom_service.service

  val logout :
    (unit, unit,
     [> `Attached of
          ([> `Internal of [> `Service ] ], [> `Get ])
          Eliom_service.a_s ],
     [ `WithoutSuffix ], unit, unit,
     [< Eliom_service.registrable > `Unregistrable ],
     [> Eliom_service.http_service ])
    Eliom_service.service

end

type content =
    Eliom_registration.browser_content Eliom_registration.kind Lwt.t

module type ELECTION_HANDLERS =
  sig
    val ballot : string option Eliom_reference.eref
    val cast_confirmed : [ `Error of Web_common.error | `Valid of string ] option Eliom_reference.eref
  end

module type AUTH_HANDLERS_RAW =
  sig
    val login : string option -> unit -> content
    val logout : unit -> unit -> content
  end

type service_handler = unit ->
  Eliom_registration.browser_content Eliom_registration.kind Lwt.t

type 'a service_cont = ('a -> service_handler) -> service_handler

module type AUTH_INSTANCE_HANDLERS = sig
  val login : string service_cont
  val logout : unit service_cont
end

module type AUTH_HANDLERS = sig
  val login : string option -> unit service_cont
  val logout : unit service_cont
end

module type WEB_BALLOT_BOX = sig
  module Ballots : MONADIC_MAP_RO
    with type 'a m = 'a Lwt.t
    and type elt = string
    and type key = string
  module Records : MONADIC_MAP_RO
    with type 'a m = 'a Lwt.t
    and type elt = datetime * string
    and type key = string

  val cast : string -> string * datetime -> string Lwt.t
  val inject_cred : string -> unit Lwt.t
  val update_files : unit -> unit Lwt.t
  val update_cred : old:string -> new_:string -> unit Lwt.t
end

module type WEB_PARAMS = sig
  val metadata : metadata
  val dir : string
end

module type WEB_ELECTION_ = sig
  include ELECTION_DATA
  include WEB_PARAMS
  module E : ELECTION with type elt = G.t
  module B : WEB_BALLOT_BOX
  module Auth : sig
    module Services : AUTH_SERVICES
    module Handlers : AUTH_HANDLERS
  end
end

module type WEB_ELECTION = sig
  include WEB_ELECTION_
  module Z : ELECTION_HANDLERS
end

type election_files = {
  f_election : string;
  f_metadata : string;
  f_public_keys : string;
  f_public_creds : string;
}

module type REGISTRABLE_ELECTION = sig
  val discard : unit -> unit
  val register : unit -> (module WEB_ELECTION) Lwt.t
end

module type NAME = sig
  val name : string
  val path : string list
  val kind : [ `Site | `Election of Uuidm.t * string ]
end

module type AUTH_MAKE_INSTANCE =
  functor (N : NAME) ->
  functor (S : AUTH_SERVICES) ->
  AUTH_INSTANCE_HANDLERS

module type AUTH_SYSTEM = sig
  type config

  val name : string

  val parse_config :
    attributes:(string * string) list ->
    config option

  val make : config -> (module AUTH_MAKE_INSTANCE)
end
