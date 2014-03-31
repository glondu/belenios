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

  val login :
    (string option, unit,
     [> `Attached of
          ([> `Internal of [> `Service ] ], [> `Get ])
          Eliom_service.a_s ],
     [ `WithoutSuffix ],
     [ `One of string ] Eliom_parameter.param_name, unit,
     [< Eliom_service.registrable > `Registrable ], 'a)
    Eliom_service.service

  val logout :
    (unit, unit,
     [> `Attached of
          ([> `Internal of [> `Service ] ], [> `Get ])
          Eliom_service.a_s ],
     [ `WithoutSuffix ], unit, unit,
     [< Eliom_service.registrable > `Registrable ], 'a)
    Eliom_service.service

end

module type CORE_SERVICES = sig

  val home :
    (unit, unit,
     [> `Attached of
          ([> `Internal of [> `Service ] ], [> `Get ])
          Eliom_service.a_s ],
     [ `WithoutSuffix ], unit, unit,
     [< Eliom_service.registrable > `Registrable ], 'a)
    Eliom_service.service

  val admin :
    (unit, unit,
     [> `Attached of
          ([> `Internal of [> `Service ] ], [> `Get ])
          Eliom_service.a_s ],
     [ `WithoutSuffix ], unit, unit,
     [< Eliom_service.registrable > `Registrable ], 'a)
    Eliom_service.service

  val source_code :
    (unit, unit,
     [> `Attached of
          ([> `Internal of [> `Service ] ], [> `Get ])
          Eliom_service.a_s ],
     [ `WithoutSuffix ], unit, unit,
     [< Eliom_service.registrable > `Registrable ], 'a)
    Eliom_service.service

  val get_randomness :
    (unit, unit,
     [> `Attached of
          ([> `Internal of [> `Service ] ], [> `Get ])
          Eliom_service.a_s ],
     [ `WithoutSuffix ], unit, unit,
     [< Eliom_service.registrable > `Registrable ], 'a)
    Eliom_service.service

end

module type ELECTION_SERVICES = sig
  include AUTH_SERVICES

  val home :
    (unit, unit,
     [> `Attached of
          ([> `Internal of [> `Service ] ], [> `Get ])
          Eliom_service.a_s ],
     [ `WithoutSuffix ], unit, unit,
     [< Eliom_service.registrable > `Registrable ], 'a)
    Eliom_service.service

  val admin :
    (unit, unit,
     [> `Attached of
          ([> `Internal of [> `Service ] ], [> `Get ])
          Eliom_service.a_s ],
     [ `WithoutSuffix ], unit, unit,
     [< Eliom_service.registrable > `Registrable ], 'a)
    Eliom_service.service

  val election_dir :
    (Web_common.election_file, unit,
     [> `Attached of
          ([> `Internal of [> `Service ] ], [> `Get ])
          Eliom_service.a_s ],
     [ `WithSuffix ],
     [ `One of Web_common.election_file ] Eliom_parameter.param_name,
     unit, [< Eliom_service.registrable > `Registrable ], 'a)
    Eliom_service.service

  val booth :
    (unit, unit,
     [> `Attached of
          ([> `Internal of [> `Service ] ], [> `Get ])
          Eliom_service.a_s ],
     [ `WithoutSuffix ], unit, unit,
     [< Eliom_service.registrable > `Unregistrable ], 'a)
    Eliom_service.service

  val election_update_credential :
    (unit, unit,
     [> `Attached of
          ([> `Internal of [> `Service ] ], [> `Get ])
          Eliom_service.a_s ],
     [ `WithoutSuffix ], unit, unit,
     [< Eliom_service.registrable > `Registrable ], 'a)
    Eliom_service.service

  val election_update_credential_post :
    (unit, string * string,
     [> `Attached of
          ([> `Internal of [ `Coservice | `Service ] ], [> `Post ])
          Eliom_service.a_s ],
     [ `WithoutSuffix ], unit,
     [ `One of string ] Eliom_parameter.param_name *
     [ `One of string ] Eliom_parameter.param_name,
     [< Eliom_service.registrable > `Registrable ], 'a)
    Eliom_service.service

  val election_vote :
    (unit, unit,
     [> `Attached of
          ([> `Internal of [> `Service ] ], [> `Get ])
          Eliom_service.a_s ],
     [ `WithoutSuffix ], unit,
     unit, [< Eliom_service.registrable > `Registrable ], 'a)
    Eliom_service.service

  val election_cast :
    (unit, unit,
     [> `Attached of
          ([> `Internal of [> `Service ] ], [> `Get ])
          Eliom_service.a_s ],
     [ `WithoutSuffix ], unit,
     unit, [< Eliom_service.registrable > `Registrable ], 'a)
    Eliom_service.service

  val election_cast_post :
    (unit, string option * Eliom_lib.file_info option,
     [> `Attached of
          ([> `Internal of [ `Coservice | `Service ] ], [> `Post ])
          Eliom_service.a_s ],
     [ `WithoutSuffix ], unit,
     [ `One of string ] Eliom_parameter.param_name *
     [ `One of Eliom_lib.file_info ] Eliom_parameter.param_name,
     [< Eliom_service.registrable > `Registrable ], 'a)
    Eliom_service.service

end

type service_handler = unit ->
  (Eliom_registration.browser_content,
   Eliom_registration.http_service
  ) Eliom_registration.kind Lwt.t

type 'a service_cont = ('a -> service_handler) -> service_handler

module type AUTH_HANDLERS = sig
  val login : string service_cont
  val logout : unit service_cont
end

module type AUTH_HANDLERS_PUBLIC = sig
  val do_login : unit service_cont
  val do_logout : unit service_cont
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
  val extract_creds : unit -> SSet.t Lwt.t
  val update_cred : old:string -> new_:string -> unit Lwt.t
end

module type ELECTION_TEMPLATES = sig

  val home :
    unit -> [> `Html ] Eliom_content.Html5.F.elt Lwt.t

  val admin :
    unit -> [> `Html ] Eliom_content.Html5.F.elt Lwt.t

  val update_credential :
    unit -> [> `Html ] Eliom_content.Html5.F.elt Lwt.t

  val cast_raw :
    unit -> [> `Html ] Eliom_content.Html5.F.elt Lwt.t

  val cast_confirmation :
    confirm:(unit ->
             (unit, 'b,
              [< Eliom_service.post_service_kind ],
              [< Eliom_service.suff ], 'c, unit,
              [< Eliom_service.registrable ], 'd)
             Eliom_service.service) ->
    can_vote:bool ->
    unit -> [> `Html ] Eliom_content.Html5.F.elt Lwt.t

  val cast_confirmed :
    result:[< `Error of Web_common.error | `Valid of string ] ->
    unit -> [> `Html ] Eliom_content.Html5.F.elt Lwt.t

end

module type WEB_PARAMS = sig
  val metadata : metadata
  val featured : bool
  val dir : string
end

module type WEB_ELECTION_RO = sig
  include ELECTION_DATA
  include WEB_PARAMS
  module E : ELECTION with type elt = G.t
  module S : ELECTION_SERVICES
end

module type WEB_ELECTION = sig
  include WEB_ELECTION_RO
  module B : WEB_BALLOT_BOX
  module H : AUTH_HANDLERS_PUBLIC
end

module type SITE_SERVICES = sig
  include CORE_SERVICES
  include AUTH_SERVICES
end

type election_files = {
  f_election : string;
  f_metadata : string;
  f_public_keys : string;
  f_public_creds : string;
}

module type SITE = sig
  include SITE_SERVICES
  include AUTH_HANDLERS_PUBLIC
  val import_election :
    featured:bool -> election_files -> (module WEB_ELECTION) option Lwt.t
  val set_main_election : string -> unit Lwt.t
  val unset_main_election : unit -> unit Lwt.t
  val add_featured_election : string -> unit Lwt.t
  val remove_featured_election : string -> unit Lwt.t
  val cont : (unit -> service_handler) Eliom_reference.eref
end

module type LOGIN_TEMPLATES = sig

  val dummy :
    service:(unit, 'a, [< Eliom_service.post_service_kind ],
             [< Eliom_service.suff ], 'b,
             [< string Eliom_parameter.setoneradio ]
             Eliom_parameter.param_name,
             [< Eliom_service.registrable ], 'c)
            Eliom_service.service ->
    unit -> [> `Html ] Eliom_content.Html5.F.elt Lwt.t

  val password :
    service:(unit, 'a, [< Eliom_service.post_service_kind ],
             [< Eliom_service.suff ], 'b,
             [< string Eliom_parameter.setoneradio ]
             Eliom_parameter.param_name *
             [< string Eliom_parameter.setoneradio ]
             Eliom_parameter.param_name,
             [< Eliom_service.registrable ], 'c)
            Eliom_service.service ->
    unit -> [> `Html ] Eliom_content.Html5.F.elt Lwt.t

  val choose :
    unit -> [> `Html ] Eliom_content.Html5.F.elt Lwt.t

end

module type TEMPLATES = sig

  val home :
    featured:(module WEB_ELECTION_RO) list ->
    unit -> [> `Html ] Eliom_content.Html5.F.elt Lwt.t

  val admin :
    elections:(module WEB_ELECTION_RO) list ->
    unit -> [> `Html ] Eliom_content.Html5.F.elt Lwt.t

  module Login (S : AUTH_SERVICES) : LOGIN_TEMPLATES
  module Election (W : WEB_ELECTION_RO) : ELECTION_TEMPLATES

end

module type NAME = sig
  val name : string
  val path : string list
  val kind : [ `Site | `Election of string ]
end

module type AUTH_SERVICE =
  functor (N : NAME) ->
  functor (T : LOGIN_TEMPLATES) ->
  AUTH_HANDLERS

module type AUTH_SYSTEM = sig
  type config

  val name : string

  val parse_config :
    instance:string ->
    attributes:(string * string) list ->
    config

  val make : config -> (module AUTH_SERVICE)
end
