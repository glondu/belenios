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
open Web_serializable_t
open Signatures

module type SAVED_SERVICE = sig
  val s :
    (unit, unit,
     [> `Attached of
          ([> `Internal of [> `Service ] ], [> `Get ])
          Eliom_service.a_s ],
     [ `WithoutSuffix ], unit, unit,
     [> Eliom_service.registrable ], 'a)
    Eliom_service.service
end

type election_config = {
  raw_election : string;
  metadata : metadata;
  featured : bool;
  params_fname : string;
  public_keys_fname : string;
}

module type CORE_SERVICES = sig

  val home :
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

  val saved_service : (module SAVED_SERVICE) Eliom_reference.eref

end

module type ELECTION_SERVICES = sig

  val home :
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


module type AUTH_CONFIG = sig
  val cas_server : string
  val enable_cas : bool
  val enable_dummy : bool
  val rewrite_prefix : string -> string
  open Util
  val password_db : (SMap.key * SMap.key) SMap.t option
end

module type CONT_SERVICE = sig
  val cont :
    unit ->
    (unit, unit,
     [> `Attached of
          ([> `External | `Internal of [> `Service ] ], [> `Get ])
          Eliom_service.a_s ],
     [ `WithoutSuffix ], unit, unit, Eliom_service.registrable, 'a)
    Eliom_service.service Lwt.t
end

type logged_user = {
  user_user : user;
  user_logout : (module CONT_SERVICE);
}

module type AUTH_SERVICES = sig

  val get_auth_systems : unit -> string list
  val get_logged_user : unit -> logged_user option Lwt.t

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

module type WEB_BALLOT_BOX = sig
  module Ballots : MONADIC_MAP_RO
    with type 'a m = 'a Lwt.t
    and type elt = string
    and type key = string
  module Records : MONADIC_MAP_RO
    with type 'a m = 'a Lwt.t
    and type elt = Serializable_builtin_t.datetime * string
    and type key = string

  val cast : string -> string * datetime -> string Lwt.t
  val inject_creds : Util.SSet.t -> unit Lwt.t
  val extract_creds : unit -> Util.SSet.t Lwt.t
  val update_cred : old:string -> new_:string -> unit Lwt.t
end

module type ELECTION_TEMPLATES = sig

  val home :
    user:logged_user option ->
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
    user:logged_user option ->
    can_vote:bool ->
    unit -> [> `Html ] Eliom_content.Html5.F.elt Lwt.t

  val cast_confirmed :
    result:[< `Error of Web_common.error | `Valid of string ] ->
    unit -> [> `Html ] Eliom_content.Html5.F.elt Lwt.t

end

module type WEB_ELECTION = sig
  module G : GROUP
  module E : ELECTION with type elt = G.t

  val election : G.t election
  val metadata : metadata
  val featured : bool
  val params_fname : string
  val public_keys_fname : string

  module B : WEB_BALLOT_BOX
  module S : ELECTION_SERVICES
end

module type SITE_SERVICES = sig
  include CORE_SERVICES
  include CONT_SERVICE
  include AUTH_SERVICES
  val register_election : election_config -> (module WEB_ELECTION) Lwt.t
end

module type TEMPLATES = sig

  val home :
    featured:(module WEB_ELECTION) list ->
    unit -> [> `Html ] Eliom_content.Html5.F.elt Lwt.t

  val login_dummy :
    service:(unit, 'a, [< Eliom_service.post_service_kind ],
             [< Eliom_service.suff ], 'b,
             [< string Eliom_parameter.setoneradio ]
             Eliom_parameter.param_name,
             [< Eliom_service.registrable ], 'c)
            Eliom_service.service ->
    unit -> [> `Html ] Eliom_content.Html5.F.elt Lwt.t

  val login_password :
    service:(unit, 'a, [< Eliom_service.post_service_kind ],
             [< Eliom_service.suff ], 'b,
             [< string Eliom_parameter.setoneradio ]
             Eliom_parameter.param_name *
             [< string Eliom_parameter.setoneradio ]
             Eliom_parameter.param_name,
             [< Eliom_service.registrable ], 'c)
            Eliom_service.service ->
    unit -> [> `Html ] Eliom_content.Html5.F.elt Lwt.t

  val login_choose :
    unit -> [> `Html ] Eliom_content.Html5.F.elt Lwt.t

  module Election (W : WEB_ELECTION) : ELECTION_TEMPLATES

end

module type NAME = sig
  val name : string
  val path : string list
end

type on_success_handler =
  user_name:string -> user_logout:(module CONT_SERVICE) -> unit Lwt.t

module type AUTH_INSTANCE = sig

  val handler :
    on_success:on_success_handler -> unit ->
    (Eliom_registration.browser_content,
     Eliom_registration.http_service)
    Eliom_registration.kind Lwt.t

end

module type AUTH_SERVICE =
  functor (N : NAME) ->
  functor (S : CONT_SERVICE) ->
  functor (T : TEMPLATES) ->
  AUTH_INSTANCE

module type AUTH_SYSTEM = sig
  type config

  val name : string

  val parse_config :
    instance:string ->
    attributes:(string * string) list ->
    config

  val make : config -> (module AUTH_SERVICE)
end
