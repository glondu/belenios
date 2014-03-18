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

module type EMPTY = sig end

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

module type SITE_SERVICES = sig

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

  val election_update_credential :
    (Serializable_t.uuid, string * string,
     [> `Attached of
          ([> `Internal of [ `Coservice | `Service ] ], [> `Post ])
          Eliom_service.a_s ],
     [ `WithoutSuffix ],
     [ `One of Serializable_t.uuid ] Eliom_parameter.param_name,
     [ `One of string ] Eliom_parameter.param_name *
     [ `One of string ] Eliom_parameter.param_name,
     [< Eliom_service.registrable > `Registrable ], 'a)
    Eliom_service.service

  val election_update_credential_form :
    (Uuidm.t, unit,
     [> `Attached of
          ([> `Internal of [> `Service ] ], [> `Get ])
          Eliom_service.a_s ],
     [ `WithoutSuffix ],
     [ `One of Uuidm.t ] Eliom_parameter.param_name, unit,
     [< Eliom_service.registrable > `Registrable ], 'a)
    Eliom_service.service

  val saved_service : (module SAVED_SERVICE) Eliom_reference.eref

end

module type ELECTION_SERVICES = sig

  val election_dir :
    (Serializable_t.uuid * Services.election_file, unit,
     [> `Attached of
          ([> `Internal of [> `Service ] ], [> `Get ])
          Eliom_service.a_s ],
     [ `WithSuffix ],
     [ `One of Serializable_t.uuid ] Eliom_parameter.param_name *
     [ `One of Services.election_file ] Eliom_parameter.param_name,
     unit, [< Eliom_service.registrable > `Registrable ], 'a)
    Eliom_service.service

  val election_file :
    'a Serializable_t.params ->
    Services.election_file ->
    (unit, unit,
     [> `Attached of
          ([> `Internal of [> `Service ] ], [> `Get ])
          Eliom_service.a_s ],
     [ `WithoutSuffix ], unit, unit,
     [< Eliom_service.registrable > `Unregistrable ], 'b)
    Eliom_service.service

  val make_booth :
    Serializable_t.uuid ->
    (unit, unit,
     [> `Attached of
          ([> `Internal of [> `Service ] ], [> `Get ])
          Eliom_service.a_s ],
     [ `WithoutSuffix ], unit, unit,
     [< Eliom_service.registrable > `Unregistrable ], 'a)
    Eliom_service.service

end

module type VOTING_SERVICES = sig

  val election_vote :
    (Serializable_t.uuid, unit,
     [> `Attached of
          ([> `Internal of [> `Service ] ], [> `Get ])
          Eliom_service.a_s ],
     [ `WithoutSuffix ],
     [ `One of Serializable_t.uuid ] Eliom_parameter.param_name,
     unit, [< Eliom_service.registrable > `Registrable ], 'a)
    Eliom_service.service

  val election_cast :
    (Serializable_t.uuid, unit,
     [> `Attached of
          ([> `Internal of [> `Service ] ], [> `Get ])
          Eliom_service.a_s ],
     [ `WithoutSuffix ],
     [ `One of Serializable_t.uuid ] Eliom_parameter.param_name,
     unit, [< Eliom_service.registrable > `Registrable ], 'a)
    Eliom_service.service

  val election_cast_post :
    (Serializable_t.uuid, string option * Eliom_lib.file_info option,
     [> `Attached of
          ([> `Internal of [ `Coservice | `Service ] ], [> `Post ])
          Eliom_service.a_s ],
     [ `WithoutSuffix ],
     [ `One of Serializable_t.uuid ] Eliom_parameter.param_name,
     [ `One of string ] Eliom_parameter.param_name *
     [ `One of Eliom_lib.file_info ] Eliom_parameter.param_name,
     [< Eliom_service.registrable > `Registrable ], 'a)
    Eliom_service.service

  val create_confirm :
    unit ->
    (Uuidm.t, unit,
     [> `Attached of
          ([> `Internal of [> `Coservice ] ], [> `Post ])
          Eliom_service.a_s ],
     [ `WithoutSuffix ],
     [ `One of Uuidm.t ] Eliom_parameter.param_name, unit,
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
  user_admin : bool;
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

module type WEB_ELECTION = sig
  module G : GROUP
  module E : ELECTION with type elt = G.t

  val election : G.t election
  val metadata : metadata

  module B : WEB_BALLOT_BOX

  val featured_p : bool
  val params_fname : string
  val public_keys_fname : string
end

module type TEMPLATES = sig

  val index :
    featured:(module WEB_ELECTION) list ->
    [> `Html ] Eliom_content.Html5.F.elt Lwt.t

  val election_update_credential :
    election:(module WEB_ELECTION) ->
    [> `Html ] Eliom_content.Html5.F.elt Lwt.t

  val election_view :
    election:(module WEB_ELECTION) ->
    user:logged_user option ->
    [> `Html ] Eliom_content.Html5.F.elt Lwt.t

  val do_cast_ballot :
    election:(module WEB_ELECTION) ->
    result:[< `Error of Web_common.error | `Valid of string ] ->
    [> `Html ] Eliom_content.Html5.F.elt Lwt.t

  val ballot_received :
    election:(module WEB_ELECTION) ->
    confirm:(unit ->
             (Serializable_t.uuid, 'b,
              [< Eliom_service.post_service_kind ],
              [< Eliom_service.suff ], 'c, unit,
              [< Eliom_service.registrable ], 'd)
             Eliom_service.service) ->
    user:logged_user option ->
    can_vote:bool -> [> `Html ] Eliom_content.Html5.F.elt Lwt.t

  val election_cast_raw :
    election:(module WEB_ELECTION) ->
    [> `Html ] Eliom_content.Html5.F.elt Lwt.t

  val dummy_login :
    service:(unit, 'a, [< Eliom_service.post_service_kind ],
             [< Eliom_service.suff ], 'b,
             [< string Eliom_parameter.setoneradio ]
             Eliom_parameter.param_name,
             [< Eliom_service.registrable ], 'c)
            Eliom_service.service ->
    [> `Html ] Eliom_content.Html5.F.elt Lwt.t

  val password_login :
    service:(unit, 'a, [< Eliom_service.post_service_kind ],
             [< Eliom_service.suff ], 'b,
             [< string Eliom_parameter.setoneradio ]
             Eliom_parameter.param_name *
             [< string Eliom_parameter.setoneradio ]
             Eliom_parameter.param_name,
             [< Eliom_service.registrable ], 'c)
            Eliom_service.service ->
    [> `Html ] Eliom_content.Html5.F.elt Lwt.t

  val generic_login :
    unit -> [> `Html ] Eliom_content.Html5.F.elt Lwt.t

end


module type ALL_SERVICES = sig
  include SITE_SERVICES
  include ELECTION_SERVICES
  include VOTING_SERVICES
  include AUTH_SERVICES
  include CONT_SERVICE
end

module type NAME = sig
  val name : string
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
