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

module type EMPTY = sig end

module type MAIN_SERVICES = sig

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

  val election_index :
    (Serializable_t.uuid, unit,
     [> `Attached of
          ([> `Internal of [> `Service ] ], [> `Get ])
          Eliom_service.a_s ],
     [ `WithoutSuffix ],
     [ `One of Serializable_t.uuid ] Eliom_parameter.param_name,
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

  val get :
    unit ->
    (unit, unit,
     [> `Attached of
          ([> `Internal of [> `Service ] ], [> `Get ])
          Eliom_service.a_s ],
     [ `WithoutSuffix ], unit, unit, Eliom_service.registrable, 'a)
    Eliom_service.service Lwt.t

end


module type AUTH_CONFIG = sig
  val cas_server : string
  val enable_cas : bool
  val enable_dummy : bool
  val admin_hash : string
  val rewrite_prefix : string -> string
  open Util
  val password_db : (SMap.key * SMap.key) SMap.t option
end


module type AUTH_SERVICES = sig

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


module type TEMPLATES = sig

  val string_login :
    kind:[< `Admin | `Dummy ] ->
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

end


module type ALL_SERVICES = sig
  include MAIN_SERVICES
  include AUTH_SERVICES
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

module type AUTH_SYSTEM = sig
  val service :
    (unit, unit,
     [> `Attached of
          ([> `Internal of [> `Service ] ], [> `Get ])
          Eliom_service.a_s ],
     [ `WithoutSuffix ], unit, unit,
     Eliom_service.registrable, 'a)
    Eliom_service.service
end
