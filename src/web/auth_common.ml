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

open Lwt
open Util
open Serializable_t
open Signatures
open Web_serializable_t
open Web_signatures
open Web_common

let string_of_user {user_domain; user_name} =
  user_domain ^ ":" ^ user_name

let auth_systems = Hashtbl.create 10

let register_auth_system auth_system =
  let module X = (val auth_system : AUTH_SYSTEM) in
  if Hashtbl.mem auth_systems X.name then (
    Printf.ksprintf failwith
      "multiple authentication systems with name %s"
      X.name
  ) else (
    Hashtbl.add auth_systems X.name auth_system
  )

type auth_instance = {
  auth_system : string;
  auth_instance : string;
  auth_config : (string * string) list;
}

module type CONFIG = sig
  include NAME
  val instances : auth_instance list
end

module Make (N : CONFIG) = struct

  let auth_instances = Hashtbl.create 10
  let auth_instance_names = ref []

  let user = Eliom_reference.eref
    ~scope:Eliom_common.default_session_scope
    None

  let on_success user_domain ~user_name ~user_logout =
    let user_user = {user_domain; user_name} in
    let logged_user = {user_user; user_logout} in
    security_log (fun () ->
      Printf.sprintf "%s successfully logged on %s using %s"
        user_name N.name user_domain
    ) >>
    Eliom_reference.set user (Some logged_user)

  module Services : AUTH_SERVICES = struct

    let get_auth_systems () = !auth_instance_names

    let get_logged_user () = Eliom_reference.get user

    let login = Eliom_service.service
      ~path:(N.path @ ["login"])
      ~get_params:Eliom_parameter.(opt (string "service"))
      ()

    let logout = Eliom_service.service
      ~path:(N.path @ ["logout"])
      ~get_params:Eliom_parameter.unit
      ()

  end

  module Register (C : CONT_SERVICE) (T : TEMPLATES) : EMPTY = struct

    let () = List.iter (fun auth_instance ->
      let {
        auth_system = name;
        auth_instance = instance;
        auth_config = attributes;
      } = auth_instance in
      if Hashtbl.mem auth_instances instance then (
        Printf.ksprintf failwith
          "multiple instances with name %s"
          instance
      ) else (
        let auth_system = Hashtbl.find auth_systems name in
        let module X = (val auth_system : AUTH_SYSTEM) in
        let config = X.parse_config ~instance ~attributes in
        let auth = X.make config in
        let module N = struct
          let name = instance
          let path = N.path @ ["auth"; instance]
        end in
        let module A = (val auth : AUTH_SERVICE) (N) (C) (T) in
        let i = (module A : AUTH_INSTANCE) in
        Hashtbl.add auth_instances instance i;
        auth_instance_names := instance :: !auth_instance_names
      )
    ) N.instances

    let () = Eliom_registration.Any.register
      ~service:Services.login
      (fun service () ->
        let use name =
          try
            let i = Hashtbl.find auth_instances name in
            let module A = (val i : AUTH_INSTANCE) in
            A.handler ~on_success:(on_success name) ()
          with Not_found -> fail_http 404
        in
        match service with
        | Some name -> use name
        | None ->
          match !auth_instance_names with
          | [name] -> use name
          | _ -> T.generic_login () >>= Eliom_registration.Html5.send
      )

    let () = Eliom_registration.Redirection.register
      ~service:Services.logout
      (fun () () ->
        lwt u = Eliom_reference.get user in
        (* should ballot be unset here or not? *)
        Eliom_reference.unset user >>
        match u with
          | Some u ->
            let module L = (val u.user_logout) in
            security_log (fun () ->
              string_of_user u.user_user ^ " logged out"
            ) >> L.cont ()
          | _ -> C.cont ()
      )

  end

end
