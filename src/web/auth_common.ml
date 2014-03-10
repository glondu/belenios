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

open Util
open Web_signatures
open Web_common

type user = {
  user_type : string;
  user_name : string;
}

type logged_user = {
  user_admin : bool;
  user_user : user;
  user_logout : (module CONT_SERVICE);
}

let string_of_user {user_type; user_name} =
  user_type ^ ":" ^ user_name

let user = Eliom_reference.eref
  ~scope:Eliom_common.default_session_scope
  None

type instantiator = string -> (module AUTH_SERVICE) -> unit

let config_spec = ref []
let config_exec = ref []

let register_auth_system ~spec ~exec =
  config_spec := spec @ !config_spec;
  config_exec := exec :: !config_exec

let get_config_spec () = !config_spec

(* TODO: make the authentication system more flexible *)

module Make (X : EMPTY) = struct

  let instances = Hashtbl.create 10
  let auth_systems = ref []

  module Services : AUTH_SERVICES = struct

    let get_auth_systems () = !auth_systems

    let login = Eliom_service.service
      ~path:["login"]
      ~get_params:Eliom_parameter.(opt (string "service"))
      ()

    let logout = Eliom_service.service
      ~path:["logout"]
      ~get_params:Eliom_parameter.unit
      ()

  end

  module Register (C : CONT_SERVICE) (T : TEMPLATES) : EMPTY = struct

    let instantiate name auth =
      if Hashtbl.mem instances name then (
        failwith ("multiple instances with name " ^ name)
      ) else (
        let module N = struct let name = name end in
        let module A = (val auth : AUTH_SERVICE) (N) (C) (T) in
        let i = (module A : AUTH_INSTANCE) in
        Hashtbl.add instances name i;
        auth_systems := name :: !auth_systems
      )

    let () = List.iter (fun f -> f ~instantiate) !config_exec

    let default_auth_system = lazy (
      match !auth_systems with
      | [name] -> name
      | _ -> failwith "several (or no) instances of auth systems"
    )

    let () = Eliom_registration.Redirection.register
      ~service:Services.login
      (fun service () ->
        lwt x = match service with
          | None -> Lwt.return (Lazy.force default_auth_system)
          | Some x -> Lwt.return x
        in
        try
          let i = Hashtbl.find instances x in
          let module A = (val i : AUTH_INSTANCE) in
          Lwt.return A.service
        with Not_found -> fail_http 404
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
