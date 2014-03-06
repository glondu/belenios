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

let create_string_login ~fallback ~post_params =
  Eliom_service.post_coservice
    ~csrf_safe:true
    ~csrf_scope:Eliom_common.default_session_scope
    ~fallback ~post_params ()

(* TODO: make the authentication system more flexible *)

module Make (X : EMPTY) = struct

  let login = Eliom_service.service
    ~path:["login"]
    ~get_params:Eliom_parameter.(opt (string "service"))
    ()

  let logout = Eliom_service.service
    ~path:["logout"]
    ~get_params:Eliom_parameter.unit
    ()

end

let next_lf str i =
  try Some (String.index_from str i '\n')
  with Not_found -> None

let auth_system_map = ref []

let register_auth_system name service =
  auth_system_map := (name, service) :: !auth_system_map

let auth_systems = lazy (List.map fst !auth_system_map)

let get_auth_systems () = Lazy.force auth_systems

let get_default_auth_system () =
  match !auth_system_map with
  | [] -> fail_http 404
  | (name, _) :: _ -> Lwt.return name

module Register (C : AUTH_CONFIG) (S : ALL_SERVICES) (T : TEMPLATES) = struct

  let () = Eliom_registration.Redirection.register ~service:S.login
    (fun service () ->
      lwt x = match service with
        | None -> get_default_auth_system ()
        | Some x -> Lwt.return x
      in
      try
        let auth_system = List.assoc x !auth_system_map in
        let module A = (val auth_system : AUTH_SYSTEM) in
        Lwt.return A.service
      with Not_found -> fail_http 404
    )

  let () = Eliom_registration.Redirection.register ~service:S.logout
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
        | _ -> S.get ()
    )

  module DefaultLogout : CONT_SERVICE = struct
    let cont = S.get
  end

  let () = if C.enable_dummy then (
    let user_admin = false in
    let user_type = "dummy" in
    let user_logout = (module DefaultLogout : CONT_SERVICE) in
    let service = Eliom_service.service
      ~path:["login-dummy"]
      ~get_params:Eliom_parameter.unit
      ()
    in
    let () = Eliom_registration.Html5.register ~service
      (fun () () ->
        let post_params = Eliom_parameter.(string "username") in
        let service = Eliom_service.post_coservice
          ~csrf_safe:true
          ~csrf_scope:Eliom_common.default_session_scope
          ~fallback:service
          ~post_params ()
        in
        let () = Eliom_registration.Redirection.register ~service
          ~scope:Eliom_common.default_session_scope
          (fun () user_name ->
            let user_user = {user_type; user_name} in
            let logged_user = {user_admin; user_user; user_logout} in
            Eliom_reference.set user (Some logged_user) >>
            Web_common.security_log (fun () ->
              user_name ^ " successfully logged in using dummy"
            ) >> S.get ())
        in T.string_login ~service ~kind:`Dummy
      )
    in
    let module A = struct let service = service end in
    let auth_system = (module A : AUTH_SYSTEM) in
    register_auth_system "dummy" auth_system
  )

  let () = match C.password_db with
    | None -> ()
    | Some db ->
      let user_admin = false in
      let user_type = "password" in
      let user_logout = (module DefaultLogout : CONT_SERVICE) in
      let service = Eliom_service.service
        ~path:["login-password"]
        ~get_params:Eliom_parameter.unit
        ()
      in
      let () = Eliom_registration.Html5.register ~service
        (fun () () ->
          let post_params = Eliom_parameter.(
            string "username" ** string "password"
          ) in
          let service = Eliom_service.post_coservice
            ~csrf_safe:true
            ~csrf_scope:Eliom_common.default_session_scope
            ~fallback:service
            ~post_params ()
          in
          let () = Eliom_registration.Redirection.register ~service
            ~scope:Eliom_common.default_session_scope
            (fun () (user_name, password) ->
              if (
                try
                  let salt, hashed = SMap.find user_name db in
                  sha256_hex (salt ^ password) = hashed
                with Not_found -> false
              ) then (
                let user_user = {user_type; user_name} in
                Eliom_reference.set user (Some {user_admin; user_user; user_logout}) >>
                security_log (fun () ->
                  user_name ^ " successfully logged in using password"
                ) >> S.get ()
              ) else forbidden ())
          in T.password_login ~service
        )
      in
      let module A = struct let service = service end in
      let auth_system = (module A : AUTH_SYSTEM) in
      register_auth_system "password" auth_system

  let () = if C.enable_cas then (
    let cas_login = Eliom_service.external_service
      ~prefix:C.cas_server
      ~path:["login"]
      ~get_params:Eliom_parameter.(string "service")
      ()
    in
    let cas_logout = Eliom_service.external_service
      ~prefix:C.cas_server
      ~path:["logout"]
      ~get_params:Eliom_parameter.(string "service")
      ()
    in
    let cas_validate = Eliom_service.external_service
      ~prefix:C.cas_server
      ~path:["validate"]
      ~get_params:Eliom_parameter.(string "service" ** string "ticket")
      ()
    in
    let login_cas = Eliom_service.service
      ~path:["login-cas"]
      ~get_params:Eliom_parameter.(opt (string "ticket"))
      ()
    in
    let () = Eliom_registration.Redirection.register ~service:login_cas
      (fun ticket () ->
        match ticket with
        | Some x ->
          let me =
            let service = Eliom_service.preapply login_cas None in
            let uri = Eliom_uri.make_string_uri ~absolute:true ~service () in
            C.rewrite_prefix uri
          in
          let validation =
            let service = Eliom_service.preapply cas_validate (me, x) in
            Eliom_uri.make_string_uri ~absolute:true ~service ()
          in
          lwt reply = Ocsigen_http_client.get_url validation in
          (match reply.Ocsigen_http_frame.frame_content with
            | Some stream ->
              lwt info = Ocsigen_stream.(string_of_stream 1000 (get stream)) in
              Ocsigen_stream.finalize stream `Success >>
              (match next_lf info 0 with
                | Some i ->
                  (match String.sub info 0 i with
                    | "yes" ->
                      (match next_lf info (i+1) with
                        | Some j ->
                          let user_name = String.sub info (i+1) (j-i-1) in
                          let user_type = "cas" in
                          let user_user = {user_type; user_name} in
                          let module L : CONT_SERVICE = struct
                            let cont () =
                              lwt service = S.get () in
                              let uri = Eliom_uri.make_string_uri ~absolute:true ~service () in
                              let uri = C.rewrite_prefix uri in
                              security_log (fun () ->
                                string_of_user user_user ^ " logged out, redirecting to CAS"
                              ) >>
                              Lwt.return (Eliom_service.preapply cas_logout uri)
                          end in
                          let user_logout = (module L : CONT_SERVICE) in
                          let user_admin = false in
                          security_log (fun () ->
                            user_name ^ " successfully logged in using CAS"
                          ) >>
                          Eliom_reference.set user
                            (Some {user_admin; user_user; user_logout}) >>
                          S.get ()
                        | None -> fail_http 502
                      )
                    | "no" -> fail_http 401
                    | _ -> fail_http 502
                  )
                | None -> fail_http 502
              )
            | None -> fail_http 502
          )
        | None ->
          let service = Eliom_service.preapply login_cas None in
          let uri = Eliom_uri.make_string_uri ~absolute:true ~service () in
          let uri = C.rewrite_prefix uri in
          Lwt.return (Eliom_service.preapply cas_login uri)
      )
    in
    let service = Eliom_service.preapply login_cas None in
    let module A = struct let service = service end in
    let auth_system = (module A : AUTH_SYSTEM) in
    register_auth_system "CAS" auth_system
  )

  let login_admin = Eliom_service.service
    ~path:["login-admin"]
    ~get_params:Eliom_parameter.unit
    ()

  let () = Eliom_registration.Html5.register
    ~service:login_admin
    (fun () () ->
      let service = create_string_login
        ~fallback:login_admin
        ~post_params:Eliom_parameter.(string "password")
      in
      let () = Eliom_registration.Redirection.register
        ~service
        ~scope:Eliom_common.default_session_scope
        (fun () user_name ->
          if sha256_hex user_name = C.admin_hash then (
            let user_type = "password" in
            let user_logout = (module DefaultLogout : CONT_SERVICE) in
            let user_user = {user_type; user_name} in
            let user_admin = true in
            Eliom_reference.set user (Some {user_admin; user_user; user_logout}) >>
            security_log (fun () ->
              "admin successfully logged in"
            ) >>
            S.get ()
          ) else forbidden ()
        )
      in
      T.string_login ~service ~kind:`Admin
    )

end
