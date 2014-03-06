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

open Web_signatures
open Web_common
open Auth_common

let next_lf str i =
  try Some (String.index_from str i '\n')
  with Not_found -> None

module Register (C : AUTH_CONFIG) (S : ALL_SERVICES) = struct

  let user_admin = false
  let user_type = "cas"

  module A : AUTH_SYSTEM = struct

    let cas_login = Eliom_service.external_service
      ~prefix:C.cas_server
      ~path:["login"]
      ~get_params:Eliom_parameter.(string "service")
      ()

    let cas_logout = Eliom_service.external_service
      ~prefix:C.cas_server
      ~path:["logout"]
      ~get_params:Eliom_parameter.(string "service")
      ()

    let cas_validate = Eliom_service.external_service
      ~prefix:C.cas_server
      ~path:["validate"]
      ~get_params:Eliom_parameter.(string "service" ** string "ticket")
      ()

    let login_cas = Eliom_service.service
      ~path:["login-cas"]
      ~get_params:Eliom_parameter.(opt (string "ticket"))
      ()

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
                          let user_user = {user_type; user_name} in
                          let module L : CONT_SERVICE = struct
                            let cont () =
                              lwt service = S.get () in
                              let uri = Eliom_uri.make_string_uri ~absolute:true ~service () in
                              let uri = C.rewrite_prefix uri in
                              security_log (fun () ->
                                string_of_user user_user ^ " logged out, redirecting to CAS"
                              ) >> Lwt.return (Eliom_service.preapply cas_logout uri)
                          end in
                          let user_logout = (module L : CONT_SERVICE) in
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

    let service = Eliom_service.preapply login_cas None

  end

  let () = register_auth_system "CAS" (module A : AUTH_SYSTEM)
end
