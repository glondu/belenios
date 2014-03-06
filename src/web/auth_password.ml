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
open Auth_common

module Register (C : AUTH_CONFIG) (S : ALL_SERVICES) (T : TEMPLATES) = struct

  module L : CONT_SERVICE = struct
    let cont = S.get
  end

  let user_admin = false
  let user_type = "password"
  let user_logout = (module L : CONT_SERVICE)

  let db = match C.password_db with
    | None -> assert false
    | Some db -> db

  module A = struct

    let service = Eliom_service.service
      ~path:["login-password"]
      ~get_params:Eliom_parameter.unit
      ()

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

  end

  let () = register_auth_system "password" (module A : AUTH_SYSTEM)

end
