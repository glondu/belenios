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
open Auth_common

module Make (N : NAME) : AUTH_INSTANCE = struct

  let user_admin = false
  let user_type = N.name

  let service = Eliom_service.service
    ~path:["auth"; N.name]
    ~get_params:Eliom_parameter.unit
    ()

  module Register (S : CONT_SERVICE) (T : TEMPLATES) = struct

    let user_logout = (module S : CONT_SERVICE)

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
            S.cont ())
        in T.string_login ~service ~kind:`Dummy
      )

  end

end

let init () =
  let instances = ref [] in
  let spec =
    let open Ocsigen_extensions.Configuration in
    [
      let attributes = [
        attribute ~name:"name" ~obligatory:true (fun s ->
          instances := s :: !instances
        );
      ] in element ~name:"auth-dummy" ~attributes ();
    ]
  and exec ~instantiate =
    List.iter (fun name ->
      instantiate name (module Make : AUTH_SERVICE)
    ) !instances
  in Auth_common.register_auth_system ~spec ~exec
