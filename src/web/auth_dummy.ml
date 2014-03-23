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
open Web_signatures
open Web_common

type config = unit

let name = "dummy"

let parse_config ~instance ~attributes =
  match attributes with
  | [] -> ()
  | _ ->
    Printf.ksprintf failwith
      "invalid configuration for instance %s of auth/%s"
      instance name

module Make (N : NAME) (T : TEMPLATES) : AUTH_HANDLERS = struct

  let service = Eliom_service.service
    ~path:N.path
    ~get_params:Eliom_parameter.unit
    ()

  let login_cont = Eliom_reference.eref
    ~scope:Eliom_common.default_session_scope
    None

  let () = Eliom_registration.Html5.register ~service
    (fun () () ->
      let post_params = Eliom_parameter.(string "username") in
      let service = Eliom_service.post_coservice
        ~csrf_safe:true
        ~csrf_scope:Eliom_common.default_session_scope
        ~fallback:service
        ~post_params ()
      in
      let () = Eliom_registration.Any.register ~service
        ~scope:Eliom_common.default_session_scope
        (fun () user_name ->
          match_lwt Eliom_reference.get login_cont with
          | Some cont ->
            Eliom_reference.unset login_cont >>
            cont user_name ()
          | None -> fail_http 400
        )
      in T.login_dummy ~service ()
    )

  let login cont () =
    Eliom_reference.set login_cont (Some cont) >>
    Eliom_registration.Redirection.send service

  let logout cont () = cont () ()

end

let make () = (module Make : AUTH_SERVICE)

module A : AUTH_SYSTEM = struct
  type config = unit
  let name = name
  let parse_config = parse_config
  let make = make
end

let () = Auth_common.register_auth_system (module A : AUTH_SYSTEM)
