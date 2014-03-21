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

type config = { db : string }

let name = "password"

let parse_config ~instance ~attributes =
  match attributes with
  | ["db", db] -> {db}
  | _ ->
    Printf.ksprintf failwith
      "invalid configuration for instance %s of auth/%s"
      instance name

module type CONFIG = sig
  val db : string
end

module Make (C : CONFIG) (N : NAME) (S : CONT_SERVICE) (T : TEMPLATES) : AUTH_INSTANCE = struct

  let service = Eliom_service.service
    ~path:N.path
    ~get_params:Eliom_parameter.unit
    ()

  let db =
    List.fold_left (fun accu line ->
      match line with
      | username :: salt :: password :: _ ->
        SMap.add username (salt, password) accu
      | _ -> failwith ("error while parsing db file for " ^ N.name)
    ) SMap.empty (Csv.load C.db)

  let user_logout = (module S : CONT_SERVICE)

  let on_success_ref = Eliom_reference.eref
    ~scope:Eliom_common.default_session_scope
    (fun ~user_name ~user_logout -> Lwt.return ())

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
            lwt on_success = Eliom_reference.get on_success_ref in
            on_success ~user_name ~user_logout >>
            S.cont ()
          ) else forbidden ())
      in T.Auth.login_password ~service
    )

  let handler ~on_success () =
    Eliom_reference.set on_success_ref on_success >>
    Eliom_registration.Redirection.send service

end

let make {db} =
  let module C = struct let db = db end in
  (module Make (C) : AUTH_SERVICE)

type c = config

module A : AUTH_SYSTEM = struct
  type config = c
  let name = name
  let parse_config = parse_config
  let make = make
end

let () = Auth_common.register_auth_system (module A : AUTH_SYSTEM)
