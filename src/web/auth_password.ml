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

module type CONFIG = sig
  val db : string
end

module Make (C : CONFIG) (N : NAME) (S : CONT_SERVICE) (T : TEMPLATES) : AUTH_INSTANCE = struct

  let user_admin = false
  let user_type = N.name

  let service = Eliom_service.service
    ~path:["auth"; N.name]
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
            S.cont ()
          ) else forbidden ())
      in T.password_login ~service
    )

  let handler () = Eliom_registration.Redirection.send service

end

type instance = {
  mutable name : string option;
  mutable db : string option;
}

let init () =
  let instances = ref [] in
  let current_instance = ref None in
  let push_current loc =
    match !current_instance with
    | None -> ()
    | Some {name = Some name; db = Some db} ->
      let module C : CONFIG = struct
        let db = db
      end in
      instances := (name, (module C : CONFIG)) :: !instances;
      current_instance := None
    | _ -> failwith ("unexpected case in auth-password/" ^ loc)
  in
  let spec =
    let open Ocsigen_extensions.Configuration in
    [
      let init () =
        push_current "init";
        current_instance := Some {name = None; db = None}
      and attributes = [
        attribute ~name:"name" ~obligatory:true (fun s ->
          match !current_instance with
          | Some ({name = None; _} as i) -> i.name <- Some s
          | _ -> failwith "unexpected case in auth-password/name"
        );
        attribute ~name:"db" ~obligatory:true (fun s ->
          match !current_instance with
          | Some ({db = None; _} as i) -> i.db <- Some s
          | _ -> failwith "unexpected case in auth-password/db"
        );
      ] in element ~name:"auth-password" ~init ~attributes ();
    ]
  and exec ~instantiate =
    push_current "exec";
    List.iter (fun (name, config) ->
      let module X = Make ((val config : CONFIG)) in
      instantiate name (module X : AUTH_SERVICE)
    ) !instances
  in Auth_common.register_auth_system ~spec ~exec
