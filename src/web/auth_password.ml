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
open Platform
open Common
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

let load_db name file =
  (* FIXME: not cooperative *)
  List.fold_left (fun accu line ->
    match line with
    | username :: salt :: password :: _ ->
      SMap.add username (salt, password) accu
    | _ -> failwith ("error while parsing db file for " ^ name)
  ) SMap.empty (Csv.load file)

let ( / ) = Filename.concat

module Make (C : CONFIG) (N : NAME) (T : LOGIN_TEMPLATES) : AUTH_HANDLERS = struct

  let scope = Eliom_common.default_session_scope

  let service = Eliom_service.service
    ~path:N.path
    ~get_params:Eliom_parameter.unit
    ()

  let db =
    ref @@ match N.kind with
    | `Site -> `Production (load_db N.name C.db)
    | `Election dir ->
      (* hash the user-input name to avoid all kinds of injection *)
      let fname = dir / sha256_hex C.db in
      try
        `Production (load_db N.name fname)
      with _ ->
        (* Maybe we should filter the kind of error...? *)
        `Bootstrap fname

  let login_cont = Eliom_reference.eref ~scope None

  let production_service_handler db =
    let post_params = Eliom_parameter.(
      string "username" ** string "password"
    ) in
    let service = Eliom_service.post_coservice
      ~csrf_safe:true
      ~csrf_scope:scope
      ~fallback:service
      ~post_params ()
    in
    let () = Eliom_registration.Any.register ~service ~scope
      (fun () (user_name, password) ->
        if (
          try
            let salt, hashed = SMap.find user_name db in
            sha256_hex (salt ^ password) = hashed
          with Not_found -> false
        ) then (
          match_lwt Eliom_reference.get login_cont with
          | Some cont ->
            Eliom_reference.unset login_cont >>
            cont user_name ()
          | None -> fail_http 400
        ) else forbidden ())
    in
    T.password ~service ()

  let bootstrap_service_handler () =
    let post_params = Eliom_parameter.file "password_db" in
    let upload_service = Eliom_service.post_coservice
      ~csrf_safe:true
      ~csrf_scope:scope
      ~fallback:service
      ~post_params ()
    in
    let () = Eliom_registration.Any.register ~service:upload_service ~scope
      (fun () password_db ->
        match !db with
        | `Bootstrap db_fname ->
          let fname = password_db.Ocsigen_extensions.tmp_filename in
          let the_db = load_db N.name fname in
          (* loading was successful, we copy the file for future reference *)
          lwt () =
            Lwt_io.(with_file Output db_fname (fun oc ->
              with_file Input fname (fun ic ->
                read_chars ic |> write_chars oc
              )
            ))
          in
          db := `Production the_db;
          Eliom_registration.Redirection.send service
        | `Production _ -> forbidden ()
      )
    in
    T.upload_password_db ~service:upload_service ()

  let () = Eliom_registration.Html5.register ~service
    (fun () () ->
      match !db with
      | `Bootstrap _ -> bootstrap_service_handler ()
      | `Production db -> production_service_handler db
    )

  let login cont () =
    Eliom_reference.set login_cont (Some cont) >>
    Eliom_registration.Redirection.send service

  let logout cont () = cont () ()

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

let () = Web_auth.register_auth_system (module A : AUTH_SYSTEM)
