(**************************************************************************)
(*                                BELENIOS                                *)
(*                                                                        *)
(*  Copyright © 2012-2016 Inria                                           *)
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
open Eliom_service
open Platform
open Web_serializable_t
open Web_common
open Web_state
open Web_services

let next_lf str i =
  try Some (String.index_from str i '\n')
  with Not_found -> None

let configure x =
  let auth_config =
    List.map (fun {auth_system; auth_instance; auth_config} ->
      auth_instance, (auth_system, List.map snd auth_config)
    ) x
  in
  Web_persist.set_auth_config "" auth_config |> Lwt_unix.run;
  List.iter (fun {auth_system; auth_config; _} ->
    match auth_system with
    | "password" ->
       let table = Ocsipersist.open_table "password_site" in
       (match auth_config with
       | [] -> ()
       | ["db", file] ->
          Ocsigen_messages.console (fun () ->
            Printf.sprintf "Loading passwords from file %s" file
          );
         let db = Csv.load file in
         List.iter (function
         | username :: salt :: password :: _ ->
            Ocsipersist.add table username (salt, password) |> Lwt_unix.run
         | _ -> failwith ("error while loading " ^ file)) db
       | _ -> failwith "error in passwords configuration")
    | _ -> ()
  ) x

let scope = Eliom_common.default_session_scope

let auth_env = Eliom_reference.eref ~scope None

let default_cont uuid () =
  match_lwt cont_pop () with
  | Some f -> f ()
  | None ->
     match uuid with
     | None ->
        Eliom_registration.Redirection.send Web_services.admin
     | Some u ->
        Eliom_registration.Redirection.send (preapply Web_services.election_home (u, ()))

let dummy_handler () name =
  match_lwt Eliom_reference.get auth_env with
  | None -> failwith "dummy handler was invoked without environment"
  | Some (uuid, service) ->
     let logout () =
       Eliom_reference.unset user >>
       default_cont uuid ()
     in
     Eliom_reference.set user (Some {uuid; service; name; logout}) >>
     Eliom_reference.unset auth_env >>
     default_cont uuid ()

let () = Eliom_registration.Any.register ~service:dummy_post dummy_handler

let password_handler () (name, password) =
  lwt uuid, service =
    match_lwt Eliom_reference.get auth_env with
    | None -> failwith "password handler was invoked without environment"
    | Some x -> return x
  in
  let table =
    "password_" ^
    match uuid with
    | None -> "site"
    | Some u ->
       let u = Uuidm.to_string u in
       underscorize u
  in
  let table = Ocsipersist.open_table table in
  lwt salt, hashed =
    try_lwt Ocsipersist.find table name
    with Not_found -> fail_http 401
  in
  if sha256_hex (salt ^ password) = hashed then
    let logout () =
      Eliom_reference.unset user >>
      default_cont uuid ()
    in
    Eliom_reference.set user (Some {uuid; service; name; logout}) >>
    Eliom_reference.unset auth_env >>
    default_cont uuid ()
  else
    fail_http 401

let () = Eliom_registration.Any.register ~service:password_post password_handler

let cas_server = Eliom_reference.eref ~scope None

let login_cas = Eliom_service.Http.service
  ~path:["auth"; "cas"]
  ~get_params:Eliom_parameter.(opt (string "ticket"))
  ()

let cas_self =
  (* lazy so rewrite_prefix is called after server initialization *)
  lazy (Eliom_uri.make_string_uri
          ~absolute:true
          ~service:(preapply login_cas None)
          () |> rewrite_prefix)

let cas_handler ticket () =
  lwt uuid, service =
    match_lwt Eliom_reference.get auth_env with
    | None -> failwith "cas handler was invoked without environment"
    | Some x -> return x
  in
  match ticket with
  | Some x ->
     lwt server =
       match_lwt Eliom_reference.get cas_server with
       | None -> failwith "cas handler was invoked without a server"
       | Some x -> return x
     in
     let validation =
       let cas_validate = Http.external_service
         ~prefix:server
         ~path:["validate"]
         ~get_params:Eliom_parameter.(string "service" ** string "ticket")
         ()
       in
       let service = preapply cas_validate (Lazy.force cas_self, x) in
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
                 let name = String.sub info (i+1) (j-i-1) in
                 let logout () =
                   Eliom_reference.unset user >>
                   let cas_logout = Http.external_service
                     ~prefix:server
                     ~path:["logout"]
                     ~get_params:Eliom_parameter.(string "service")
                     ()
                   in
                   let service = preapply cas_logout (Lazy.force cas_self) in
                   Eliom_registration.Redirection.send service
                 in
                 Eliom_reference.set user (Some {uuid; service; name; logout}) >>
                 default_cont uuid ()
              | None -> fail_http 502)
           | "no" -> fail_http 401
           | _ -> fail_http 502)
        | None -> fail_http 502)
     | None -> fail_http 502)
  | None ->
     Eliom_reference.unset cas_server >>
     Eliom_reference.unset auth_env >>
     default_cont uuid ()

let () = Eliom_registration.Any.register ~service:login_cas cas_handler

let cas_login_handler config () =
  match config with
  | [server] ->
     Eliom_reference.set cas_server (Some server) >>
     let cas_login = Http.external_service
       ~prefix:server
       ~path:["login"]
       ~get_params:Eliom_parameter.(string "service")
       ()
     in
     let service = preapply cas_login (Lazy.force cas_self) in
     Eliom_registration.Redirection.send service
  | _ -> failwith "cas_login_handler invoked with bad config"

let get_login_handler service uuid auth_system config =
  Eliom_reference.set auth_env (Some (uuid, service)) >>
  match auth_system with
  | "dummy" -> Web_templates.login_dummy () >>= Eliom_registration.Html5.send
  | "cas" -> cas_login_handler config ()
  | "password" -> Web_templates.login_password () >>= Eliom_registration.Html5.send
  | _ -> fail_http 404

let login_handler service uuid =
  let myself service =
    match uuid with
    | None -> preapply site_login service
    | Some u -> preapply election_login ((u, ()), service)
  in
  match_lwt Eliom_reference.get user with
  | Some _ ->
     cont_push (fun () -> Eliom_registration.Redirection.send (myself service)) >>
     Web_templates.already_logged_in () >>= Eliom_registration.Html5.send
  | None ->
     let uuid_or_empty = match uuid with
       | None -> ""
       | Some u -> Uuidm.to_string u
     in
     lwt c = Web_persist.get_auth_config uuid_or_empty in
     match service with
     | Some s ->
        lwt auth_system, config =
          try return @@ List.assoc s c
          with Not_found -> fail_http 404
        in
        get_login_handler s uuid auth_system config
     | None ->
        match c with
        | [s, _] -> Eliom_registration.Redirection.send (myself (Some s))
        | _ ->
           let builder =
             match uuid with
             | None -> fun s ->
               preapply Web_services.site_login (Some s)
             | Some u -> fun s ->
               preapply Web_services.election_login ((u, ()), Some s)
           in
           Web_templates.login_choose (List.map fst c) builder () >>=
           Eliom_registration.Html5.send

let logout_handler () =
  match_lwt Eliom_reference.get user with
  | Some u -> u.logout ()
  | None ->
     match_lwt cont_pop () with
     | Some f -> f ()
     | None -> Eliom_registration.Redirection.send Web_services.home

let () = Eliom_registration.Any.register ~service:site_login
  (fun service () -> login_handler service None)

let () = Eliom_registration.Any.register ~service:logout
  (fun () () -> logout_handler ())

let () = Eliom_registration.Any.register ~service:election_login
  (fun ((uuid, ()), service) () -> login_handler service (Some uuid))
