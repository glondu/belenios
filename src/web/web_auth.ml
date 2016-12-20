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
open Web_serializable_j
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
  match%lwt cont_pop () with
  | Some f -> f ()
  | None ->
     match uuid with
     | None ->
        Eliom_registration.Redirection.send Web_services.admin
     | Some u ->
        Eliom_registration.Redirection.send (preapply Web_services.election_home (u, ()))

(** Dummy authentication *)

let dummy_handler () name =
  match%lwt Eliom_reference.get auth_env with
  | None -> failwith "dummy handler was invoked without environment"
  | Some (uuid, service) ->
     Eliom_reference.set user (Some {uuid; service; name}) >>
     Eliom_reference.unset auth_env >>
     default_cont uuid ()

let () = Eliom_registration.Any.register ~service:dummy_post dummy_handler

(** Password authentication *)

let password_handler () (name, password) =
  let%lwt uuid, service =
    match%lwt Eliom_reference.get auth_env with
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
  let%lwt salt, hashed =
    try%lwt Ocsipersist.find table name
    with Not_found -> fail_http 401
  in
  if sha256_hex (salt ^ password) = hashed then
    Eliom_reference.set user (Some {uuid; service; name}) >>
    Eliom_reference.unset auth_env >>
    default_cont uuid ()
  else
    fail_http 401

let () = Eliom_registration.Any.register ~service:password_post password_handler

(** CAS authentication *)

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

let parse_cas_validation info =
  match next_lf info 0 with
  | Some i ->
     (match String.sub info 0 i with
     | "yes" -> `Yes
        (match next_lf info (i+1) with
        | Some j -> Some (String.sub info (i+1) (j-i-1))
        | None -> None)
     | "no" -> `No
     | _ -> `Error `Parsing)
  | None -> `Error `Parsing

let get_cas_validation server ticket =
  let url =
    let cas_validate = Http.external_service
      ~prefix:server
      ~path:["validate"]
      ~get_params:Eliom_parameter.(string "service" ** string "ticket")
      ()
    in
    let service = preapply cas_validate (Lazy.force cas_self, ticket) in
    Eliom_uri.make_string_uri ~absolute:true ~service ()
  in
  let%lwt reply = Ocsigen_http_client.get_url url in
  match reply.Ocsigen_http_frame.frame_content with
  | Some stream ->
     let%lwt info = Ocsigen_stream.(string_of_stream 1000 (get stream)) in
     Ocsigen_stream.finalize stream `Success >>
     return (parse_cas_validation info)
  | None -> return (`Error `Http)

let cas_handler ticket () =
  let%lwt uuid, service =
    match%lwt Eliom_reference.get auth_env with
    | None -> failwith "cas handler was invoked without environment"
    | Some x -> return x
  in
  match ticket with
  | Some x ->
     let%lwt server =
       match%lwt Eliom_reference.get cas_server with
       | None -> failwith "cas handler was invoked without a server"
       | Some x -> return x
     in
     (match%lwt get_cas_validation server x with
     | `Yes (Some name) ->
        Eliom_reference.set user (Some {uuid; service; name}) >>
        default_cont uuid ()
     | `No -> fail_http 401
     | `Yes None | `Error _ -> fail_http 502)
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

(** OpenID Connect (OIDC) authentication *)

let oidc_state = Eliom_reference.eref ~scope None

let login_oidc = Eliom_service.Http.service
  ~path:["auth"; "oidc"]
  ~get_params:Eliom_parameter.any
  ()

let oidc_self =
  lazy (Eliom_uri.make_string_uri
          ~absolute:true
          ~service:(preapply login_oidc [])
          () |> rewrite_prefix)

let oidc_get_userinfo ocfg info =
  let info = oidc_tokens_of_string info in
  let access_token = info.oidc_access_token in
  let url = ocfg.userinfo_endpoint in
  let headers = Http_headers.(
    add (name "Authorization") ("Bearer " ^ access_token) empty
  ) in
  let%lwt reply = Ocsigen_http_client.get_url ~headers url in
  match reply.Ocsigen_http_frame.frame_content with
  | Some stream ->
     let%lwt info = Ocsigen_stream.(string_of_stream 10000 (get stream)) in
     Ocsigen_stream.finalize stream `Success >>
     let x = oidc_userinfo_of_string info in
     return (Some (match x.oidc_email with Some x -> x | None -> x.oidc_sub))
  | None -> return None

let oidc_get_name ocfg client_id client_secret code =
  let content = [
    "code", code;
    "client_id", client_id;
    "client_secret", client_secret;
    "redirect_uri", Lazy.force oidc_self;
    "grant_type", "authorization_code";
  ] in
  let%lwt reply = Ocsigen_http_client.post_urlencoded_url ~content ocfg.token_endpoint in
  match reply.Ocsigen_http_frame.frame_content with
  | Some stream ->
    let%lwt info = Ocsigen_stream.(string_of_stream 10000 (get stream)) in
    Ocsigen_stream.finalize stream `Success >>
    oidc_get_userinfo ocfg info
  | None -> return None

let oidc_handler params () =
  let%lwt uuid, service =
    match%lwt Eliom_reference.get auth_env with
    | None -> failwith "oidc handler was invoked without environment"
    | Some x -> return x
  in
  let code = try Some (List.assoc "code" params) with Not_found -> None in
  let state = try Some (List.assoc "state" params) with Not_found -> None in
  match code, state with
  | Some code, Some state ->
    let%lwt ocfg, client_id, client_secret, st =
      match%lwt Eliom_reference.get oidc_state with
      | None -> failwith "oidc handler was invoked without a state"
      | Some x -> return x
    in
    Eliom_reference.unset oidc_state >>
    Eliom_reference.unset auth_env >>
    if state <> st then fail_http 401 else
    (match%lwt oidc_get_name ocfg client_id client_secret code with
    | Some name ->
       Eliom_reference.set user (Some {uuid; service; name}) >>
       default_cont uuid ()
    | None -> fail_http 401)
  | _, _ -> default_cont uuid ()

let () = Eliom_registration.Any.register ~service:login_oidc oidc_handler

let get_oidc_configuration server =
  let url = server ^ "/.well-known/openid-configuration" in
  let%lwt reply = Ocsigen_http_client.get_url url in
  match reply.Ocsigen_http_frame.frame_content with
  | Some stream ->
     let%lwt info = Ocsigen_stream.(string_of_stream 10000 (get stream)) in
     Ocsigen_stream.finalize stream `Success >>
     return (oidc_configuration_of_string info)
  | None -> fail_http 404

let split_prefix_path url =
  let n = String.length url in
  let i = String.rindex url '/' in
  String.sub url 0 i, [String.sub url (i+1) (n-i-1)]

let oidc_login_handler config () =
  match config with
  | [server; client_id; client_secret] ->
     let%lwt ocfg = get_oidc_configuration server in
     let%lwt state = generate_token () in
     Eliom_reference.set oidc_state (Some (ocfg, client_id, client_secret, state)) >>
     let prefix, path = split_prefix_path ocfg.authorization_endpoint in
     let auth_endpoint = Http.external_service ~prefix ~path
       ~get_params:Eliom_parameter.(string "redirect_uri" **
           string "response_type" ** string "client_id" **
           string "scope" ** string "state" ** string "prompt")
       ()
     in
     let service = preapply auth_endpoint
       (Lazy.force oidc_self, ("code", (client_id, ("openid email", (state, "consent")))))
     in
     Eliom_registration.Redirection.send service
  | _ -> failwith "oidc_login_handler invoked with bad config"

(** Generic authentication *)

let get_login_handler service uuid auth_system config =
  Eliom_reference.set auth_env (Some (uuid, service)) >>
  match auth_system with
  | "dummy" -> Web_templates.login_dummy () >>= Eliom_registration.Html5.send
  | "cas" -> cas_login_handler config ()
  | "password" -> Web_templates.login_password () >>= Eliom_registration.Html5.send
  | "oidc" -> oidc_login_handler config ()
  | _ -> fail_http 404

let login_handler service uuid =
  let myself service =
    match uuid with
    | None -> preapply site_login service
    | Some u -> preapply election_login ((u, ()), service)
  in
  match%lwt Eliom_reference.get user with
  | Some _ ->
     cont_push (fun () -> Eliom_registration.Redirection.send (myself service)) >>
     Web_templates.already_logged_in () >>= Eliom_registration.Html5.send
  | None ->
     let uuid_or_empty = match uuid with
       | None -> ""
       | Some u -> Uuidm.to_string u
     in
     let%lwt c = Web_persist.get_auth_config uuid_or_empty in
     match service with
     | Some s ->
        let%lwt auth_system, config =
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
  Eliom_reference.unset Web_state.user >>
  match%lwt cont_pop () with
  | Some f -> f ()
  | None -> Eliom_registration.Redirection.send Web_services.home

let () = Eliom_registration.Any.register ~service:site_login
  (fun service () -> login_handler service None)

let () = Eliom_registration.Any.register ~service:logout
  (fun () () -> logout_handler ())

let () = Eliom_registration.Any.register ~service:election_login
  (fun ((uuid, ()), service) () -> login_handler service (Some uuid))
