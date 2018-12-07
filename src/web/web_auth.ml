(**************************************************************************)
(*                                BELENIOS                                *)
(*                                                                        *)
(*  Copyright © 2012-2018 Inria                                           *)
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
open Web_serializable_j
open Web_common
open Web_state
open Web_services

type auth_config = (string * string) list

type result = Eliom_registration.Html.result

type post_login_handler =
  uuid option -> auth_config -> (string -> unit Lwt.t) -> unit Lwt.t

let scope = Eliom_common.default_session_scope

let auth_env = Eliom_reference.eref ~scope None

let default_cont uuid () =
  match%lwt cont_pop () with
  | Some f -> f ()
  | None ->
     match uuid with
     | None ->
        Eliom_registration.(Redirection.send (Redirection Web_services.admin))
     | Some u ->
        Eliom_registration.(Redirection.send (Redirection (preapply Web_services.election_home (u, ()))))

let run_post_login_handler auth_system f =
  match%lwt Eliom_reference.get auth_env with
  | None -> Printf.ksprintf failwith "%s handler was invoked without environment" auth_system
  | Some (uuid, service, config) ->
     let%lwt () = Eliom_reference.unset auth_env in
     let authenticate name = Eliom_reference.set user (Some {uuid; service; name}) in
     let%lwt () = f uuid config authenticate in
     default_cont uuid ()

type pre_login_handler = auth_config -> result Lwt.t

let pre_login_handlers = ref []

let get_pre_login_handler service uuid auth_system config =
  let%lwt () = Eliom_reference.set auth_env (Some (uuid, service, config)) in
  match List.assoc_opt auth_system !pre_login_handlers with
  | Some handler -> handler config
  | None -> fail_http 404

let register_pre_login_handler auth_system handler =
  pre_login_handlers := (auth_system, handler) :: !pre_login_handlers

let rec find_auth_instance x = function
  | [] -> None
  | { auth_instance = i; auth_system = s; auth_config = c } :: _ when i = x -> Some (s, c)
  | _ :: xs -> find_auth_instance x xs

let login_handler service uuid =
  let myself service =
    match uuid with
    | None -> preapply site_login service
    | Some u -> preapply election_login ((u, ()), service)
  in
  match%lwt Eliom_reference.get user with
  | Some _ ->
     let%lwt () = cont_push (fun () -> Eliom_registration.(Redirection.send (Redirection (myself service)))) in
     Web_templates.already_logged_in () >>= Eliom_registration.Html.send
  | None ->
     let%lwt c = match uuid with
       | None -> return !Web_config.site_auth_config
       | Some u -> Web_persist.get_auth_config u
     in
     match service with
     | Some s ->
        let%lwt auth_system, config =
          match find_auth_instance s c with
          | Some x -> return x
          | None -> fail_http 404
        in
        get_pre_login_handler s uuid auth_system config
     | None ->
        match c with
        | [s] -> Eliom_registration.(Redirection.send (Redirection (myself (Some s.auth_instance))))
        | _ ->
           let builder =
             match uuid with
             | None -> fun s ->
               preapply Web_services.site_login (Some s)
             | Some u -> fun s ->
               preapply Web_services.election_login ((u, ()), Some s)
           in
           Web_templates.login_choose (List.map (fun x -> x.auth_instance) c) builder () >>=
           Eliom_registration.Html.send

let logout_handler () =
  let%lwt () = Eliom_reference.unset Web_state.user in
  match%lwt cont_pop () with
  | Some f -> f ()
  | None -> Eliom_registration.(Redirection.send (Redirection Web_services.home))

let () = Eliom_registration.Any.register ~service:site_login
  (fun service () -> login_handler service None)

let () = Eliom_registration.Any.register ~service:logout
  (fun () () -> logout_handler ())

let () = Eliom_registration.Any.register ~service:election_login
  (fun ((uuid, ()), service) () -> login_handler service (Some uuid))
