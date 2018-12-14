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
open Web_services

type result = Eliom_registration.Html.result

type post_login_handler =
  uuid option -> auth_config -> (string -> unit Lwt.t) -> unit Lwt.t

let scope = Eliom_common.default_session_scope

let auth_env = Eliom_reference.eref ~scope None

let run_post_login_handler auth_system f =
  match%lwt Eliom_reference.get auth_env with
  | None -> Printf.ksprintf failwith "%s handler was invoked without environment" auth_system
  | Some (uuid, a, cont) ->
     let%lwt () = Eliom_reference.unset auth_env in
     let authenticate name =
       let user = { user_domain = a.auth_instance; user_name = name } in
       match uuid with
       | None -> Eliom_reference.set Web_state.site_user (Some user)
       | Some uuid -> Eliom_reference.set Web_state.election_user (Some (uuid, user))
     in
     let%lwt () = f uuid a authenticate in
     cont ()

type pre_login_handler = auth_config -> result Lwt.t

let pre_login_handlers = ref []

let get_pre_login_handler uuid cont a =
  let%lwt () = Eliom_reference.set auth_env (Some (uuid, a, cont)) in
  match List.assoc_opt a.auth_system !pre_login_handlers with
  | Some handler -> handler a
  | None -> fail_http 404

let register_pre_login_handler auth_system handler =
  pre_login_handlers := (auth_system, handler) :: !pre_login_handlers

let rec find_auth_instance x = function
  | [] -> None
  | { auth_instance = i; _ } as y :: _ when i = x -> Some y
  | _ :: xs -> find_auth_instance x xs

let get_cont login_or_logout x =
  let open Eliom_registration in
  let redir = match x with
    | `Election uuid -> Redirection (preapply election_cast uuid)
    | `Site ContSiteHome -> Redirection home
    | `Site ContSiteAdmin -> Redirection admin
    | `Site (ContSiteElection uuid) ->
       match login_or_logout with
       | `Login -> Redirection (preapply election_admin uuid)
       | `Logout -> Redirection (preapply election_home (uuid, ()))
  in
  fun () -> Redirection.send redir

let login_handler service kind =
  let uuid = match kind with
    | `Site _ -> None
    | `Election uuid -> Some uuid
  in
  let myself service =
    match kind with
    | `Site cont -> preapply site_login (service, cont)
    | `Election uuid -> preapply election_login ((uuid, ()), service)
  in
  let%lwt user = match uuid with
    | None -> Eliom_reference.get Web_state.site_user
    | Some uuid -> Web_state.get_election_user uuid
  in
  match user with
  | Some _ -> get_cont `Login kind ()
  | None ->
     let%lwt c = match uuid with
       | None -> return !Web_config.site_auth_config
       | Some u -> Web_persist.get_auth_config u
     in
     match service with
     | Some s ->
        let%lwt a =
          match find_auth_instance s c with
          | Some x -> return x
          | None -> fail_http 404
        in
        let cont = get_cont `Login kind in
        get_pre_login_handler uuid cont a
     | None ->
        match c with
        | [s] -> Eliom_registration.(Redirection.send (Redirection (myself (Some s.auth_instance))))
        | _ ->
           let builder =
             match kind with
             | `Site cont -> fun s ->
               preapply Web_services.site_login (Some s, cont)
             | `Election uuid -> fun s ->
               preapply Web_services.election_login ((uuid, ()), Some s)
           in
           Web_templates.login_choose (List.map (fun x -> x.auth_instance) c) builder () >>=
           Eliom_registration.Html.send

let logout_handler cont =
  let%lwt () = Eliom_reference.unset Web_state.site_user in
  get_cont `Logout (`Site cont) ()

let () = Eliom_registration.Any.register ~service:site_login
  (fun (service, cont) () -> login_handler service (`Site cont))

let () = Eliom_registration.Any.register ~service:logout
  (fun cont () -> logout_handler cont)

let () = Eliom_registration.Any.register ~service:election_login
  (fun ((uuid, ()), service) () -> login_handler service (`Election uuid))
