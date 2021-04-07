(**************************************************************************)
(*                                BELENIOS                                *)
(*                                                                        *)
(*  Copyright © 2012-2021 Inria                                           *)
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
open Lwt.Syntax
open Eliom_service
open Web_serializable_j
open Web_common
open Web_services

type result = Eliom_registration.Html.result

type post_login_handler =
  uuid option -> auth_config -> (string -> unit Lwt.t) -> (unit, unit) Stdlib.result Lwt.t

let scope = Eliom_common.default_session_scope

let auth_env = Eliom_reference.eref ~scope None

let get_cont login_or_logout x =
  let open Eliom_registration in
  let redir = match x with
    | `Election uuid -> Redirection (preapply ~service:election_cast_fallback uuid)
    | `Site ContSiteHome -> Redirection home
    | `Site ContSiteAdmin -> Redirection admin
    | `Site (ContSiteElection uuid) ->
       match login_or_logout with
       | `Login -> Redirection (preapply ~service:election_admin uuid)
       | `Logout -> Redirection (preapply ~service:election_home (uuid, ()))
  in
  fun () -> Redirection.send redir

let restart_login service = function
  | `Election uuid -> preapply ~service:election_login ((uuid, ()), Some service)
  | `Site cont -> preapply ~service:site_login (Some service, cont)

let run_post_login_handler ~auth_system ~state f =
  let* env = Eliom_reference.get auth_env in
  match env with
  | None -> Eliom_registration.Action.send ()
  | Some (uuid, a, kind, st) ->
     let restart_login () =
       let service = restart_login a.auth_instance kind in
       Pages_common.login_failed ~service ()
       >>= Eliom_registration.Html.send ~code:401
     in
     if auth_system = a.auth_system && st = state then
       let authenticate name =
         let* () = Eliom_reference.unset auth_env in
         let user = { user_domain = a.auth_instance; user_name = name } in
         match uuid with
         | None -> Eliom_reference.set Web_state.site_user (Some user)
         | Some uuid -> Eliom_reference.set Web_state.election_user (Some (uuid, user))
       in
       let* x = f uuid a authenticate in
       match x with
       | Ok () -> get_cont `Login kind ()
       | Error () -> restart_login ()
     else
       restart_login ()

type pre_login_handler = auth_config -> state:string -> result Lwt.t

let pre_login_handlers = ref []

let get_pre_login_handler uuid kind a =
  let* state = generate_token () in
  let* () = Eliom_reference.set auth_env (Some (uuid, a, kind, state)) in
  match List.assoc_opt a.auth_system !pre_login_handlers with
  | Some handler -> handler a ~state
  | None -> fail_http 404

let register_pre_login_handler ~auth_system handler =
  pre_login_handlers := (auth_system, handler) :: !pre_login_handlers;
  run_post_login_handler ~auth_system

let rec find_auth_instance x = function
  | [] -> None
  | { auth_instance = i; _ } as y :: _ when i = x -> Some y
  | _ :: xs -> find_auth_instance x xs

let login_handler service kind =
  let uuid = match kind with
    | `Site _ -> None
    | `Election uuid -> Some uuid
  in
  let myself service =
    match kind with
    | `Site cont -> preapply ~service:site_login (service, cont)
    | `Election uuid -> preapply ~service:election_login ((uuid, ()), service)
  in
  let* user = match uuid with
    | None -> Eliom_reference.get Web_state.site_user
    | Some uuid -> Web_state.get_election_user uuid
  in
  match user, uuid with
  | Some _, None -> get_cont `Login kind ()
  | Some _, Some _ | None, _ ->
     let* c = match uuid with
       | None -> return !Web_config.site_auth_config
       | Some uuid ->
          let* metadata = Web_persist.get_election_metadata uuid in
          match metadata.e_auth_config with
          | None -> return []
          | Some x -> return x
     in
     match service with
     | Some s ->
        let* a =
          match find_auth_instance s c with
          | Some x -> return x
          | None -> fail_http 404
        in
        get_pre_login_handler uuid kind a
     | None ->
        match c with
        | [s] -> Eliom_registration.(Redirection.send (Redirection (myself (Some s.auth_instance))))
        | _ ->
           let builder =
             match kind with
             | `Site cont -> fun s ->
               preapply ~service:Web_services.site_login (Some s, cont)
             | `Election uuid -> fun s ->
               preapply ~service:Web_services.election_login ((uuid, ()), Some s)
           in
           Pages_common.login_choose (List.map (fun x -> x.auth_instance) c) builder () >>=
           Eliom_registration.Html.send

let logout_handler cont =
  let* () = Eliom_reference.unset Web_state.site_user in
  get_cont `Logout (`Site cont) ()

let () = Eliom_registration.Any.register ~service:site_login
  (fun (service, cont) () -> login_handler service (`Site cont))

let () = Eliom_registration.Any.register ~service:logout
  (fun cont () -> logout_handler cont)

let () = Eliom_registration.Any.register ~service:election_login
  (fun ((uuid, ()), service) () -> login_handler service (`Election uuid))
