(**************************************************************************)
(*                                BELENIOS                                *)
(*                                                                        *)
(*  Copyright © 2012-2020 Inria                                           *)
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

let scope = Eliom_common.default_session_scope

let oidc_state = Eliom_reference.eref ~scope None

let login_oidc = Eliom_service.create
  ~path:(Eliom_service.Path ["auth"; "oidc"])
  ~meth:(Eliom_service.Get Eliom_parameter.any)
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
     let%lwt () = Ocsigen_stream.finalize stream `Success in
     let x = oidc_userinfo_of_string info in
     return_some (match x.oidc_email with Some x -> x | None -> x.oidc_sub)
  | None -> return_none

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
    let%lwt () = Ocsigen_stream.finalize stream `Success in
    oidc_get_userinfo ocfg info
  | None -> return_none

let oidc_handler params () =
  let code = List.assoc_opt "code" params in
  let state = List.assoc_opt "state" params in
  match code, state with
  | Some code, Some state ->
     Web_auth.run_post_login_handler ~auth_system:"oidc" ~state
       (fun _ _ authenticate ->
         let%lwt ocfg, client_id, client_secret =
           match%lwt Eliom_reference.get oidc_state with
           | None -> failwith "oidc handler was invoked without a state"
           | Some x -> return x
         in
         let%lwt () = Eliom_reference.unset oidc_state in
         match%lwt oidc_get_name ocfg client_id client_secret code with
         | Some name -> authenticate name
         | None -> fail_http 401
       )
  | _, _ -> fail_http 401

let () = Eliom_registration.Any.register ~service:login_oidc oidc_handler

let get_oidc_configuration server =
  let url = server ^ "/.well-known/openid-configuration" in
  let%lwt reply = Ocsigen_http_client.get_url url in
  match reply.Ocsigen_http_frame.frame_content with
  | Some stream ->
     let%lwt info = Ocsigen_stream.(string_of_stream 10000 (get stream)) in
     let%lwt () = Ocsigen_stream.finalize stream `Success in
     return (oidc_configuration_of_string info)
  | None -> fail_http 404

let split_prefix_path url =
  let n = String.length url in
  let i = String.rindex url '/' in
  String.sub url 0 i, [String.sub url (i+1) (n-i-1)]

let oidc_login_handler a ~state =
  let get x = List.assoc_opt x a.auth_config in
  match get "server", get "client_id", get "client_secret" with
  | Some server, Some client_id, Some client_secret ->
     let%lwt ocfg = get_oidc_configuration server in
     let%lwt () = Eliom_reference.set oidc_state (Some (ocfg, client_id, client_secret)) in
     let prefix, path = split_prefix_path ocfg.authorization_endpoint in
     let auth_endpoint = Eliom_service.extern ~prefix ~path
       ~meth:(Eliom_service.Get Eliom_parameter.(string "redirect_uri" **
           string "response_type" ** string "client_id" **
           string "scope" ** string "state" ** string "prompt"))
       ()
     in
     let service = preapply auth_endpoint
       (Lazy.force oidc_self, ("code", (client_id, ("openid email", (state, "consent")))))
     in
     Eliom_registration.(Redirection.send (Redirection service))
  | _ -> failwith "oidc_login_handler invoked with bad config"

let () = Web_auth.register_pre_login_handler "oidc" oidc_login_handler
