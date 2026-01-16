(**************************************************************************)
(*                                BELENIOS                                *)
(*                                                                        *)
(*  Copyright © 2012-2022 Inria                                           *)
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
open Belenios_storage_api
open Belenios_web_api
open Web_common

module Make (Web_services : Web_services_sig.S) (Web_auth : Web_auth_sig.S) =
struct
  type Web_auth_sig.data += Data_oidc of oidc_configuration

  let login_oidc =
    Eliom_service.create
      ~path:(Eliom_service.Path [ "auth"; "oidc" ])
      ~meth:(Eliom_service.Get Eliom_parameter.any) ()

  let oidc_self =
    lazy
      (Web_services.make_absolute_string_uri
         ~service:(preapply ~service:login_oidc [])
         ())

  let oidc_get_userinfo ocfg info =
    try
      let info = oidc_tokens_of_string info in
      let access_token = info.oidc_access_token in
      let url = ocfg.userinfo_endpoint in
      let headers =
        Cohttp.Header.init_with "Authorization" ("Bearer " ^ access_token)
      in
      let* _, body = Cohttp_lwt_unix.Client.get ~headers (Uri.of_string url) in
      let* info = Cohttp_lwt.Body.to_string body in
      try
        let x = oidc_userinfo_of_string info in
        let info : Belenios_web_api.user_info =
          let login, address =
            match x.oidc_email with
            | Some x -> (x, Some x)
            | None -> (x.oidc_sub, None)
          in
          { login; address; name = x.oidc_name; timestamp = None }
        in
        return_some info
      with _ -> return_none
    with _ -> return_none

  let oidc_get_name ocfg client_id client_secret code =
    let params =
      [
        ("code", [ code ]);
        ("client_id", [ client_id ]);
        ("client_secret", [ client_secret ]);
        ("redirect_uri", [ Lazy.force oidc_self ]);
        ("grant_type", [ "authorization_code" ]);
      ]
    in
    let* _, body =
      Cohttp_lwt_unix.Client.post_form ~params
        (Uri.of_string ocfg.token_endpoint)
    in
    let* info = Cohttp_lwt.Body.to_string body in
    oidc_get_userinfo ocfg info

  let get_oidc_configuration server =
    let url = server ^ "/.well-known/openid-configuration" in
    let* _, body = Cohttp_lwt_unix.Client.get (Uri.of_string url) in
    let* info = Cohttp_lwt.Body.to_string body in
    try return (oidc_configuration_of_string info)
    with _ -> fail_http `Not_found

  let split_prefix_path url =
    let n = String.length url in
    let i = String.rindex url '/' in
    (String.sub url 0 i, [ String.sub url (i + 1) (n - i - 1) ])

  let handler _ a =
    let get x = List.assoc_opt x a.auth_config in
    let module X = struct
      let pre_login_handler _ ~state =
        match (get "server", get "client_id") with
        | Some server, Some client_id ->
            let* ocfg = get_oidc_configuration server in
            Web_auth.State.set_data ~state (Data_oidc ocfg);
            let prefix, path = split_prefix_path ocfg.authorization_endpoint in
            let auth_endpoint =
              Eliom_service.extern ~prefix ~path
                ~meth:
                  (Eliom_service.Get
                     Eliom_parameter.(
                       string "redirect_uri" ** string "response_type"
                       ** string "client_id" ** string "scope" ** string "state"
                       ** string "prompt"))
                ()
            in
            let service =
              preapply ~service:auth_endpoint
                ( Lazy.force oidc_self,
                  ( "code",
                    (client_id, ("openid profile email", (state, "consent"))) )
                )
            in
            let url = Web_services.make_absolute_string_uri ~service () in
            return (Web_auth_sig.Redirection url)
        | _ -> failwith "oidc_login_handler invoked with bad config"

      let direct _ _ =
        failwith "direct authentication not implemented for OpenID Connect"
    end in
    (module X : Web_auth_sig.AUTH_SYSTEM)

  let run_post_login_handler =
    Web_auth.register ~auth_system:"oidc" { handler; extern = true }

  let oidc_handler params () =
    let code = List.assoc_opt "code" params in
    let state = List.assoc_opt "state" params in
    match (code, state) with
    | Some code, Some state ->
        run_post_login_handler ~state
          {
            Web_auth.post_login_handler =
              (fun _ a cont ->
                let get x = List.assoc_opt x a.auth_config in
                match (get "client_id", get "client_secret") with
                | Some client_id, Some client_secret ->
                    let ocfg =
                      match Web_auth.State.get_data ~state with
                      | Data_oidc x -> x
                      | _ ->
                          failwith
                            "oidc handler was invoked without discovered \
                             configuration"
                    in
                    let* name =
                      oidc_get_name ocfg client_id client_secret code
                    in
                    cont name
                | _, _ -> fail_http `Service_unavailable);
          }
    | _, _ -> fail_http `Unauthorized

  let () = Eliom_registration.Any.register ~service:login_oidc oidc_handler
end
