(**************************************************************************)
(*                                BELENIOS                                *)
(*                                                                        *)
(*  Copyright © 2012-2025 Inria                                           *)
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
open Web_common

module Make (Web_services : Web_services_sig.S) (Web_auth : Web_auth_sig.S) =
struct
  let get_context =
    let open Eliom_service in
    create
      ~path:(Path [ "auth"; "belenios"; "get-context" ])
      ~meth:(Get Eliom_parameter.(string "state"))
      ()

  let () =
    let open Eliom_registration in
    Any.register ~service:get_context (fun state () ->
        let context : Belenios_web_api.connect_context =
          match Web_auth.State.get_election ~state with
          | Some x ->
              let username =
                match x.state with
                | None -> None
                | Some x -> x.precast_data.credential_record.cr_username
              in
              { kind = `Election x.uuid; username }
          | None -> { kind = `Site; username = None }
        in
        String.send
          ( Belenios_web_api.string_of_connect_context context,
            "application/json" ))

  let login_belenios =
    Eliom_service.create
      ~path:(Eliom_service.Path [ "auth"; "belenios"; "return" ])
      ~meth:(Eliom_service.Get Eliom_parameter.any) ()

  let belenios_get_info ~server ~code =
    let url =
      let service =
        Eliom_service.extern ~prefix:server ~path:[ "validate" ]
          ~meth:(Get Eliom_parameter.(string "code"))
          ()
      in
      let service = preapply ~service code in
      Eliom_uri.make_string_uri ~absolute:true ~service ()
    in
    let* x, body = Cohttp_lwt_unix.Client.get (Uri.of_string url) in
    let* info = Cohttp_lwt.Body.to_string body in
    match Cohttp.Code.code_of_status x.status with
    | 200 -> (
        match Belenios_web_api.user_info_of_string info with
        | x -> Lwt.return_some x
        | exception _ -> Lwt.return_none)
    | _ -> Lwt.return_none

  let handler _ a =
    let get x = List.assoc_opt x a.auth_config in
    let module X = struct
      let pre_login_handler _ ~state =
        match (get "server", get "callback") with
        | Some prefix, Some callback ->
            let auth_endpoint =
              Eliom_service.extern ~prefix ~path:[ "login" ]
                ~meth:
                  (Eliom_service.Get
                     Eliom_parameter.(string "callback" ** string "state"))
                ()
            in
            let service = preapply ~service:auth_endpoint (callback, state) in
            let url = Web_services.make_absolute_string_uri ~service () in
            return (Web_auth_sig.Redirection url)
        | _ -> failwith "belenios_login_handler invoked with bad config"

      let direct _ _ =
        failwith "direct authentication not implemented for Belenios Connect"
    end in
    (module X : Web_auth_sig.AUTH_SYSTEM)

  let run_post_login_handler =
    Web_auth.register ~auth_system:"belenios" { handler; extern = true }

  let belenios_handler params () =
    let code = List.assoc_opt "code" params in
    let state = List.assoc_opt "state" params in
    match (code, state) with
    | Some code, Some state ->
        run_post_login_handler ~state
          {
            Web_auth.post_login_handler =
              (fun _ a cont ->
                let get x = List.assoc_opt x a.auth_config in
                match get "server" with
                | Some server ->
                    let* info = belenios_get_info ~server ~code in
                    cont info
                | _ -> fail_http `Service_unavailable);
          }
    | _ -> fail_http `Unauthorized

  let () =
    Eliom_registration.Any.register ~service:login_belenios belenios_handler
end
