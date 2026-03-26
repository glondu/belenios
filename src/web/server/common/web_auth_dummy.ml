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

open Lwt.Syntax
open Lwt
open Belenios
open Belenios_storage_api

let perform_admin_login = Web_auth.perform_admin_login

module Make
    (Web_services : Web_services_sig.S)
    (Pages_common : Pages_common_sig.S)
    (Web_auth : Web_auth_sig.S) =
struct
  let handler uuid _ =
    let module X = struct
      let pre_login_handler username_or_address ~state =
        let site_or_election =
          match uuid with None -> `Site | Some _ -> `Election
        in
        let* page =
          Pages_common.login_dummy site_or_election username_or_address ~state
        in
        return (Web_auth_sig.Html page)
    end in
    (module X : Web_auth_sig.AUTH_SYSTEM)

  let dispatch a endpoint method_ (body : Api_generic.body) =
    match endpoint with
    | [] -> (
        match method_ with
        | `POST -> (
            let@ i = body.run Belenios_web_api.auth_dummy_info_of_string in
            let user =
              { user_domain = a.auth_instance; user_name = i.username }
            in
            let* account =
              perform_admin_login a ~name:None ~address:None user
            in
            match account with
            | Ok account ->
                let* token = Api_generic.new_token account user in
                Api_generic.return_json 200
                @@ Belenios_web_api.string_of_auth_token token
            | Error () -> Api_generic.forbidden)
        | _ -> Api_generic.method_not_allowed)
    | _ -> Api_generic.not_found

  let run_post_login_handler =
    Web_auth.register ~auth_system:"dummy" { handler; extern = false; dispatch }

  let () =
    Eliom_registration.Any.register ~service:Web_services.dummy_post
      (fun () (state, login) ->
        run_post_login_handler ~state
          {
            Web_auth.post_login_handler =
              (fun _ _ cont ->
                let info : Belenios_web_api.user_info =
                  { login; name = None; address = None; timestamp = None }
                in
                cont (Some info));
          })
end
