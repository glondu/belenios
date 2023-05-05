(**************************************************************************)
(*                                BELENIOS                                *)
(*                                                                        *)
(*  Copyright © 2012-2023 Inria                                           *)
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
open Belenios_core
open Common
open Web_serializable_t
open Web_common

type login_env = {
  username_or_address : [ `Username | `Address ];
  state : string;
  auth_instance : string;
}

module Make
    (Web_state : Web_state_sig.S)
    (Web_services : Web_services_sig.S)
    (Pages_common : Pages_common_sig.S)
    (Web_auth : Web_auth_sig.S) =
struct
  module HashedInt = struct
    type t = int

    let equal = ( = )
    let hash x = x
  end

  module Captcha_throttle = Lwt_throttle.Make (HashedInt)

  let captcha_throttle = Captcha_throttle.create ~rate:1 ~max:5 ~n:1

  let scope =
    `Session (Eliom_common.create_scope_hierarchy "belenios-auth-email")

  let uuid_ref = Eliom_reference.eref ~scope None
  let env = Eliom_reference.eref ~scope None
  let login_env = Eliom_reference.eref ~scope None

  let auth_system uuid { auth_config; auth_instance; _ } =
    let module X = struct
      let pre_login_handler username_or_address ~state =
        let* () = Eliom_reference.set uuid_ref uuid in
        let site_or_election =
          match uuid with None -> `Site | Some _ -> `Election
        in
        match List.assoc_opt "use_captcha" auth_config with
        | Some "true" ->
            let* b = Captcha_throttle.wait captcha_throttle 0 in
            if b then
              let* challenge = Web_captcha.create_captcha () in
              let* fragment =
                Pages_common.login_email_captcha ~state None challenge ""
              in
              return @@ Web_auth_sig.Html fragment
            else
              let* fragment = Pages_common.login_email_not_now () in
              return @@ Web_auth_sig.Html fragment
        | _ ->
            if site_or_election = `Election then
              let env = { username_or_address; state; auth_instance } in
              let* () = Eliom_reference.set login_env (Some env) in
              let service = Web_services.email_election_login in
              let url =
                Eliom_uri.make_string_uri ~service ~absolute:true ()
                |> rewrite_prefix
              in
              return @@ Web_auth_sig.Redirection url
            else
              let* fragment =
                Pages_common.login_email site_or_election username_or_address
                  ~state
              in
              return @@ Web_auth_sig.Html fragment

      let direct _ = failwith "direct authentication not implemented for email"
    end in
    (module X : Web_auth_sig.AUTH_SYSTEM)

  let run_post_login_handler =
    Web_auth.register ~auth_system:"email" auth_system

  module Sender = struct
    let send ~address ~code =
      let* subject, body = Pages_common.email_email ~address ~code in
      send_email ~subject ~body ~recipient:address MailLogin
  end

  module Otp = Otp.Make (Sender) ()

  let handle_email_post ~state ?(show_email_address = false) name ok =
    let name = String.trim name in
    let* address, site_or_election =
      let* uuid = Eliom_reference.get uuid_ref in
      match uuid with
      | None -> return ((if is_email name then Some name else None), `Site)
      | Some uuid ->
          let* address =
            let* x = Web_persist.get_voter uuid name in
            match x with
            | None -> Lwt.return_none
            | Some v ->
                let address, _, _ = Voter.get v in
                Lwt.return_some address
          in
          return (address, `Election)
    in
    match (ok, address) with
    | true, Some address ->
        let* () = Otp.generate ~address in
        let* () = Eliom_reference.set env (Some (state, name, address)) in
        let* () = Eliom_reference.unset uuid_ref in
        let address = if show_email_address then Some address else None in
        Pages_common.email_login ?address site_or_election
        >>= Eliom_registration.Html.send
    | _ ->
        run_post_login_handler ~state
          { Web_auth.post_login_handler = (fun _ _ cont -> cont None) }

  let () =
    Eliom_registration.Any.register ~service:Web_services.email_election_login
      (fun () () ->
        let* env = Eliom_reference.get login_env in
        match env with
        | None ->
            Pages_common.authentication_impossible ()
            >>= Eliom_registration.Html.send
        | Some { username_or_address; state; auth_instance } -> (
            let* precast_data = Eliom_reference.get Web_state.precast_data in
            match precast_data with
            | Some (_, { cr_username = Some name; _ }) ->
                handle_email_post ~show_email_address:true ~state name true
            | _ ->
                let* fragment =
                  Pages_common.login_email `Election username_or_address ~state
                in
                let* title = Pages_common.login_title `Election auth_instance in
                Pages_common.base ~title ~content:[ fragment ] ()
                >>= Eliom_registration.Html.send))

  let () =
    Eliom_registration.Any.register ~service:Web_services.email_post
      (fun () (state, name) -> handle_email_post ~state name true)

  let () =
    Eliom_registration.Any.register ~service:Web_services.email_captcha_post
      (fun () (state, (challenge, (response, name))) ->
        let* b = Web_captcha.check_captcha ~challenge ~response in
        handle_email_post ~state name b)

  let () =
    Eliom_registration.Any.register ~service:Web_services.email_login_post
      (fun () code ->
        let code = String.trim code in
        let* x = Eliom_reference.get env in
        match x with
        | Some (state, name, address) ->
            run_post_login_handler ~state
              {
                Web_auth.post_login_handler =
                  (fun _ _ cont ->
                    let* ok =
                      if Otp.check ~address ~code then
                        let* () = Eliom_state.discard ~scope () in
                        return_some (name, address)
                      else return_none
                    in
                    cont ok);
              }
        | None ->
            run_post_login_handler ~state:""
              { Web_auth.post_login_handler = (fun _ _ cont -> cont None) })
end
