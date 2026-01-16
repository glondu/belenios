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
open Belenios
open Belenios_storage_api
open Belenios_server_core
open Web_common

module Make
    (Web_i18n : Web_i18n_sig.S)
    (Web_state : Web_state_sig.S)
    (Web_services : Web_services_sig.S)
    (Pages_common : Pages_common_sig.S)
    (Web_auth : Web_auth_sig.S) =
struct
  type Web_auth_sig.data += Data_email of Belenios_messages.recipient

  module Captcha_throttle = Lwt_throttle.Make (Int)

  let captcha_throttle = Captcha_throttle.create ~rate:1 ~max:5 ~n:1

  let handler uuid ({ auth_config; _ } : auth_config) =
    let module X = struct
      let pre_login_handler username_or_address ~state =
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
              return (Web_auth_sig.Html fragment)
            else
              let* fragment = Pages_common.login_email_not_now () in
              return (Web_auth_sig.Html fragment)
        | _ -> (
            match site_or_election with
            | `Election ->
                let service = Web_services.email_election_login in
                let url =
                  Web_services.make_absolute_string_uri ~service state
                in
                return (Web_auth_sig.Redirection url)
            | `Site ->
                let* fragment =
                  Pages_common.login_email site_or_election username_or_address
                    ~state
                in
                return (Web_auth_sig.Html fragment))

      let direct _ _ =
        failwith "direct authentication not implemented for email"
    end in
    (module X : Web_auth_sig.AUTH_SYSTEM)

  let run_post_login_handler =
    Web_auth.register ~auth_system:"email" { handler; extern = false }

  module Sender = struct
    type payload = unit
    type context = uuid option

    let send ?lang ~context ~recipient ?state ~code () =
      let* l = Web_i18n.get_preferred_gettext ?lang "voter" in
      let open (val l) in
      Send_message.send
      @@ `Mail_login
           {
             lang = Language.unwrap lang;
             recipient;
             state;
             code;
             uuid = context;
           }
  end

  module Otp = Otp.Make (Sender) ()

  let handle_email_post ~state ?(show_email_address = false) name ok =
    let name = String.trim name in
    let election_env = Web_auth.State.get_election ~state in
    let uuid =
      let@ { uuid; _ } = Option.bind election_env in
      Some uuid
    in
    let* address, site_or_election =
      match uuid with
      | None -> return ((if is_email name then Some name else None), `Site)
      | Some uuid ->
          let* address =
            let@ s = Storage.with_transaction in
            let*& _, { address; login; _ } =
              Web_persist.get_voter s uuid name
            in
            match (address, login) with
            | Some x, _ -> Lwt.return_some x
            | _, Some x -> Lwt.return_some x
            | _ -> Lwt.return_none
          in
          return (address, `Election)
    in
    let lang =
      match election_env with
      | Some { state = Some { lang; _ }; _ } -> lang
      | _ -> None
    in
    match (ok, address) with
    | true, Some address ->
        let recipient : Belenios_messages.recipient = { name; address } in
        let* r =
          Otp.generate ?lang ~context:uuid ~recipient ~state ~payload:() ()
        in
        Web_auth.State.set_data ~state (Data_email recipient);
        let address = if show_email_address then Some r else None in
        Pages_common.email_login ?lang ?address ~state site_or_election
        >>= Eliom_registration.Html.send
    | _ ->
        run_post_login_handler ~state
          { Web_auth.post_login_handler = (fun _ _ cont -> cont None) }

  let () =
    Eliom_registration.Any.register ~service:Web_services.email_election_login
      (fun state () ->
        match Web_auth.State.get_auth ~state with
        | Some { username_or_address; auth_instance } -> (
            let@ precast_data cont =
              let x = Web_auth.State.get_election ~state in
              match x with
              | Some { state = Some { precast_data; _ }; _ } ->
                  cont (Some precast_data)
              | _ -> cont None
            in
            match precast_data with
            | Some { credential_record = { cr_username = Some name; _ }; _ } ->
                handle_email_post ~show_email_address:true ~state name true
            | _ ->
                let* fragment =
                  Pages_common.login_email `Election username_or_address ~state
                in
                let* title = Pages_common.login_title `Election auth_instance in
                Pages_common.base ~title ~content:[ fragment ] ()
                >>= Eliom_registration.Html.send)
        | _ ->
            Pages_common.authentication_impossible ()
            >>= Eliom_registration.Html.send)

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
      (fun () (state, code) ->
        let code = String.trim code in
        match Web_auth.State.get_data ~state with
        | Data_email { name = login; address } ->
            run_post_login_handler ~state
              {
                Web_auth.post_login_handler =
                  (fun _ _ cont ->
                    let* ok =
                      match Otp.check ~address ~code with
                      | Some () ->
                          let info : Belenios_web_api.user_info =
                            {
                              login;
                              name = None;
                              address = Some address;
                              timestamp = None;
                            }
                          in
                          return_some info
                      | None -> return_none
                    in
                    cont ok);
              }
        | _ ->
            run_post_login_handler ~state:""
              { Web_auth.post_login_handler = (fun _ _ cont -> cont None) })

  let () =
    Eliom_registration.Any.register ~service:Web_services.email_login_link
      (fun (state, code) () ->
        let election_env = Web_auth.State.get_election ~state in
        let site_or_election =
          match election_env with Some _ -> `Election | None -> `Site
        in
        let lang =
          match election_env with
          | Some { state = Some { lang; _ }; _ } -> lang
          | _ -> None
        in
        Pages_common.email_login ?lang ~code ~state site_or_election
        >>= Eliom_registration.Html.send)
end
