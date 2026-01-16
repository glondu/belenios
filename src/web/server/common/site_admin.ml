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
open Web_common
module Pages_admin_root = Pages_admin

module Make
    (X : Pages_sig.S)
    (Site_common : Site_common_sig.S)
    (Web_auth : Web_auth_sig.S) =
struct
  open X
  open Web_services
  open Site_common
  open Eliom_service
  open Eliom_registration

  let get_preferred_gettext ?lang () =
    Web_i18n.get_preferred_gettext ?lang "admin"

  let () =
    let@ a = Accounts.add_update_hook in
    let* user = Eliom_reference.get Web_state.site_user in
    match user with
    | Some (u, b, t) when a.id = b.id ->
        Eliom_reference.set Web_state.site_user (Some (u, a, t))
    | _ -> Lwt.return_unit

  let with_site_user f =
    let* user = Eliom_reference.get Web_state.site_user in
    match user with Some u -> f u | None -> forbidden ()

  let with_metadata_check_owner s uuid f =
    let* user = Eliom_reference.get Web_state.site_user in
    let* metadata = Web_persist.get_election_metadata s uuid in
    match user with
    | Some (_, a, _) when Accounts.check a metadata.e_owners -> f metadata
    | _ -> forbidden ()

  let () =
    Redirection.register ~service:privacy_notice_accept (fun () cont ->
        let () = Web_state.set_consent_cookie () in
        let cont =
          match cont with
          | ContAdmin -> Redirection home
          | ContSignup service ->
              Redirection (preapply ~service:signup_captcha service)
        in
        return cont)

  let () =
    Any.register ~service:sealing (fun () () ->
        let* site_user = Eliom_reference.get Web_state.site_user in
        match site_user with
        | None -> forbidden ()
        | Some _ -> (
            match !Web_config.sealing with
            | Some (file, content_type) -> File.send ~content_type file
            | None -> fail_http `Not_found))

  let () =
    Any.register ~service:home (fun () () ->
        let* site_user = Eliom_reference.get Web_state.site_user in
        match site_user with
        | None ->
            Pages_admin.admin_login Web_auth.get_site_login_handler
            >>= Html.send
        | Some (_, a, _) ->
            let* show =
              match a.consent with
              | Some x when x >= !Web_config.tos_last_update -> return_false
              | _ -> (
                  let@ s = Storage.with_transaction in
                  let@ x, set = Accounts.update_account_by_id s a.id in
                  match Lopt.get_value x with
                  | None -> return_true
                  | Some a ->
                      let current_consent =
                        match a.consent with None -> 0. | Some x -> x
                      in
                      if
                        current_consent < !Web_config.tos_last_update
                        && Web_state.get_consent_cookie ()
                      then return_true
                      else
                        let consent = Some (Unix.gettimeofday ()) in
                        let* () = set { a with consent } in
                        return_false)
            in
            if show then Pages_admin.privacy_notice ContAdmin >>= Html.send
            else if a.email = None then Pages_admin.set_email () >>= Html.send
            else String_redirection.send (make_admin_link None))

  module SetEmailSender = struct
    type payload = unit
    type context = unit

    let send ?lang ~context:() ~recipient ?state ~code () =
      let* l = get_preferred_gettext ?lang () in
      let open (val l) in
      Send_message.send
      @@ `Account_set_email
           { lang = Language.unwrap lang; recipient; state; code; uuid = None }
  end

  module SetEmailOtp = Otp.Make (SetEmailSender) ()

  let () =
    Any.register ~service:set_email_post (fun () address ->
        let@ _, account, _ = with_site_user in
        if is_email ~blacklist:!Web_config.blacklisted_domains address then
          let* () =
            Eliom_reference.set Web_state.set_email_env (Some address)
          in
          let* r =
            SetEmailOtp.generate ~context:()
              ~recipient:{ name = account.name; address }
              ~payload:() ()
          in
          Pages_admin.set_email_confirm r >>= Html.send
        else
          let* l = get_preferred_gettext () in
          let open (val l) in
          let msg = s_ "This e-mail address is invalid!" in
          let title = s_ "Error" in
          Pages_common.generic_page ~title msg () >>= Html.send ~code:400)

  let () =
    Any.register ~service:set_email_confirm (fun () code ->
        let* u = Eliom_reference.get Web_state.site_user in
        let* x = Eliom_reference.get Web_state.set_email_env in
        match (x, u) with
        | None, _ | _, None -> forbidden ()
        | Some address, Some (_, a, _) -> (
            match SetEmailOtp.check ~address ~code with
            | Some () ->
                let@ s = Storage.with_transaction in
                let@ a, set =
                 fun cont ->
                  let@ x, set = Accounts.update_account_by_id s a.id in
                  match Lopt.get_value x with
                  | None -> Lwt.fail @@ Failure "set_email_confirm"
                  | Some x -> cont (x, set)
                in
                let* () = set { a with email = Some address } in
                let* () = Web_state.discard () in
                Redirection.send (Redirection home)
            | None ->
                let* l = get_preferred_gettext () in
                let open (val l) in
                let msg =
                  s_
                    "The provided code is incorrect. Please go back and try \
                     again."
                in
                let title = s_ "Incorrect code" in
                Pages_common.generic_page ~title msg () >>= Html.send ~code:403))

  let () =
    Any.register ~service:election_download_archive (fun (uuid, ()) () ->
        let@ s = Storage.with_transaction in
        let@ _ = with_metadata_check_owner s uuid in
        let* l = get_preferred_gettext () in
        let open (val l) in
        Lwt.try_bind
          (fun () ->
            Storage.get_unixfilename s (Election (uuid, Confidential_archive)))
          (fun archive_name ->
            File.send ~content_type:"application/zip" archive_name)
          (function
            | Not_found ->
                Pages_common.generic_page ~title:(s_ "Error")
                  (s_ "The election is not archived!")
                  ()
                >>= Html.send
            | e -> Lwt.reraise e))

  module HashedInt = struct
    type t = int

    let equal = ( = )
    let hash = Fun.id
  end

  module Captcha_throttle = Lwt_throttle.Make (HashedInt)

  let captcha_throttle = Captcha_throttle.create ~rate:1 ~max:5 ~n:1

  let signup_captcha_handler service error email =
    let* l = get_preferred_gettext () in
    let open (val l) in
    let* b = Captcha_throttle.wait captcha_throttle 0 in
    if b then
      let* challenge = Web_captcha.create_captcha () in
      Pages_admin.signup_captcha ~service error challenge email
    else
      let service = preapply ~service:signup_captcha service in
      Pages_common.generic_page ~title:(s_ "Create account") ~service
        (s_ "You cannot create an account now. Please try later.")
        ()

  let () =
    Html.register ~service:signup_captcha (fun service () ->
        let b = Web_state.get_consent_cookie () in
        if b then Pages_admin.privacy_notice (ContSignup service)
        else signup_captcha_handler service None "")

  let () =
    Html.register ~service:signup_captcha_post
      (fun service (challenge, (response, email)) ->
        let* l = get_preferred_gettext () in
        let open (val l) in
        let* error =
          let* ok = Web_captcha.check_captcha ~challenge ~response in
          if ok then
            if is_email ~blacklist:!Web_config.blacklisted_domains email then
              return_none
            else return_some BadAddress
          else return_some BadCaptcha
        in
        match error with
        | None ->
            let* r =
              Web_signup.send_confirmation_code l ~service
                ~recipient:{ name = email; address = email }
            in
            let* () =
              Eliom_reference.set Web_state.signup_address (Some email)
            in
            Pages_admin.signup_login r
        | _ -> signup_captcha_handler service error email)

  let changepw_captcha_handler service error email username =
    let* l = get_preferred_gettext () in
    let open (val l) in
    let* b = Captcha_throttle.wait captcha_throttle 1 in
    if b then
      let* challenge = Web_captcha.create_captcha () in
      Pages_admin.signup_changepw ~service error challenge email username
    else
      let service = preapply ~service:changepw_captcha service in
      Pages_common.generic_page ~title:(s_ "Change password") ~service
        (s_ "You cannot change your password now. Please try later.")
        ()

  let () =
    Html.register ~service:changepw_captcha (fun service () ->
        changepw_captcha_handler service None "" "")

  let () =
    Html.register ~service:changepw_captcha_post
      (fun service (challenge, (response, (email, username))) ->
        let* l = get_preferred_gettext () in
        let open (val l) in
        let* error =
          let* ok = Web_captcha.check_captcha ~challenge ~response in
          if ok then return_none else return_some BadCaptcha
        in
        match error with
        | None ->
            let* r =
              let* x =
                Web_auth_password.lookup_account ~service ~email ~username
              in
              match x with
              | Some (username, Some address) ->
                  let* () =
                    Eliom_reference.set Web_state.signup_address (Some address)
                  in
                  Web_signup.send_changepw_code l ~service
                    ~recipient:{ name = username; address }
              | _ ->
                  let () =
                    Printf.ksprintf Ocsigen_messages.warning
                      "Unsuccessful attempt to change the password of %S (%S) \
                       for service %s"
                      username email service
                  in
                  Lwt.return_error ()
            in
            Pages_admin.signup_login r
        | _ -> changepw_captcha_handler service error email username)

  let () =
    Any.register ~service:signup_login_post (fun () code ->
        let code = Stdlib.String.trim code in
        let* address = Eliom_reference.get Web_state.signup_address in
        match address with
        | None -> forbidden ()
        | Some address -> (
            match Web_signup.confirm_code ~address ~code with
            | Some x ->
                let* () = Eliom_reference.set Web_state.signup_env (Some x) in
                redir_preapply signup () ()
            | _ -> forbidden ()))

  let () =
    Any.register ~service:signup (fun () () ->
        let* address = Eliom_reference.get Web_state.signup_address in
        let* x = Eliom_reference.get Web_state.signup_env in
        match (address, x) with
        | Some address, Some { kind = CreateAccount; _ } ->
            Pages_admin.signup address None "" >>= Html.send
        | Some address, Some { kind = ChangePassword { username }; _ } ->
            Pages_admin.changepw ~username ~address None >>= Html.send
        | _ -> forbidden ())

  let () =
    Any.register ~service:signup_post
      (fun () (username, (password, password2)) ->
        let* l = get_preferred_gettext () in
        let open (val l) in
        let* address = Eliom_reference.get Web_state.signup_address in
        let* x = Eliom_reference.get Web_state.signup_env in
        match (address, x) with
        | Some email, Some { service; kind = CreateAccount } ->
            if password = password2 then
              let user = { user_name = username; user_domain = service } in
              let* x = Web_auth_password.add_account user ~password ~email in
              match x with
              | Ok () ->
                  let* () = Web_state.discard () in
                  let service =
                    preapply ~service:site_login
                      (Some service, default_admin ContSiteHome)
                  in
                  Pages_common.generic_page ~title:(s_ "Create account")
                    ~service
                    (s_ "The account has been created.")
                    ()
                  >>= Html.send
              | Error e ->
                  Pages_admin.signup email (Some e) username >>= Html.send
            else
              Pages_admin.signup email (Some PasswordMismatch) username
              >>= Html.send
        | _ -> forbidden ())

  let () =
    Any.register ~service:changepw_post (fun () (password, password2) ->
        let* l = get_preferred_gettext () in
        let open (val l) in
        let* address = Eliom_reference.get Web_state.signup_address in
        let* x = Eliom_reference.get Web_state.signup_env in
        match (address, x) with
        | Some address, Some { service; kind = ChangePassword { username } } ->
            if password = password2 then
              let user = { user_name = username; user_domain = service } in
              let* x = Web_auth_password.change_password user ~password in
              match x with
              | Ok () ->
                  let* () = Web_state.discard () in
                  let service =
                    preapply ~service:site_login
                      (Some service, default_admin ContSiteHome)
                  in
                  Pages_common.generic_page ~title:(s_ "Change password")
                    ~service
                    (s_ "The password has been changed.")
                    ()
                  >>= Html.send
              | Error e ->
                  Pages_admin.changepw ~username ~address (Some e) >>= Html.send
            else
              Pages_admin.changepw ~username ~address (Some PasswordMismatch)
              >>= Html.send
        | _ -> forbidden ())

  let () =
    Html.register ~service:compute_fingerprint (fun () () ->
        Pages_admin.compute_fingerprint ())

  let has_sudo_capability f =
    let* x = Eliom_reference.get Web_state.site_user in
    match x with
    | Some (_, a, token) when Accounts.(has_capability Sudo a) -> f token
    | _ -> forbidden ()

  let () =
    Any.register ~service:sudo (fun () () ->
        let@ _ = has_sudo_capability in
        Pages_admin.sudo () >>= Html.send)

  let () =
    Any.register ~service:sudo_post (fun () (user_domain, user_name) ->
        let@ token = has_sudo_capability in
        let u = { user_domain; user_name } in
        let* id = Storage.get_user_id u in
        let fail () =
          let* l = get_preferred_gettext () in
          let open (val l) in
          let msg = s_ "This account does not exist" in
          let title = s_ "Account not found" in
          Pages_common.generic_page ~title ~service:sudo msg () >>= Html.send
        in
        match id with
        | None -> fail ()
        | Some id -> (
            let@ s = Storage.with_transaction in
            let* a = Accounts.get_account_by_id s id in
            match a with
            | None -> fail ()
            | Some a ->
                let () = Api_generic.invalidate_token token in
                let* token = Api_generic.new_token a u in
                let* () =
                  Eliom_reference.set Web_state.site_user (Some (u, a, token))
                in
                Redirection.send (Redirection home)))

  let () =
    Any.register ~service:api_token (fun () () ->
        let* x = Eliom_reference.get Web_state.site_user in
        let code, content =
          match x with
          | None -> (403, "Forbidden")
          | Some (_, _, token) -> (200, token)
        in
        String.send ~code (content, "text/plain"))

  type connect_code = {
    timeout : float;
    user_info : Belenios_web_api.user_info;
  }

  let connect_codes = ref SMap.empty

  let get_codes ~now =
    !connect_codes |> SMap.filter (fun _ { timeout; _ } -> now < timeout)

  let add_code ~now code x = connect_codes := get_codes ~now |> SMap.add code x

  let consume_code ~now code =
    let codes = get_codes ~now in
    let r = SMap.find_opt code codes in
    connect_codes := SMap.remove code codes;
    r

  let get_context ~server ~state =
    let url =
      let service =
        Eliom_service.extern ~prefix:server ~path:[ "get-context" ]
          ~meth:(Get Eliom_parameter.(string "state"))
          ()
      in
      let service = preapply ~service state in
      Eliom_uri.make_string_uri ~absolute:true ~service ()
    in
    let* x, body = Cohttp_lwt_unix.Client.get (Uri.of_string url) in
    let* context = Cohttp_lwt.Body.to_string body in
    match Cohttp.Code.code_of_status x.status with
    | 200 -> (
        match Belenios_web_api.connect_context_of_string context with
        | x -> Lwt.return_some x
        | exception _ -> Lwt.return_none)
    | _ -> Lwt.return_none

  let connect_auth ?uuid ~callback ~server ~state () =
    let* x = Eliom_reference.get Web_state.site_user in
    match x with
    | None ->
        Redirection.send
          (Redirection
             (preapply ~service:site_login
                (None, default_admin (ContSiteConnect (callback, state)))))
    | Some (_, account, _) ->
        let* () = Eliom_reference.unset Web_state.connect_context in
        let* page =
          Pages_admin.connect_consent ?uuid ~account ~callback ~server ~state ()
        in
        Html.send page

  let () =
    Any.register ~service:connect_login (fun (callback, state) () ->
        match List.assoc_opt callback !Web_config.connect with
        | None -> fail_http `Bad_request
        | Some server -> (
            let* context =
              let* x = Eliom_reference.get Web_state.connect_context in
              match x with
              | None ->
                  let* x = get_context ~server ~state in
                  let* () =
                    Eliom_reference.set Web_state.connect_context (Some x)
                  in
                  Lwt.return x
              | Some x -> Lwt.return x
            in
            match context with
            | None -> connect_auth ~callback ~server ~state ()
            | Some context -> (
                match context.kind with
                | `Site -> connect_auth ~callback ~server ~state ()
                | `Election uuid ->
                    connect_auth ~callback ~server ~state ~uuid ())))

  let () =
    Any.register ~service:connect_consent (fun (callback, state) () ->
        match List.assoc_opt callback !Web_config.connect with
        | None -> fail_http `Bad_request
        | Some server -> (
            let* x = Eliom_reference.get Web_state.site_user in
            match x with
            | None -> fail_http `Forbidden
            | Some (_, a, _) ->
                let user_info : Belenios_web_api.user_info =
                  {
                    login = string_of_int a.id;
                    name = Some a.name;
                    address = a.email;
                    timestamp = None;
                  }
                in
                let now = Unix.gettimeofday () in
                let timeout = now +. 60. in
                let code = Belenios_server_core.generate_token ~length:22 () in
                add_code ~now code { timeout; user_info };
                let service =
                  let service =
                    Eliom_service.extern ~prefix:server ~path:[ "return" ]
                      ~meth:
                        (Eliom_service.Get
                           Eliom_parameter.(string "code" ** string "state"))
                      ()
                  in
                  preapply ~service (code, state)
                in
                Redirection.send (Redirection service)))

  let () =
    Any.register ~service:connect_validate (fun code () ->
        let now = Unix.gettimeofday () in
        match consume_code ~now code with
        | Some { user_info; _ } ->
            String.send
              ( Belenios_web_api.string_of_user_info user_info,
                "application/json" )
        | _ -> fail_http `Forbidden)
end
