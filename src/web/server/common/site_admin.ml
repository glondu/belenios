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
open Belenios_server_core
open Web_common
module Pages_admin_root = Pages_admin

module Make
    (X : Pages_sig.S)
    (Site_common : Site_common_sig.S)
    (Web_cont : Web_cont_sig.S)
    (Web_auth : Web_auth_sig.S) =
struct
  open X
  open Web_services
  open Site_common
  open Eliom_service
  open Eliom_registration

  let get_preferred_gettext () = Web_i18n.get_preferred_gettext "admin"

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

  let without_site_user ?fallback () f =
    let* l = get_preferred_gettext () in
    let open (val l) in
    let* user = Eliom_reference.get Web_state.site_user in
    match user with
    | None -> f ()
    | Some u -> (
        match fallback with
        | Some g -> g u
        | None ->
            Pages_common.generic_page ~title:(s_ "Error")
              (s_
                 "This page is not accessible to authenticated administrators, \
                  because it is meant to be used by third parties.")
              ()
            >>= Html.send)

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
    Html.register ~service:home (fun () () ->
        let* site_user = Eliom_reference.get Web_state.site_user in
        match site_user with
        | None -> Pages_admin.admin_login Web_auth.get_site_login_handler
        | Some (_, a, _) ->
            let* show =
              match a.consent with
              | Some x
                when Datetime.to_unixfloat x >= !Web_config.tos_last_update ->
                  return_false
              | _ -> (
                  let@ s = Storage.with_transaction in
                  let* x = Accounts.update_account_by_id s a.id in
                  match x with
                  | None -> return_true
                  | Some (a, set) ->
                      let current_consent =
                        match a.consent with
                        | None -> 0.
                        | Some x -> Datetime.to_unixfloat x
                      in
                      if
                        current_consent < !Web_config.tos_last_update
                        && Web_state.get_consent_cookie ()
                      then return_true
                      else
                        let consent = Some (Datetime.now ()) in
                        let* () = set { a with consent } in
                        return_false)
            in
            if show then Pages_admin.privacy_notice ContAdmin
            else if a.email = None then Pages_admin.set_email ()
            else Pages_admin.admin ())

  module SetEmailSender = struct
    type payload = unit
    type context = unit

    let send ~context:() ~address ~code =
      let* l = get_preferred_gettext () in
      let subject, body = Pages_admin_root.mail_set_email l address code in
      send_email ~subject ~recipient:address ~body MailSetEmail
  end

  module SetEmailOtp = Otp.Make (SetEmailSender) ()

  let () =
    Any.register ~service:set_email_post (fun () address ->
        let@ _ = with_site_user in
        if is_email ~blacklist:!Web_config.blacklisted_domains address then
          let* () =
            Eliom_reference.set Web_state.set_email_env (Some address)
          in
          let* () = SetEmailOtp.generate ~context:() ~address ~payload:() in
          Pages_admin.set_email_confirm ~address >>= Html.send
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
                  let* x = Accounts.update_account_by_id s a.id in
                  match x with
                  | None -> Lwt.fail @@ Failure "set_email_confirm"
                  | Some x -> cont x
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
    Any.register ~service:election_regenpwd (fun uuid () ->
        Pages_admin.regenpwd uuid () >>= Html.send)

  let () =
    Any.register ~service:election_regenpwd_post (fun uuid user ->
        let@ s = Storage.with_transaction in
        let@ metadata = with_metadata_check_owner s uuid in
        let* l = get_preferred_gettext () in
        let open (val l) in
        let service = preapply ~service:election_admin uuid in
        let* b = Web_persist.regen_password s uuid metadata user in
        if b then
          Pages_common.generic_page ~title:(s_ "Success") ~service
            (Printf.sprintf (f_ "A new password has been mailed to %s.") user)
            ()
          >>= Html.send
        else
          Pages_common.generic_page ~title:(s_ "Error") ~service
            (Printf.sprintf
               (f_ "%s is not a registered user for this election.")
               user)
            ()
          >>= Html.send)

  let election_admin_handler ?shuffle_token ?tally_token s uuid =
    let* l = get_preferred_gettext () in
    let open (val l) in
    let@ election = with_election s uuid in
    let* metadata = Web_persist.get_election_metadata s uuid in
    let* site_user = Eliom_reference.get Web_state.site_user in
    match site_user with
    | Some (_, a, _) when Accounts.check a metadata.e_owners ->
        let* status = Api_elections.get_election_status s uuid in
        Pages_admin.election_admin ?shuffle_token ?tally_token s election
          metadata status ()
        >>= Html.send
    | Some _ ->
        let msg = s_ "You are not allowed to administer this election!" in
        Pages_common.generic_page ~title:(s_ "Forbidden") msg ()
        >>= Html.send ~code:403
    | _ ->
        redir_preapply site_login
          (None, default_admin (ContSiteElection uuid))
          ()

  let () =
    Any.register ~service:election_admin (fun uuid () ->
        let@ s = Storage.with_transaction in
        election_admin_handler s uuid)

  let election_set_state state uuid () =
    let@ s = Storage.with_transaction in
    let@ _ = with_metadata_check_owner s uuid in
    let set = Web_persist.(if state then open_election else close_election) in
    let* b = set s uuid in
    if b then redir_preapply election_admin uuid () else forbidden ()

  let () = Any.register ~service:election_open (election_set_state true)
  let () = Any.register ~service:election_close (election_set_state false)

  let election_set_result_hidden s uuid date =
    let@ _ = with_metadata_check_owner s uuid in
    let* dates = Web_persist.get_election_automatic_dates s uuid in
    let dates = { dates with auto_date_publish = date } in
    let* () = Web_persist.set_election_automatic_dates s uuid dates in
    redir_preapply election_admin uuid ()

  let parse_datetime_from_post l x =
    let open (val l : Belenios_ui.I18n.GETTEXT) in
    try Datetime.wrap x
    with _ -> Printf.ksprintf failwith (f_ "%s is not a valid date!") x

  let () =
    Any.register ~service:election_hide_result (fun uuid date ->
        let@ date cont =
          match Option.wrap Datetime.wrap date with
          | None ->
              let* l = get_preferred_gettext () in
              let open (val l) in
              let service = preapply ~service:election_admin uuid in
              let msg = Printf.sprintf (f_ "%s is not a valid date!") date in
              Pages_common.generic_page ~title:(s_ "Error") ~service msg ()
              >>= Html.send
          | Some t -> cont @@ Datetime.to_unixfloat t
        in
        let@ s = Storage.with_transaction in
        election_set_result_hidden s uuid (Some date))

  let () =
    Any.register ~service:election_show_result (fun uuid () ->
        let@ s = Storage.with_transaction in
        election_set_result_hidden s uuid None)

  let () =
    Any.register ~service:election_auto_post
      (fun uuid (auto_open, auto_close) ->
        let@ s = Storage.with_transaction in
        let@ _ = with_metadata_check_owner s uuid in
        let* l = get_preferred_gettext () in
        let open (val l) in
        let auto_dates =
          try
            let format x =
              if x = "" then None else Some (parse_datetime_from_post l x)
            in
            Ok (format auto_open, format auto_close)
          with Failure e -> Error e
        in
        match auto_dates with
        | Ok (e_auto_open, e_auto_close) ->
            let* dates = Web_persist.get_election_automatic_dates s uuid in
            let dates =
              {
                dates with
                auto_date_open = Option.map Datetime.to_unixfloat e_auto_open;
                auto_date_close = Option.map Datetime.to_unixfloat e_auto_close;
              }
            in
            let* () = Web_persist.set_election_automatic_dates s uuid dates in
            redir_preapply election_admin uuid ()
        | Error msg ->
            let service = preapply ~service:election_admin uuid in
            Pages_common.generic_page ~title:(s_ "Error") ~service msg ()
            >>= Html.send)

  let () =
    Any.register ~service:election_delete (fun uuid () ->
        let@ s = Storage.with_transaction in
        let@ _ = with_metadata_check_owner s uuid in
        let* () = Web_persist.delete_election s uuid in
        redir_preapply home () ())

  let () =
    let rex = Re.Pcre.regexp "\".*\" \".*:(.*)\"" in
    Any.register ~service:election_missing_voters (fun (uuid, ()) () ->
        let@ s = Storage.with_transaction in
        let@ _ = with_metadata_check_owner s uuid in
        let* voters = Web_persist.get_all_voters s uuid in
        let voters =
          List.fold_left
            (fun accu x ->
              let _, login, _ = Voter.get x in
              SMap.add (Stdlib.String.lowercase_ascii login) x accu)
            SMap.empty voters
        in
        let* voters =
          let* file = Web_persist.get_records s uuid in
          match file with
          | Some rs ->
              return
                (List.fold_left
                   (fun accu r ->
                     let s = Re.Pcre.exec ~rex r in
                     let v = Re.Pcre.get_substring s 1 in
                     SMap.remove (Stdlib.String.lowercase_ascii v) accu)
                   voters rs)
          | None -> return voters
        in
        let buf = Buffer.create 128 in
        SMap.iter
          (fun v _ ->
            Buffer.add_string buf v;
            Buffer.add_char buf '\n')
          voters;
        let* x = String.send (Buffer.contents buf, "text/plain") in
        return @@ Eliom_registration.cast_unknown_content_kind x)

  let () =
    Any.register ~service:election_pretty_records (fun (uuid, ()) () ->
        let@ s = Storage.with_transaction in
        let@ _ = with_metadata_check_owner s uuid in
        let@ election = with_election s uuid in
        let* records = Api_elections.get_records s uuid in
        Pages_admin.pretty_records s election records () >>= Html.send)

  let () =
    Any.register ~service:election_project_result (fun ((uuid, ()), index) () ->
        if index < 0 then fail_http `Not_found
        else
          let@ s = Storage.with_transaction in
          let* hidden =
            let* dates = Web_persist.get_election_automatic_dates s uuid in
            match dates.auto_date_publish with
            | None -> return_false
            | Some _ -> return_true
          in
          let* allow =
            if hidden then
              let* metadata = Web_persist.get_election_metadata s uuid in
              let* site_user = Eliom_reference.get Web_state.site_user in
              match site_user with
              | Some (_, a, _) when Accounts.check a metadata.e_owners ->
                  return_true
              | _ -> return_false
            else return_true
          in
          if allow then
            let* result = Public_archive.get_result s uuid in
            match result with
            | None -> fail_http `Not_found
            | Some result -> (
                let result =
                  election_result_of_string Yojson.Safe.read_json result
                in
                match result.result with
                | `List xs -> (
                    match List.nth_opt xs index with
                    | None -> fail_http `Not_found
                    | Some x ->
                        let* x =
                          String.send
                            (Yojson.Safe.to_string x, "application/json")
                        in
                        return @@ Eliom_registration.cast_unknown_content_kind x
                    )
                | _ -> fail_http `Not_found)
          else forbidden ())

  let () =
    Any.register ~service:election_download_archive (fun (uuid, ()) () ->
        let@ s = Storage.with_transaction in
        let@ _ = with_metadata_check_owner s uuid in
        let* l = get_preferred_gettext () in
        let open (val l) in
        Lwt.try_bind
          (fun () ->
            let module S = (val s) in
            S.get_as_file (Election (uuid, Confidential_archive)))
          (fun archive_name ->
            File.send ~content_type:"application/zip" archive_name)
          (function
            | Not_found ->
                let service = preapply ~service:election_admin uuid in
                Pages_common.generic_page ~title:(s_ "Error") ~service
                  (s_ "The election is not archived!")
                  ()
                >>= Html.send
            | e -> Lwt.reraise e))

  let find_trustee_id s uuid token =
    let* x = Spool.get s uuid Spool.decryption_tokens in
    match x with
    | None -> return (int_of_string_opt token)
    | Some tokens ->
        let rec find i = function
          | [] -> None
          | t :: ts -> if t = token then Some i else find (i + 1) ts
        in
        return (find 1 tokens)

  let () =
    Any.register ~service:election_tally_trustees (fun (uuid, token) () ->
        let@ s = Storage.with_transaction in
        let@ () =
          without_site_user
            ~fallback:(fun _ ->
              election_admin_handler ~tally_token:token s uuid)
            ()
        in
        let* l = get_preferred_gettext () in
        let open (val l) in
        let* state = Web_persist.get_election_state s uuid in
        match state with
        | `EncryptedTally -> (
            let* x = find_trustee_id s uuid token in
            match x with
            | Some trustee_id ->
                let* pds = Public_archive.get_partial_decryptions s uuid in
                if List.exists (fun x -> x.owned_owner = trustee_id) pds then
                  Pages_common.generic_page ~title:(s_ "Error")
                    (s_
                       "Your partial decryption has already been received and \
                        checked!")
                    ()
                  >>= Html.send
                else
                  make_trustee_link uuid (`Decrypt token)
                  |> String_redirection.send
            | None -> forbidden ())
        | `Open | `Closed | `Shuffling ->
            let msg =
              s_
                "The election is not ready to be tallied. Please come back \
                 later."
            in
            Pages_common.generic_page ~title:(s_ "Forbidden") msg ()
            >>= Html.send ~code:403
        | `Tallied | `Archived ->
            let msg = s_ "The election has already been tallied." in
            Pages_common.generic_page ~title:(s_ "Forbidden") msg ()
            >>= Html.send ~code:403)

  let handle_election_tally_release uuid () =
    let@ s = Storage.with_transaction in
    let@ _ = with_metadata_check_owner s uuid in
    let* l = get_preferred_gettext () in
    let open (val l) in
    Lwt.catch
      (fun () ->
        let* () = Web_persist.release_tally s uuid in
        redir_preapply election_home_redirect (uuid, ()) ())
      (fun e ->
        let msg =
          Printf.sprintf
            (f_
               "An error occurred while computing the result (%s). Most \
                likely, it means that some trustee has not done his/her job.")
            (Printexc.to_string e)
        in
        Pages_common.generic_page ~title:(s_ "Error") msg () >>= Html.send)

  let () =
    Any.register ~service:election_tally_release handle_election_tally_release

  let () =
    Any.register ~service:election_compute_encrypted_tally (fun uuid () ->
        let@ s = Storage.with_transaction in
        let@ _ = with_metadata_check_owner s uuid in
        let* _ = Web_persist.compute_encrypted_tally s uuid in
        redir_preapply election_admin uuid ())

  let () =
    Any.register ~service:election_shuffle_link (fun (uuid, token) () ->
        let@ s = Storage.with_transaction in
        let@ () =
          without_site_user
            ~fallback:(fun _ ->
              election_admin_handler ~shuffle_token:token s uuid)
            ()
        in
        let* expected_token = Web_persist.get_shuffle_token s uuid in
        match expected_token with
        | Some x when token = x.tk_token ->
            make_trustee_link uuid (`Shuffle token) |> String_redirection.send
        | _ -> forbidden ())

  let () =
    Any.register ~service:election_shuffler_select (fun () (uuid, trustee) ->
        let@ s = Storage.with_transaction in
        let@ metadata = with_metadata_check_owner s uuid in
        let* () = Api_elections.select_shuffler s uuid metadata trustee in
        redir_preapply election_admin uuid ())

  let () =
    Any.register ~service:election_shuffler_skip_confirm
      (fun () (uuid, trustee) ->
        let@ s = Storage.with_transaction in
        let@ _ = with_metadata_check_owner s uuid in
        Pages_admin.election_shuffler_skip_confirm uuid trustee >>= Html.send)

  let () =
    Any.register ~service:election_shuffler_skip (fun () (uuid, trustee) ->
        let@ s = Storage.with_transaction in
        let@ _ = with_metadata_check_owner s uuid in
        let* () = Api_elections.skip_shuffler s uuid trustee in
        redir_preapply election_admin uuid ())

  let () =
    Any.register ~service:election_decrypt (fun uuid () ->
        let@ s = Storage.with_transaction in
        let@ _ = with_metadata_check_owner s uuid in
        let* _ = Web_persist.finish_shuffling s uuid in
        redir_preapply election_admin uuid ())

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
            let* () = Web_signup.send_confirmation_code l ~service email in
            let* () =
              Eliom_reference.set Web_state.signup_address (Some email)
            in
            Pages_admin.signup_login ()
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
            let* () =
              let* x =
                Web_auth_password.lookup_account ~service ~email ~username
              in
              match x with
              | Some (username, Some address) ->
                  let* () =
                    Eliom_reference.set Web_state.signup_address (Some address)
                  in
                  Web_signup.send_changepw_code l ~service ~address ~username
              | _ ->
                  return
                    (Printf.ksprintf Ocsigen_messages.warning
                       "Unsuccessful attempt to change the password of %S (%S) \
                        for service %s"
                       username email service)
            in
            Pages_admin.signup_login ()
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
                let* token = Api_generic.new_token a in
                let* () =
                  Eliom_reference.set Web_state.site_user (Some (u, a, token))
                in
                Redirection.send (Redirection home)))

  let with_user_and_account f =
    let* x = Eliom_reference.get Web_state.site_user in
    match x with Some x -> f x | None -> forbidden ()

  let () =
    Any.register ~service:account (fun () () ->
        let@ _, a, _ = with_user_and_account in
        Pages_admin.account a >>= Html.send)

  let () =
    Any.register ~service:account_post (fun () name ->
        let@ _, a, _ = with_user_and_account in
        let@ s = Storage.with_transaction in
        let@ a, set =
         fun cont ->
          let* x = Accounts.update_account_by_id s a.id in
          match x with
          | None -> Lwt.fail @@ Failure "account_post"
          | Some x -> cont x
        in
        let* () = set { a with name } in
        Redirection.send (Redirection home))

  let () =
    Any.register ~service:api_token (fun () () ->
        let* x = Eliom_reference.get Web_state.site_user in
        let code, content =
          match x with
          | None -> (403, "Forbidden")
          | Some (_, _, token) -> (200, token)
        in
        String.send ~code (content, "text/plain"))

  let process_election_for_data_policy (action, uuid, next_t) =
    let uuid_s = Uuid.unwrap uuid in
    let now = Datetime.now () in
    let action, comment =
      match action with
      | `Destroy -> (Web_persist.delete_draft, "destroyed")
      | `Delete -> (Web_persist.delete_election, "deleted")
      | `Archive -> (Web_persist.archive_election, "archived")
    in
    let@ s = Storage.with_transaction in
    if Datetime.compare now next_t > 0 then
      let* () = action s uuid in
      return
        (Printf.ksprintf Ocsigen_messages.warning
           "Election %s has been automatically %s" uuid_s comment)
    else return_unit

  let rec data_policy_loop () =
    let open Ocsigen_messages in
    let () = accesslog "Data policy process started" in
    let* elections = Storage.get_next_actions () in
    let* () = Lwt_list.iter_s process_election_for_data_policy elections in
    let () = accesslog "Data policy process completed" in
    let* () = sleep 3600. in
    data_policy_loop ()
end
