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
    Any.register ~service:home (fun () () ->
        Redirection.send (Redirection admin))

  let get_elections_by_owner_sorted u =
    let* elections = Storage.get_elections_by_owner u in
    let filter f =
      let open Belenios_api.Serializable_t in
      List.filter (fun ({ state; _ } : summary) -> f state) elections
    in
    let draft = filter (fun x -> x = `Draft) in
    let elections =
      filter (function
        | `Open | `Closed | `Shuffling | `EncryptedTally -> true
        | _ -> false)
    in
    let tallied = filter (fun x -> x = `Tallied) in
    let archived = filter (fun x -> x = `Archived) in
    let sort l =
      let open Belenios_api.Serializable_t in
      List.sort
        (fun ({ date = x; _ } : summary) { date = y; _ } -> Float.compare x y)
        l
    in
    return (sort draft, sort elections, sort tallied, sort archived)

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
          | ContAdmin -> Redirection admin
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
    Html.register ~service:admin (fun () () ->
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
            else
              let* elections = get_elections_by_owner_sorted a.id in
              Pages_admin.admin ~elections)

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
                Redirection.send (Redirection admin)
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

  let create_new_election (account : account) cred draft_authentication =
    let open Belenios_api.Serializable_t in
    let (Version v) = List.hd Belenios.Election.supported_crypto_versions in
    let open (val Belenios.Election.get_serializers v) in
    let draft_questions =
      {
        t_description = Defaults.description;
        t_name = Defaults.name;
        t_questions = Array.map of_concrete Defaults.questions;
        t_administrator = Some account.name;
        t_credential_authority =
          Some (match cred with `Automatic -> "server" | `Manual -> "");
      }
    in
    let address =
      match account.email with None -> "" | Some x -> Printf.sprintf " <%s>" x
    in
    let draft =
      {
        draft_version = Defaults.version;
        draft_owners = [ account.id ];
        draft_questions;
        draft_languages = [ "en"; "fr" ];
        draft_contact = Some (account.name ^ address);
        draft_booth = 2;
        draft_authentication;
        draft_group = !Web_config.default_group;
      }
    in
    let* uuid =
      let@ s = Storage.with_transaction in
      Api_drafts.post_drafts account s (Belenios_api.Common.Draft (v, draft))
    in
    match uuid with
    | Some uuid -> redir_preapply election_draft uuid ()
    | None ->
        let* l = get_preferred_gettext () in
        let open (val l) in
        Pages_common.generic_page ~title:(s_ "Error")
          (s_ "Creating new elections is forbidden on this server!")
          ()
        >>= Html.send

  let () =
    Any.register ~service:election_draft_pre (fun () () ->
        let@ _ = with_site_user in
        Pages_admin.election_draft_pre () >>= Html.send)

  let http_rex = "^https?://[a-z0-9/.-]+$"

  let is_http_url =
    let rex = Re.Pcre.regexp ~flags:[ `CASELESS ] http_rex in
    fun x -> match re_exec_opt ~rex x with Some _ -> true | None -> false

  let () =
    Any.register ~service:election_draft_new
      (fun () (credmgmt, (auth, cas_server)) ->
        let* l = get_preferred_gettext () in
        let open (val l) in
        let@ _, a, _ = with_site_user in
        let* credmgmt =
          match credmgmt with
          | Some "auto" -> return `Automatic
          | Some "manual" -> return `Manual
          | _ -> fail_http `Bad_request
        in
        let* auth_parsed =
          match auth with
          | Some "password" -> return `Password
          | Some "cas" -> (
              match cas_server with
              | None -> fail_http `Bad_request
              | Some cas_server ->
                  return @@ `CAS (Stdlib.String.trim cas_server))
          | Some x ->
              let n = Stdlib.String.length x in
              if n > 1 && Stdlib.String.get x 0 = '%' then
                let name = Stdlib.String.sub x 1 (n - 1) in
                return @@ `Configured name
              else fail_http `Bad_request
          | _ -> fail_http `Bad_request
        in
        let has_cas_server =
          match cas_server with None | Some "" -> false | Some _ -> true
        in
        match auth_parsed with
        | `CAS cas_server when not (is_http_url cas_server) ->
            Pages_common.generic_page ~title:(s_ "Error") (s_ "Bad CAS server!")
              ()
            >>= Html.send
        | _ when has_cas_server && auth <> Some "cas" ->
            Pages_common.generic_page ~title:(s_ "Error")
              (s_ "Non-empty CAS server, but CAS authentication not selected!")
              ()
            >>= Html.send
        | _ -> create_new_election a credmgmt auth_parsed)

  let with_draft_public s uuid f =
    let* election = Spool.get s uuid Spool.draft in
    match election with None -> fail_http `Not_found | Some x -> f x

  let with_draft_public_update s uuid f =
    let* election = Spool.update s uuid Spool.draft in
    match election with None -> fail_http `Not_found | Some x -> f x

  let with_draft ?(save = true) s uuid f =
    let@ _, a, _ = with_site_user in
    let@ (Draft (_, se) as x), set =
     fun cont ->
      if save then
        let* x = Spool.update s uuid Spool.draft in
        match x with
        | None -> fail_http `Not_found
        | Some (x, set) -> cont (x, Some set)
      else
        let* x = Spool.get s uuid Spool.draft in
        match x with None -> fail_http `Not_found | Some x -> cont (x, None)
    in
    if Accounts.check a se.se_owners then
      Lwt.catch
        (fun () ->
          let* r = f x in
          let* () =
            match set with None -> Lwt.return_unit | Some set -> set x
          in
          return r)
        (fun e ->
          let msg = match e with Failure s -> s | _ -> Printexc.to_string e in
          let service = preapply ~service:election_draft uuid in
          let* l = get_preferred_gettext () in
          let open (val l) in
          Pages_common.generic_page ~title:(s_ "Error") ~service msg ()
          >>= Html.send)
    else forbidden ()

  let with_draft_update ?(save = true) s uuid f =
    let@ _, a, _ = with_site_user in
    let@ (((Draft (_, se) as x), set) as d) =
     fun cont ->
      let* x = Spool.update s uuid Spool.draft in
      match x with None -> fail_http `Not_found | Some x -> cont x
    in
    if Accounts.check a se.se_owners then
      Lwt.catch
        (fun () ->
          let* r = f d in
          let* () = if save then set x else return_unit in
          return r)
        (fun e ->
          let msg = match e with Failure s -> s | _ -> Printexc.to_string e in
          let service = preapply ~service:election_draft uuid in
          let* l = get_preferred_gettext () in
          let open (val l) in
          Pages_common.generic_page ~title:(s_ "Error") ~service msg ()
          >>= Html.send)
    else forbidden ()

  let () =
    Any.register ~service:election_draft (fun uuid () ->
        let@ s = Storage.with_transaction in
        let@ se = with_draft ~save:false s uuid in
        Pages_admin.election_draft uuid se () >>= Html.send)

  let () =
    Any.register ~service:election_draft_trustees (fun uuid () ->
        let@ s = Storage.with_transaction in
        let@ (Draft (_, se) as x) = with_draft ~save:false s uuid in
        match se.se_trustees with
        | `Basic _ ->
            Pages_admin.election_draft_trustees uuid x () >>= Html.send
        | `Threshold _ ->
            redir_preapply election_draft_threshold_trustees uuid ())

  let () =
    Any.register ~service:election_draft_threshold_trustees (fun uuid () ->
        let@ s = Storage.with_transaction in
        let@ se = with_draft ~save:false s uuid in
        Pages_admin.election_draft_threshold_trustees uuid se () >>= Html.send)

  let () =
    Any.register ~service:election_draft_credential_authority (fun uuid () ->
        let@ s = Storage.with_transaction in
        let@ se = with_draft ~save:false s uuid in
        Pages_admin.election_draft_credential_authority uuid se () >>= Html.send)

  let () =
    Any.register ~service:election_draft_set_credential_authority
      (fun uuid name ->
        let@ s = Storage.with_transaction in
        let@ (Draft (_, se)) = with_draft s uuid in
        let* l = get_preferred_gettext () in
        let open (val l) in
        let service =
          Eliom_service.preapply ~service:election_draft_credential_authority
            uuid
        in
        match
          if se.se_metadata.e_cred_authority = Some "server" then
            Error
              (s_ "You cannot set the credential authority for this election!")
          else
            match name with
            | "" -> Ok None
            | "server" ->
                Error (s_ "Invalid public name for credential authority!")
            | x -> Ok (Some x)
        with
        | Ok e_cred_authority ->
            se.se_metadata <- { se.se_metadata with e_cred_authority };
            let msg =
              s_
                "The public name of the credential authority has been set \
                 successfully!"
            in
            Pages_common.generic_page ~title:(s_ "Success") ~service msg ()
            >>= Html.send
        | Error msg ->
            Pages_common.generic_page ~title:(s_ "Error") ~service msg ()
            >>= Html.send)

  let () =
    Any.register ~service:election_draft_languages (fun uuid languages ->
        let@ s = Storage.with_transaction in
        let@ (Draft (_, se)) = with_draft s uuid in
        let* l = get_preferred_gettext () in
        let open (val l) in
        let langs = languages_of_string languages in
        match langs with
        | [] ->
            let service = preapply ~service:election_draft uuid in
            Pages_common.generic_page ~title:(s_ "Error") ~service
              (s_ "You must select at least one language!")
              ()
            >>= Html.send
        | _ :: _ -> (
            let available_languages =
              List.map fst Belenios_ui.Languages.available
            in
            let unavailable =
              List.filter (fun x -> not (List.mem x available_languages)) langs
            in
            match unavailable with
            | [] ->
                se.se_metadata <-
                  { se.se_metadata with e_languages = Some langs };
                redir_preapply election_draft uuid ()
            | l :: _ ->
                let service = preapply ~service:election_draft uuid in
                Pages_common.generic_page ~title:(s_ "Error") ~service
                  (Printf.sprintf (f_ "No such language: %s") l)
                  ()
                >>= Html.send))

  let () =
    Any.register ~service:election_draft_contact (fun uuid contact ->
        let@ s = Storage.with_transaction in
        let@ (Draft (_, se)) = with_draft s uuid in
        let contact =
          if contact = "" || contact = Defaults.contact then None
          else Some contact
        in
        se.se_metadata <- { se.se_metadata with e_contact = contact };
        redir_preapply election_draft uuid ())

  let () =
    Any.register ~service:election_draft_admin_name (fun uuid name ->
        let@ s = Storage.with_transaction in
        let@ (Draft (_, se)) = with_draft s uuid in
        let administrator = if name = "" then None else Some name in
        se.se_administrator <- administrator;
        redir_preapply election_draft uuid ())

  let () =
    Any.register ~service:election_draft_description
      (fun uuid (name, description) ->
        let@ s = Storage.with_transaction in
        let@ (Draft (_, se)) = with_draft s uuid in
        let* l = get_preferred_gettext () in
        let open (val l) in
        if Stdlib.String.length name > Defaults.max_election_name_size then
          let msg =
            Printf.sprintf
              (f_ "The election name must be %d characters or less!")
              Defaults.max_election_name_size
          in
          Pages_common.generic_page ~title:(s_ "Error") msg () >>= Html.send
        else (
          se.se_questions <-
            { se.se_questions with t_name = name; t_description = description };
          redir_preapply election_draft uuid ()))

  let handle_password account se uuid ~force voters =
    let* l = get_preferred_gettext () in
    let open (val l) in
    let max_voters = Accounts.max_voters account in
    if List.length voters > max_voters then
      Lwt.fail
        (Failure
           (Printf.sprintf
              (f_ "Cannot send passwords, there are too many voters (max is %d)")
              max_voters))
    else if se.se_questions.t_name = Defaults.name then
      Lwt.fail (Failure (s_ "The election name has not been edited!"))
    else
      let title = se.se_questions.t_name in
      let langs = get_languages se.se_metadata.e_languages in
      let show_weight = has_explicit_weights voters in
      let* jobs =
        Lwt_list.fold_left_s
          (fun jobs id ->
            match id.sv_password with
            | Some _ when not force -> Lwt.return jobs
            | None | Some _ ->
                let* email, x =
                  Mails_voter.generate_password_email se.se_metadata langs title
                    uuid id.sv_id show_weight
                in
                id.sv_password <- Some x;
                Lwt.return (email :: jobs))
          [] voters
      in
      let* () = Mails_voter.submit_bulk_emails jobs in
      let service = preapply ~service:election_draft uuid in
      Pages_common.generic_page ~title:(s_ "Success") ~service
        (s_ "Passwords have been generated and mailed!")
        ()
      >>= Html.send

  let () =
    Any.register ~service:election_draft_auth_genpwd (fun uuid () ->
        let@ s = Storage.with_transaction in
        let@ (Draft (_, se)) = with_draft s uuid in
        let@ _, account, _ = with_site_user in
        handle_password account se uuid ~force:false se.se_voters)

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

  let () =
    Any.register ~service:election_draft_questions (fun uuid () ->
        let@ s = Storage.with_transaction in
        let@ se = with_draft ~save:false s uuid in
        Pages_admin.election_draft_questions uuid se () >>= Html.send)

  let () =
    Any.register ~service:election_draft_questions_post
      (fun uuid (template, booth_version) ->
        let@ s = Storage.with_transaction in
        let@ (Draft (v, se) as x) = with_draft s uuid in
        let* l = get_preferred_gettext () in
        let open (val l) in
        let open (val Election.get_serializers v) in
        let template = template_of_string read_question template in
        let fixed_group = Web_persist.is_group_fixed uuid x in
        (match
           ( Election.has_nh_questions (Template (v, se.se_questions)),
             Election.has_nh_questions (Template (v, template)) )
         with
        | true, true | false, false -> ()
        | true, false when fixed_group -> ()
        | true, false -> se.se_group <- !Web_config.default_group
        | false, true when fixed_group ->
            failwith (s_ "This kind of change is not allowed now!")
        | false, true -> se.se_group <- !Web_config.nh_group);
        se.se_questions <- template;
        let e_booth_version =
          match booth_version with 1 -> None | x -> Some x
        in
        se.se_metadata <- { se.se_metadata with e_booth_version };
        redir_preapply election_draft uuid ())

  let () =
    Any.register ~service:election_draft_preview (fun (uuid, ()) () ->
        let@ s = Storage.with_transaction in
        let@ (Draft (v, se)) = with_draft ~save:false s uuid in
        let version = se.se_version in
        let group = se.se_group in
        let module G = (val Group.of_string ~version group : GROUP) in
        let public_key = G.to_string G.g in
        let raw_election =
          Election.make_raw_election ~version
            (Template (v, se.se_questions))
            ~uuid ~group ~public_key
        in
        let* x = String.send (raw_election, "application/json") in
        return @@ Eliom_registration.cast_unknown_content_kind x)

  let () =
    Any.register ~service:election_draft_voters (fun uuid () ->
        let@ s = Storage.with_transaction in
        let@ se = with_draft ~save:false s uuid in
        let@ _, account, _ = with_site_user in
        let max_voters = Accounts.max_voters account in
        Pages_admin.election_draft_voters uuid se max_voters () >>= Html.send)

  let check_consistency voters =
    let get_shape voter =
      match voter.sv_id with
      | `Plain, { login; weight; _ } -> `Plain (login <> None, weight <> None)
      | `Json, _ -> `Json
    in
    match voters with
    | [] -> true
    | voter :: voters ->
        let shape = get_shape voter in
        let rec loop = function
          | [] -> true
          | voter :: voters -> get_shape voter = shape && loop voters
        in
        loop voters

  let () =
    Any.register ~service:election_draft_voters_add (fun uuid voters ->
        let@ s = Storage.with_transaction in
        let@ (Draft (_, se) as x) = with_draft s uuid in
        let@ _, account, _ = with_site_user in
        let* l = get_preferred_gettext () in
        let open (val l) in
        match Web_persist.get_credentials_status uuid x with
        | `Done | `Pending _ -> forbidden ()
        | `None -> (
            let voters =
              split_lines voters
              |> List.map (fun x ->
                     match Voter.of_string x with
                     | exception _ ->
                         Printf.ksprintf failwith
                           (f_ "%S is not a valid identity")
                           x
                     | voter ->
                         if not (Voter.validate voter) then
                           Printf.ksprintf failwith
                             (f_ "%S is not a valid identity")
                             x;
                         voter)
            in
            match
              Api_drafts.merge_voters se.se_voters voters (fun _ -> None)
            with
            | Error x ->
                let _, x, _ = Voter.get x in
                Printf.ksprintf failwith
                  (f_
                     "Duplicate voter: %s. This is not allowed. If two voters \
                      have the same address, use different logins.")
                  x
            | Ok (voters, total_weight) ->
                let max_voters = Accounts.max_voters account in
                let () =
                  let expanded =
                    Weight.expand ~total:total_weight total_weight
                  in
                  if Z.compare expanded Weight.max_expanded_weight > 0 then
                    Printf.ksprintf failwith
                      (f_
                         "The total weight (%s) cannot be handled. Its \
                          expanded value must be less than %s.")
                      Weight.(to_string total_weight)
                      (Z.to_string Weight.max_expanded_weight)
                in
                if not (check_consistency voters) then
                  failwith
                    (s_
                       "The voter list is not consistent (a login or a weight \
                        is missing).");
                let uses_password_auth =
                  match se.se_metadata.e_auth_config with
                  | Some configs ->
                      List.exists
                        (fun { auth_system; _ } -> auth_system = "password")
                        configs
                  | None -> false
                in
                let cred_auth_is_server =
                  se.se_metadata.e_cred_authority = Some "server"
                in
                if
                  (uses_password_auth || cred_auth_is_server)
                  && List.length voters > max_voters
                then
                  Lwt.fail
                    (Failure
                       (Printf.sprintf
                          (f_ "There are too many voters (max is %d)")
                          max_voters))
                else (
                  se.se_voters <- voters;
                  redir_preapply election_draft_voters uuid ())))

  let () =
    Any.register ~service:election_draft_voters_remove (fun uuid voter ->
        let@ s = Storage.with_transaction in
        let@ (Draft (_, se) as x) = with_draft s uuid in
        match Web_persist.get_credentials_status uuid x with
        | `Done | `Pending _ -> forbidden ()
        | `None ->
            let filter v =
              let _, login, _ = Voter.get v.sv_id in
              login <> voter
            in
            se.se_voters <- List.filter filter se.se_voters;
            redir_preapply election_draft_voters uuid ())

  let () =
    Any.register ~service:election_draft_voters_remove_all (fun uuid () ->
        let@ s = Storage.with_transaction in
        let@ (Draft (_, se) as x) = with_draft s uuid in
        match Web_persist.get_credentials_status uuid x with
        | `Done | `Pending _ -> forbidden ()
        | `None ->
            se.se_voters <- [];
            redir_preapply election_draft_voters uuid ())

  let () =
    Any.register ~service:election_draft_voters_passwd (fun uuid voter ->
        let@ s = Storage.with_transaction in
        let@ (Draft (_, se)) = with_draft s uuid in
        let@ _, account, _ = with_site_user in
        let filter v =
          let _, login, _ = Voter.get v.sv_id in
          login = voter
        in
        let voter = List.filter filter se.se_voters in
        handle_password account se uuid ~force:true voter)

  let ensure_trustees_mode s uuid ((Draft (_, se) as x), set) mode =
    match (se.se_trustees, mode) with
    | `Basic _, `Basic | `Threshold _, `Threshold _ -> Lwt.return x
    | `Threshold _, `Basic | `Basic _, `Threshold _ -> (
        let* () = Api_drafts.put_draft_trustees_mode (x, set) mode in
        let* x = Spool.get s uuid Spool.draft in
        match x with
        | Some se -> Lwt.return se
        | None -> Lwt.fail (Failure "inconsistency in ensure_trustees_mode"))

  let handle_trustee_add mode uuid (trustee_address, trustee_name) =
    let@ s = Storage.with_transaction in
    let@ se, set = with_draft_update ~save:false s uuid in
    let* l = get_preferred_gettext () in
    let open (val l) in
    if is_email trustee_address then
      let* se = ensure_trustees_mode s uuid (se, set) mode in
      let open Belenios_api.Serializable_t in
      let trustee =
        {
          trustee_address = Some trustee_address;
          trustee_name;
          trustee_token = None;
          trustee_state = None;
          trustee_key = None;
        }
      in
      let* () = Api_drafts.post_draft_trustees (se, set) trustee in
      redir_preapply election_draft_trustees uuid ()
    else
      let msg =
        Printf.sprintf (f_ "%s is not a valid e-mail address!") trustee_address
      in
      let service = preapply ~service:election_draft_trustees uuid in
      Pages_common.generic_page ~title:(s_ "Error") ~service msg ()
      >>= Html.send

  let handle_trustee_del service uuid address =
    let@ s = Storage.with_transaction in
    let@ se = with_draft_update ~save:false s uuid in
    let* _ = Api_drafts.delete_draft_trustee se address in
    redir_preapply service uuid ()

  let () =
    Any.register ~service:election_draft_trustee_add (handle_trustee_add `Basic)

  let () =
    Any.register ~service:election_draft_trustee_del
      (handle_trustee_del election_draft_trustees)

  let () =
    Any.register ~service:election_draft_credentials (fun (uuid, token) () ->
        let@ () = without_site_user () in
        let@ s = Storage.with_transaction in
        let@ se = with_draft_public s uuid in
        match Web_persist.get_credentials_status uuid se with
        | `Done | `Pending _ ->
            Pages_admin.election_draft_credentials_already_generated ()
            >>= Html.send
        | `None ->
            Printf.sprintf "%s/draft/credentials.html#%s-%s" !Web_config.prefix
              (Uuid.unwrap uuid) token
            |> String_redirection.send)

  let () =
    Html.register ~service:election_draft_credentials_static (fun () () ->
        Pages_admin.election_draft_credentials_static ())

  let handle_credentials_post uuid token creds =
    let@ s = Storage.with_transaction in
    let@ (Draft (_, se) as x), set = with_draft_public_update s uuid in
    if se.se_public_creds <> token then forbidden ()
    else
      match Web_persist.get_credentials_status uuid x with
      | `Done | `Pending _ -> forbidden ()
      | `None ->
          let creds = public_credentials_of_string creds in
          let* () =
            Api_drafts.submit_public_credentials s uuid (x, set) creds
          in
          Pages_admin.election_draft_credentials_done x () >>= Html.send

  let () =
    Any.register ~service:election_draft_credentials_post
      (fun (uuid, token) creds ->
        let@ () = without_site_user () in
        wrap_handler (fun () -> handle_credentials_post uuid token creds))

  let () =
    Any.register ~service:election_draft_credentials_post_file
      (fun (uuid, token) creds ->
        let@ () = without_site_user () in
        let* creds = exhaust_file creds in
        wrap_handler (fun () -> handle_credentials_post uuid token creds))

  let () =
    Any.register ~service:election_draft_credentials_server (fun uuid () ->
        let@ s = Storage.with_transaction in
        let@ (Draft (_, se) as x) = with_draft s uuid in
        let@ _, account, _ = with_site_user in
        let max_voters = Accounts.max_voters account in
        let* l = get_preferred_gettext () in
        let open (val l) in
        if se.se_questions.t_name = Defaults.name then
          Lwt.fail (Failure (s_ "The election name has not been edited!"))
        else
          let* x = Api_drafts.generate_credentials_on_server account uuid x in
          match x with
          | Ok () ->
              let service = preapply ~service:election_draft uuid in
              Pages_common.generic_page ~title:(s_ "Success") ~service
                (s_
                   "Credentials have been generated! They will be e-mailed \
                    when the election is validated. You should download \
                    private credentials (and store them in a secure location), \
                    in case someone loses their credential.")
                ()
              >>= Html.send
          | Error `Already ->
              Lwt.fail (Failure (s_ "The credentials were already sent"))
          | Error `NoVoters -> Lwt.fail (Failure (s_ "No voters"))
          | Error `TooManyVoters ->
              Lwt.fail
                (Failure
                   (Printf.sprintf
                      (f_
                         "Cannot send credentials, there are too many voters \
                          (max is %d)")
                      max_voters))
          | Error `NoServer ->
              Lwt.fail (Failure (s_ "The authority is not the server")))

  let () =
    Any.register ~service:election_draft_credentials_get (fun uuid () ->
        let@ s = Storage.with_transaction in
        let@ _ = with_draft ~save:false s uuid in
        let* () = Web_persist.set_private_creds_downloaded s uuid in
        let* filename =
          let module S = (val s) in
          S.get_as_file (Election (uuid, Private_creds))
        in
        File.send ~content_type:"text/plain" filename)

  let () =
    Any.register ~service:election_draft_trustee (fun (uuid, token) () ->
        let* l = get_preferred_gettext () in
        let open (val l) in
        let@ s = Storage.with_transaction in
        let@ () =
          without_site_user
            ~fallback:(fun (_, a, _) ->
              let@ (Draft (_, se) as x) = with_draft_public s uuid in
              if Accounts.check a se.se_owners then
                Pages_admin.election_draft_trustees ~token uuid x ()
                >>= Html.send
              else forbidden ())
            ()
        in
        let@ (Draft (_, se)) = with_draft_public s uuid in
        let ts =
          match se.se_trustees with
          | `Basic x -> x.dbp_trustees
          | `Threshold _ -> []
        in
        match List.find_opt (fun t -> t.st_token = token) ts with
        | None -> forbidden ()
        | Some t ->
            if t.st_public_key <> "" then
              let msg = s_ "Your public key has already been received!" in
              let title = s_ "Error" in
              Pages_common.generic_page ~title msg () >>= Html.send ~code:403
            else
              Printf.sprintf "%s/draft/trustee.html#%s-%s" !Web_config.prefix
                (Uuid.unwrap uuid) token
              |> String_redirection.send)

  let () =
    Html.register ~service:election_draft_trustee_static (fun () () ->
        Pages_admin.election_draft_trustee_static ())

  let () =
    Any.register ~service:election_draft_trustee_post
      (fun (uuid, token) public_key ->
        let@ () = without_site_user () in
        let* l = get_preferred_gettext () in
        let open (val l) in
        if token = "" then forbidden ()
        else
          let* x =
            let@ s = Storage.with_transaction in
            let@ (Draft (_, se) as fse), set =
              with_draft_public_update s uuid
            in
            let ts =
              match se.se_trustees with
              | `Basic x -> x.dbp_trustees
              | `Threshold _ -> []
            in
            let&* t = List.find_opt (fun x -> token = x.st_token) ts in
            if t.st_public_key <> "" then
              let msg =
                s_
                  "A public key already existed, the key you've just uploaded \
                   has been ignored!"
              in
              let title = s_ "Error" in
              return_some (title, msg, 400)
            else
              let version = se.se_version in
              let module G = (val Group.of_string ~version se.se_group : GROUP)
              in
              let module Trustees = (val Trustees.get_by_version version) in
              let pk =
                trustee_public_key_of_string (sread G.of_string)
                  (sread G.Zq.of_string) public_key
              in
              let module K = Trustees.MakeCombinator (G) in
              if not (K.check [ `Single pk ]) then
                let msg = s_ "Invalid public key!" in
                let title = s_ "Error" in
                return_some (title, msg, 400)
              else (
                (* we keep pk as a string because of G.t *)
                t.st_public_key <- public_key;
                let* () = set fse in
                let msg = s_ "Your key has been received and checked!" in
                let title = s_ "Success" in
                return_some (title, msg, 200))
          in
          match x with
          | None -> forbidden ()
          | Some (title, msg, code) ->
              Pages_common.generic_page ~title msg () >>= Html.send ~code)

  let () =
    Any.register ~service:election_draft_confirm (fun uuid () ->
        let@ s = Storage.with_transaction in
        let@ se = with_draft ~save:false s uuid in
        Pages_admin.election_draft_confirm s uuid se () >>= Html.send)

  let redirect_billing ~cont ~url ~callback ~id =
    let* () = Eliom_reference.set Web_state.billing_env (Some cont) in
    Printf.sprintf "%s/pre?id=%s&callback=%s" url id callback
    |> String_redirection.send

  let handle_draft_create postbilling cont =
    let@ s = Storage.with_transaction in
    let uuid =
      match cont.path with
      | ContSiteElection uuid -> uuid
      | _ -> failwith "handle_draft_create"
    in
    let@ _, account, _ = with_site_user in
    let@ (Draft (_, se) as fse), set = with_draft_update ~save:false s uuid in
    Lwt.catch
      (fun () ->
        if Accounts.check account se.se_owners then
          let* status = Api_drafts.get_draft_status s uuid fse in
          let* () =
            Web_persist.validate_election ~admin_id:account.id s uuid (fse, set)
              status
          in
          Web_cont.exec cont
        else Lwt.fail (Failure "Forbidden"))
      (fun e ->
        match (postbilling, !Web_config.billing, e) with
        | ( false,
            Some (url, callback),
            Api_generic.Error (`ValidationError (`MissingBilling id)) ) ->
            redirect_billing ~cont ~url ~callback ~id
        | _ -> Pages_admin.new_election_failure (`Exception e) () >>= Html.send)

  let () =
    Any.register ~service:election_draft_create (fun uuid () ->
        let cont = { path = ContSiteElection uuid; admin = Classic } in
        handle_draft_create false cont)

  let () =
    Any.register ~service:election_draft_prebilling (fun (id, cont) () ->
        match !Web_config.billing with
        | Some (url, callback) -> redirect_billing ~cont ~url ~callback ~id
        | None -> handle_draft_create false cont)

  let () =
    Any.register ~service:election_draft_postbilling (fun () () ->
        let* cont = Eliom_reference.get Web_state.billing_env in
        match cont with
        | None ->
            Pages_admin.new_election_failure
              (`Exception (Failure "Session expired")) ()
            >>= Html.send
        | Some cont -> handle_draft_create true cont)

  let () =
    Any.register ~service:election_draft_destroy (fun uuid () ->
        let@ s = Storage.with_transaction in
        let@ _ = with_draft ~save:false s uuid in
        let* () = Web_persist.delete_draft s uuid in
        Redirection.send (Redirection admin))

  let () =
    Any.register ~service:election_draft_import (fun uuid () ->
        let@ _, account, _ = with_site_user in
        let* _, a, b, c = get_elections_by_owner_sorted account.id in
        let@ s = Storage.with_transaction in
        let@ se = with_draft ~save:false s uuid in
        Pages_admin.election_draft_import uuid se (a, b, c) () >>= Html.send)

  let () =
    Any.register ~service:election_draft_import_post (fun uuid from_s ->
        let from = Uuid.wrap from_s in
        let@ s = Storage.with_transaction in
        let@ se = with_draft_update ~save:false s uuid in
        let@ _ = with_metadata_check_owner s from in
        let* l = get_preferred_gettext () in
        let open (val l) in
        let* x = Api_drafts.import_voters s uuid se from in
        match x with
        | Ok () -> redir_preapply election_draft_voters uuid ()
        | Error `Forbidden -> forbidden ()
        | Error `NotFound ->
            Pages_common.generic_page ~title:(s_ "Error")
              ~service:(preapply ~service:election_draft_voters uuid)
              (Printf.sprintf
                 (f_ "Could not retrieve voter list from election %s")
                 from_s)
              ()
            >>= Html.send
        | Error (`TotalWeightTooBig total_weight) ->
            Pages_common.generic_page ~title:(s_ "Error")
              ~service:(preapply ~service:election_draft_voters uuid)
              (Printf.sprintf
                 (f_
                    "The total weight (%s) cannot be handled. Its expanded \
                     value must be less than %s.")
                 Weight.(to_string total_weight)
                 (Z.to_string Weight.max_expanded_weight))
              ()
            >>= Html.send
        | Error (`Duplicate x) ->
            Pages_common.generic_page ~title:(s_ "Error")
              ~service:(preapply ~service:election_draft_voters uuid)
              (Printf.sprintf
                 (f_
                    "Duplicate voter: %s. This is not allowed. If two voters \
                     have the same address, use different logins.")
                 x)
              ()
            >>= Html.send)

  let () =
    Any.register ~service:election_draft_import_trustees (fun uuid () ->
        let@ _, account, _ = with_site_user in
        let* _, a, b, c = get_elections_by_owner_sorted account.id in
        let@ s = Storage.with_transaction in
        let@ se = with_draft ~save:false s uuid in
        Pages_admin.election_draft_import_trustees uuid se (a, b, c) ()
        >>= Html.send)

  let () =
    Any.register ~service:election_draft_import_trustees_post (fun uuid from ->
        let from = Uuid.wrap from in
        let@ s = Storage.with_transaction in
        let@ se = with_draft_update ~save:false s uuid in
        let@ _ = with_metadata_check_owner s from in
        let* metadata = Web_persist.get_election_metadata s from in
        let* x = Api_drafts.import_trustees se s from metadata in
        match x with
        | Ok `Basic -> redir_preapply election_draft_trustees uuid ()
        | Ok `Threshold ->
            redir_preapply election_draft_threshold_trustees uuid ()
        | Stdlib.Error e ->
            let* l = get_preferred_gettext () in
            let open (val l) in
            let msg =
              match e with
              | `None ->
                  s_ "Could not retrieve trustees from selected election!"
              | `Invalid ->
                  s_ "Imported trustees are invalid for this election!"
              | `Inconsistent -> s_ "Inconsistency in imported election!"
              | `MissingPrivateKeys ->
                  s_ "Encrypted decryption keys are missing!"
              | `Unsupported -> s_ "Unsupported trustees!"
            in
            Pages_common.generic_page ~title:(s_ "Error")
              ~service:(preapply ~service:election_draft_trustees uuid)
              msg ()
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
        redir_preapply admin () ())

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
                  Printf.sprintf "%s/election/trustees.html#%s-%s"
                    !Web_config.prefix (Uuid.unwrap uuid) token
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

  let () =
    Html.register ~service:election_tally_trustees_static (fun () () ->
        Pages_admin.tally_trustees_static ())

  exception TallyEarlyError

  let render_tally_early_error_as_forbidden f =
    Lwt.catch f (function TallyEarlyError -> forbidden () | e -> Lwt.fail e)

  let () =
    Any.register ~service:election_tally_trustees_post
      (fun (uuid, token) partial_decryption ->
        let@ () = render_tally_early_error_as_forbidden in
        let* l = get_preferred_gettext () in
        let open (val l) in
        let@ s = Storage.with_transaction in
        let* () =
          let* state = Web_persist.get_election_state s uuid in
          match state with
          | `EncryptedTally -> return ()
          | _ -> Lwt.fail TallyEarlyError
        in
        let* trustee_id =
          let* x = find_trustee_id s uuid token in
          match x with Some x -> return x | None -> Lwt.fail TallyEarlyError
        in
        let* pds = Public_archive.get_partial_decryptions s uuid in
        let* () =
          if List.exists (fun x -> x.owned_owner = trustee_id) pds then
            Lwt.fail TallyEarlyError
          else return ()
        in
        let* () = if trustee_id > 0 then return () else fail_http `Not_found in
        let@ election = with_election s uuid in
        let module W = (val election) in
        let* pks =
          let* trustees = Public_archive.get_trustees s uuid in
          let trustees =
            trustees_of_string
              W.(sread G.of_string)
              W.(sread G.Zq.of_string)
              trustees
          in
          trustees
          |> List.map (function
               | `Single x -> [ x ]
               | `Pedersen t -> Array.to_list t.t_verification_keys)
          |> List.flatten |> Array.of_list |> return
        in
        let pk = pks.(trustee_id - 1).trustee_public_key in
        let pd =
          partial_decryption_of_string
            W.(sread G.of_string)
            W.(sread G.Zq.of_string)
            partial_decryption
        in
        let* et =
          let* x = Public_archive.get_latest_encrypted_tally s uuid in
          match x with
          | None -> assert false
          | Some x ->
              Lwt.return @@ encrypted_tally_of_string W.(sread G.of_string) x
        in
        if
          string_of_partial_decryption
            W.(swrite G.to_string)
            W.(swrite G.Zq.to_string)
            pd
          = partial_decryption
          && W.E.check_factor et pk pd
        then
          let pd = (trustee_id, partial_decryption) in
          let* () = Web_persist.add_partial_decryption s uuid pd in
          Pages_common.generic_page ~title:(s_ "Success")
            (s_ "Your partial decryption has been received and checked!")
            ()
          >>= Html.send
        else
          let service =
            preapply ~service:election_tally_trustees (uuid, token)
          in
          Pages_common.generic_page ~title:(s_ "Error") ~service
            (s_ "The partial decryption didn't pass validation!")
            ()
          >>= Html.send)

  let handle_election_tally_release uuid () =
    let@ s = Storage.with_transaction in
    let@ _ = with_metadata_check_owner s uuid in
    let* l = get_preferred_gettext () in
    let open (val l) in
    Lwt.catch
      (fun () ->
        let* () = Web_persist.release_tally s uuid in
        redir_preapply election_home (uuid, ()) ())
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
            Printf.sprintf "%s/election/shuffle.html#%s-%s" !Web_config.prefix
              (Uuid.unwrap uuid) token
            |> String_redirection.send
        | _ -> forbidden ())

  let () =
    Html.register ~service:election_shuffle_link_static (fun () () ->
        Pages_admin.shuffle_static ())

  let () =
    Any.register ~service:election_shuffle_post (fun (uuid, token) shuffle ->
        let@ s = Storage.with_transaction in
        let@ election = with_election s uuid in
        let@ () = without_site_user () in
        let* l = get_preferred_gettext () in
        let open (val l) in
        let* expected_token = Web_persist.get_shuffle_token s uuid in
        match expected_token with
        | Some x when token = x.tk_token ->
            Lwt.catch
              (fun () ->
                let* y =
                  Web_persist.append_to_shuffles s election x.tk_trustee_id
                    shuffle
                in
                match y with
                | Some _ ->
                    let* () = Spool.del s uuid Spool.shuffle_token in
                    let* () = Spool.del s uuid Spool.audit_cache in
                    Pages_common.generic_page ~title:(s_ "Success")
                      (s_ "The shuffle has been successfully applied!")
                      ()
                    >>= Html.send
                | None ->
                    Pages_common.generic_page ~title:(s_ "Error")
                      (s_ "An error occurred while applying the shuffle.")
                      ()
                    >>= Html.send)
              (fun e ->
                Pages_common.generic_page ~title:(s_ "Error")
                  (Printf.sprintf
                     (f_ "Data is invalid! (%s)")
                     (Printexc.to_string e))
                  ()
                >>= Html.send)
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

  let () =
    Any.register ~service:election_draft_threshold_set (fun uuid threshold ->
        let@ s = Storage.with_transaction in
        let@ se = with_draft_update ~save:false s uuid in
        let* l = get_preferred_gettext () in
        let open (val l) in
        let* x = Api_drafts.set_threshold se threshold in
        match x with
        | Ok () -> redir_preapply election_draft_threshold_trustees uuid ()
        | Error `NoTrustees ->
            let msg = s_ "Please add some trustees first!" in
            let service =
              preapply ~service:election_draft_threshold_trustees uuid
            in
            Pages_common.generic_page ~title:(s_ "Error") ~service msg ()
            >>= Html.send
        | Error `OutOfBounds ->
            let msg =
              s_
                "The threshold must be positive and smaller than the number of \
                 trustees!"
            in
            let service =
              preapply ~service:election_draft_threshold_trustees uuid
            in
            Pages_common.generic_page ~title:(s_ "Error") ~service msg ()
            >>= Html.send)

  let () =
    Any.register ~service:election_draft_threshold_trustee_add
      (handle_trustee_add (`Threshold 0))

  let () =
    Any.register ~service:election_draft_threshold_trustee_del
      (handle_trustee_del election_draft_threshold_trustees)

  let () =
    Any.register ~service:election_draft_threshold_trustee
      (fun (uuid, token) () ->
        let@ s = Storage.with_transaction in
        let@ () =
          without_site_user
            ~fallback:(fun (_, a, _) ->
              let@ (Draft (_, se) as fse) = with_draft_public s uuid in
              if Accounts.check a se.se_owners then
                Pages_admin.election_draft_threshold_trustees ~token uuid fse ()
                >>= Html.send
              else forbidden ())
            ()
        in
        let* election = Spool.get s uuid Spool.draft in
        match election with
        | None -> fail_http `Not_found
        | Some _ ->
            Printf.sprintf "%s/draft/threshold-trustee.html#%s-%s"
              !Web_config.prefix (Uuid.unwrap uuid) token
            |> String_redirection.send)

  let () =
    Html.register ~service:election_draft_threshold_trustee_static (fun () () ->
        Pages_admin.election_draft_threshold_trustee_static ())

  let wrap_handler_without_site_user f =
    without_site_user () (fun () -> wrap_handler f)

  let () =
    Any.register ~service:election_draft_threshold_trustee_post
      (fun (uuid, token) data ->
        let@ () = wrap_handler_without_site_user in
        let* () =
          let@ s = Storage.with_transaction in
          let@ Draft (v, se), set = with_draft_public_update s uuid in
          let version = se.se_version in
          let module G = (val Group.of_string ~version se.se_group : GROUP) in
          let se_trustees =
            se.se_trustees
            |> string_of_draft_trustees Yojson.Safe.write_json
            |> draft_trustees_of_string (sread G.Zq.of_string)
          in
          let dtp =
            match se_trustees with
            | `Basic _ -> failwith "No threshold trustees"
            | `Threshold x -> x
          in
          let ts = Array.of_list dtp.dtp_trustees in
          let threshold =
            match dtp.dtp_threshold with
            | Some t -> t
            | None -> failwith "No threshold set"
          in
          let i, t =
            match
              Array.findi
                (fun i x -> if token = x.stt_token then Some (i, x) else None)
                ts
            with
            | Some (i, t) -> (i, t)
            | None -> failwith "Trustee not found"
          in
          let context =
            {
              group = se.se_group;
              size = Array.length ts;
              threshold;
              index = i + 1;
            }
          in
          let get_certs () =
            Array.map
              (fun x ->
                match x.stt_cert with
                | None -> failwith "Missing certificate"
                | Some y -> y)
              ts
          in
          let get_polynomials () =
            Array.map
              (fun x ->
                match x.stt_polynomial with
                | None -> failwith "Missing polynomial"
                | Some y -> y)
              ts
          in
          let module Trustees = (val Trustees.get_by_version version) in
          let module P = Trustees.MakePKI (G) (Random) in
          let module C = Trustees.MakeChannels (G) (Random) (P) in
          let module K = Trustees.MakePedersen (G) (Random) (P) (C) in
          let* () =
            match t.stt_step with
            | Some 1 ->
                let cert = cert_of_string (sread G.Zq.of_string) data in
                if K.step1_check context cert then (
                  t.stt_cert <- Some cert;
                  t.stt_step <- Some 2;
                  return_unit)
                else failwith "Invalid certificate"
            | Some 3 ->
                let certs = get_certs () in
                let polynomial =
                  polynomial_of_string (sread G.Zq.of_string) data
                in
                if K.step3_check certs i polynomial then (
                  t.stt_polynomial <- Some polynomial;
                  t.stt_step <- Some 4;
                  return_unit)
                else failwith "Invalid polynomial"
            | Some 5 ->
                let certs = get_certs () in
                let polynomials = get_polynomials () in
                let voutput =
                  voutput_of_string (sread G.of_string) (sread G.Zq.of_string)
                    data
                in
                if K.step5_check certs i polynomials voutput then (
                  t.stt_voutput <- Some data;
                  t.stt_step <- Some 6;
                  return_unit)
                else failwith "Invalid voutput"
            | _ -> failwith "Unknown step"
          in
          let* () =
            if Array.for_all (fun x -> x.stt_step = Some 2) ts then (
              (try
                 let threshold = K.step2 (get_certs ()) in
                 assert (dtp.dtp_threshold = Some threshold);
                 Array.iter (fun x -> x.stt_step <- Some 3) ts
               with e -> dtp.dtp_error <- Some (Printexc.to_string e));
              return_unit)
            else return_unit
          in
          let* () =
            if Array.for_all (fun x -> x.stt_step = Some 4) ts then (
              (try
                 let certs = get_certs () in
                 let polynomials = get_polynomials () in
                 let vinputs = K.step4 certs polynomials in
                 for j = 0 to Array.length ts - 1 do
                   ts.(j).stt_vinput <- Some vinputs.(j)
                 done;
                 Array.iter (fun x -> x.stt_step <- Some 5) ts
               with e -> dtp.dtp_error <- Some (Printexc.to_string e));
              return_unit)
            else return_unit
          in
          let* () =
            if Array.for_all (fun x -> x.stt_step = Some 6) ts then (
              (try
                 let certs = get_certs () in
                 let polynomials = get_polynomials () in
                 let voutputs =
                   Array.map
                     (fun x ->
                       match x.stt_voutput with
                       | None -> failwith "Missing voutput"
                       | Some y ->
                           voutput_of_string (sread G.of_string)
                             (sread G.Zq.of_string) y)
                     ts
                 in
                 let p = K.step6 certs polynomials voutputs in
                 dtp.dtp_parameters <-
                   Some
                     (string_of_threshold_parameters (swrite G.to_string)
                        (swrite G.Zq.to_string) p);
                 Array.iter (fun x -> x.stt_step <- Some 7) ts
               with e -> dtp.dtp_error <- Some (Printexc.to_string e));
              return_unit)
            else return_unit
          in
          se.se_trustees <-
            se_trustees
            |> string_of_draft_trustees (swrite G.Zq.to_string)
            |> draft_trustees_of_string Yojson.Safe.read_json;
          set (Draft (v, se))
        in
        redir_preapply election_draft_threshold_trustee (uuid, token) ())

  module HashedInt = struct
    type t = int

    let equal = ( = )
    let hash x = x
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
                      (Some service, default_admin ContSiteAdmin)
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
                      (Some service, default_admin ContSiteAdmin)
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
                Redirection.send (Redirection admin)))

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
        Redirection.send (Redirection admin))

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
