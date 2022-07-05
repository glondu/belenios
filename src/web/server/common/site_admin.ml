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
open Belenios_platform
open Platform
open Belenios_core
open Belenios
open Serializable_builtin_t
open Serializable_j
open Signatures
open Common
open Web_serializable_builtin_t
open Web_serializable_j
open Web_common

module Pages_admin_root = Pages_admin

module Make (X : Pages_sig.S) (Site_common : Site_common_sig.S) (Web_auth : Web_auth_sig.S) = struct

  open X
  open Web_services
  open Site_common

  module PString = String

  open Eliom_service
  open Eliom_registration

  let get_preferred_gettext () = Web_i18n.get_preferred_gettext "admin"

  let delete_election uuid =
    let* election = find_election uuid in
    match election with
    | None -> return_unit
    | Some election ->
       let* metadata = Web_persist.get_election_metadata uuid in
       Api_elections.delete_election election metadata

  let () = Any.register ~service:home
             (fun () () -> Redirection.send (Redirection admin))

  let get_elections_by_owner_sorted u =
    let* elections = Web_persist.get_elections_by_owner u in
    let filter kind =
      List.filter (fun (x, _, _, _) -> x = kind) elections |>
        List.map (fun (_, a, b, c) -> a, b, c)
    in
    let draft = filter `Draft in
    let elections = filter `Validated in
    let tallied = filter `Tallied in
    let archived = filter `Archived in
    let sort l =
      List.sort (fun (_, x, _) (_, y, _) -> datetime_compare x y) l |>
        List.map (fun (x, _, y) -> x, y)
    in
    return (sort draft, sort elections, sort tallied, sort archived)

  let () =
    let@ a = Accounts.add_update_hook in
    let* user = Eliom_reference.get Web_state.site_user in
    match user with
    | Some (u, b, t) when a.account_id = b.account_id ->
       Eliom_reference.set Web_state.site_user (Some (u, a, t))
    | _ -> Lwt.return_unit

  let with_site_user f =
    let* user = Eliom_reference.get Web_state.site_user in
    match user with
    | Some u -> f u
    | None -> forbidden ()

  let with_metadata_check_owner uuid f =
    let* user = Eliom_reference.get Web_state.site_user in
    let* metadata = Web_persist.get_election_metadata uuid in
    match user, metadata.e_owner with
    | Some x, Some y when Accounts.check x y -> f metadata
    | _, _ -> forbidden ()

  let without_site_user ?fallback () f =
    let* l = get_preferred_gettext () in
    let open (val l) in
    let* user = Eliom_reference.get Web_state.site_user in
    match user with
    | None -> f ()
    | Some u ->
       match fallback with
       | Some g -> g u
       | None ->
          Pages_common.generic_page ~title:(s_ "Error")
            (s_ "This page is not accessible to authenticated administrators, because it is meant to be used by third parties.")
            () >>= Html.send

  let () =
    Redirection.register ~service:privacy_notice_accept
      (fun () cont ->
        let* () = Eliom_reference.set Web_state.show_cookie_disclaimer false in
        let cont = match cont with
          | ContAdmin -> Redirection admin
          | ContSignup service -> Redirection (preapply ~service:signup_captcha service)
        in
        return cont
      )

  let () = Html.register ~service:admin
             (fun () () ->
               let* site_user = Eliom_reference.get Web_state.site_user in
               match site_user with
               | None -> Pages_admin.admin_login Web_auth.get_site_login_handler
               | Some (_, a, _) ->
                  let* show =
                    match a.account_consent with
                    | Some _ -> return_false
                    | None ->
                       let* b = Eliom_reference.get Web_state.show_cookie_disclaimer in
                       if b then (
                         return_true
                       ) else (
                         let account_consent = Some (now ()) in
                         let a = {a with account_consent} in
                         let* () = Accounts.update_account {a with account_consent} in
                         return_false
                       )
                  in
                  if show then (
                    Pages_admin.privacy_notice ContAdmin
                  ) else (
                    if a.account_email = "" then (
                      Pages_admin.set_email ()
                    ) else (
                      let* elections = get_elections_by_owner_sorted a.account_id in
                      Pages_admin.admin ~elections
                    )
                  )
             )

  module SetEmailSender = struct
    let send ~address ~code =
      let* l = get_preferred_gettext () in
      let subject, body = Pages_admin_root.mail_set_email l address code in
      send_email ~subject ~recipient:address ~body MailSetEmail
  end

  module SetEmailOtp = Otp.Make (SetEmailSender) ()

  let () =
    Any.register ~service:set_email_post
      (fun () address ->
        let@ _ = with_site_user in
        if is_email address then (
          let* () = Eliom_reference.set Web_state.set_email_env (Some address) in
          let* () = SetEmailOtp.generate ~address in
          Pages_admin.set_email_confirm ~address
          >>= Html.send
        ) else (
          let* l = get_preferred_gettext () in
          let open (val l) in
          let msg = s_ "This e-mail address is invalid!" in
          let title = s_ "Error" in
          Pages_common.generic_page ~title msg ()
          >>= Html.send ~code:400
        )
      )

  let () =
    Any.register ~service:set_email_confirm
      (fun () code ->
        let* u = Eliom_reference.get Web_state.site_user in
        let* x = Eliom_reference.get Web_state.set_email_env in
        match x, u with
        | None, _ | _, None -> forbidden ()
        | Some address, Some (_, a, _) ->
           if SetEmailOtp.check ~address ~code then (
             let a = {a with account_email = address} in
             let* () = Accounts.update_account a in
             let* () = Eliom_reference.unset Web_state.set_email_env in
             Redirection.send (Redirection admin)
           ) else (
             let* l = get_preferred_gettext () in
             let open (val l) in
             let msg = s_ "The provided code is incorrect. Please go back and try again." in
             let title = s_ "Incorrect code" in
             Pages_common.generic_page ~title msg ()
             >>= Html.send ~code:403
           )
      )

  let create_new_election account cred draft_authentication =
    let open Belenios_api.Serializable_t in
    let draft_questions =
      {
        t_description = default_description;
        t_name = default_name;
        t_questions = default_questions;
        t_administrator = Some account.account_name;
        t_credential_authority = Some (match cred with `Automatic -> "server" | `Manual -> "");
      }
    in
    let draft =
      {
        draft_version = List.hd supported_crypto_versions;
        draft_owners = [account.account_id];
        draft_questions;
        draft_languages = ["en"; "fr"];
        draft_contact = Some (Printf.sprintf "%s <%s>" account.account_name account.account_email);
        draft_booth = 2;
        draft_authentication;
        draft_group = !Web_config.default_group;
      }
    in
    let* uuid = Api_drafts.post_drafts account draft in
    match uuid with
    | Some uuid -> redir_preapply election_draft uuid ()
    | None ->
       let* l = get_preferred_gettext () in
       let open (val l) in
       Pages_common.generic_page ~title:(s_ "Error")
         (s_ "Creating new elections is forbidden on this server!") ()
       >>= Html.send

  let () =
    Any.register ~service:election_draft_pre
      (fun () () ->
        let@ _ = with_site_user in
        Pages_admin.election_draft_pre () >>= Html.send
      )

  let http_rex = "^https?://[a-z0-9/.-]+$"

  let is_http_url =
    let rex = Pcre.regexp ~flags:[`CASELESS] http_rex in
    fun x ->
    match pcre_exec_opt ~rex x with
    | Some _ -> true
    | None -> false

  let () =
    Any.register ~service:election_draft_new
      (fun () (credmgmt, (auth, cas_server)) ->
        let* l = get_preferred_gettext () in
        let open (val l) in
        let@ _, a, _ = with_site_user in
        let* credmgmt = match credmgmt with
          | Some "auto" -> return `Automatic
          | Some "manual" -> return `Manual
          | _ -> fail_http `Bad_request
        in
        let* auth_parsed =
          match auth with
          | Some "password" -> return `Password
          | Some "cas" ->
             (match cas_server with
              | None -> fail_http `Bad_request
              | Some cas_server -> return @@ `CAS (PString.trim cas_server)
             )
          | Some x ->
             let n = PString.length x in
             if n > 1 && PString.get x 0 = '%' then (
               let name = PString.sub x 1 (n - 1) in
               return @@ `Configured name
             ) else fail_http `Bad_request
          | _ -> fail_http `Bad_request
        in
        let has_cas_server =
          match cas_server with
          | None | Some "" -> false
          | Some _ -> true
        in
        match auth_parsed with
        | `CAS cas_server when not (is_http_url cas_server) ->
           Pages_common.generic_page ~title:(s_ "Error") (s_ "Bad CAS server!") () >>= Html.send
        | _ when has_cas_server && auth <> Some "cas" ->
           Pages_common.generic_page ~title:(s_ "Error") (s_ "Non-empty CAS server, but CAS authentication not selected!") () >>= Html.send
        | _ -> create_new_election a credmgmt auth_parsed
      )

  let with_draft_election_ro uuid f =
    let@ u = with_site_user in
    let* election = Web_persist.get_draft_election uuid in
    match election with
    | None -> fail_http `Not_found
    | Some se -> if Accounts.check u se.se_owner then f se else forbidden ()

  let () =
    Any.register ~service:election_draft
      (fun uuid () ->
        let@ se = with_draft_election_ro uuid in
        Pages_admin.election_draft uuid se ()
        >>= Html.send
      )

  let () =
    Any.register ~service:election_draft_trustees
      (fun uuid () ->
        let@ se = with_draft_election_ro uuid in
        match se.se_threshold_trustees with
        | None -> Pages_admin.election_draft_trustees uuid se () >>= Html.send
        | Some _ -> redir_preapply election_draft_threshold_trustees uuid ()
      )

  let () =
    Any.register ~service:election_draft_threshold_trustees
      (fun uuid () ->
        let@ se = with_draft_election_ro uuid in
        Pages_admin.election_draft_threshold_trustees uuid se ()
        >>= Html.send
      )

  let () =
    Any.register ~service:election_draft_credential_authority
      (fun uuid () ->
        let@ se = with_draft_election_ro uuid in
        Pages_admin.election_draft_credential_authority uuid se ()
        >>= Html.send
      )

  let with_draft_election ?(save = true) uuid f =
    let@ u = with_site_user in
    let* l = get_preferred_gettext () in
    let open (val l) in
    let@ () = Web_election_mutex.with_lock uuid in
    let* election = Web_persist.get_draft_election uuid in
    match election with
    | None -> fail_http `Not_found
    | Some se ->
       if Accounts.check u se.se_owner then (
         Lwt.catch
           (fun () ->
             let* r = f se in
             let* () = if save then Web_persist.set_draft_election uuid se else return_unit in
             return r
           )
           (fun e ->
             let msg = match e with Failure s -> s | _ -> Printexc.to_string e in
             let service = preapply ~service:election_draft uuid in
             Pages_common.generic_page ~title:(s_ "Error") ~service msg () >>= Html.send
           )
       ) else forbidden ()

  let () =
    Any.register ~service:election_draft_set_credential_authority
      (fun uuid name ->
        let@ se = with_draft_election uuid in
        let* l = get_preferred_gettext () in
        let open (val l) in
        let service = Eliom_service.preapply ~service:election_draft_credential_authority uuid in
        match (
          if se.se_metadata.e_cred_authority = Some "server" then
            Error (s_ "You cannot set the credential authority for this election!")
          else
            match name with
            | "" -> Ok None
            | "server" -> Error (s_ "Invalid public name for credential authority!")
            | x -> Ok (Some x)
        ) with
        | Ok e_cred_authority ->
           se.se_metadata <- {se.se_metadata with e_cred_authority};
           let msg = s_ "The public name of the credential authority has been set successfully!" in
           Pages_common.generic_page ~title:(s_ "Success") ~service msg () >>= Html.send
        | Error msg ->
           Pages_common.generic_page ~title:(s_ "Error") ~service msg () >>= Html.send
      )

  let () =
    Any.register ~service:election_draft_languages
      (fun uuid languages ->
        let@ se = with_draft_election uuid in
        let* l = get_preferred_gettext () in
        let open (val l) in
        let langs = languages_of_string languages in
        match langs with
        | [] ->
           let service = preapply ~service:election_draft uuid in
           Pages_common.generic_page ~title:(s_ "Error") ~service
             (s_ "You must select at least one language!") () >>= Html.send
        | _ :: _ ->
           let available_languages = List.map fst Belenios_ui.Languages.available in
           let unavailable =
             List.filter (fun x ->
                 not (List.mem x available_languages)
               ) langs
           in
           match unavailable with
           | [] ->
              se.se_metadata <- {
                 se.se_metadata with
                 e_languages = Some langs
               };
              redir_preapply election_draft uuid ()
           | l :: _ ->
              let service = preapply ~service:election_draft uuid in
              Pages_common.generic_page ~title:(s_ "Error") ~service
                (Printf.sprintf (f_ "No such language: %s") l) () >>= Html.send
      )

  let () =
    Any.register ~service:election_draft_contact
      (fun uuid contact ->
        let@ se = with_draft_election uuid in
        let contact =
          if contact = "" || contact = default_contact then
            None
          else Some contact
        in
        se.se_metadata <- {
            se.se_metadata with
            e_contact = contact
          };
        redir_preapply election_draft uuid ()
      )

  let () =
    Any.register ~service:election_draft_admin_name
      (fun uuid name ->
        let@ se = with_draft_election uuid in
        let administrator = if name = "" then None else Some name in
        se.se_administrator <- administrator;
        redir_preapply election_draft uuid ()
      )

  let () =
    Any.register ~service:election_draft_description
      (fun uuid (name, description) ->
        let@ se = with_draft_election uuid in
        let* l = get_preferred_gettext () in
        let open (val l) in
        if PString.length name > max_election_name_size then (
          let msg =
            Printf.sprintf (f_ "The election name must be %d characters or less!")
              max_election_name_size
          in
          Pages_common.generic_page ~title:(s_ "Error") msg () >>= Html.send
        ) else (
          se.se_questions <- {se.se_questions with
                               t_name = name;
                               t_description = description;
                             };
          let* () = Web_persist.clear_elections_by_owner_cache () in
          redir_preapply election_draft uuid ()
        )
      )

  let handle_password se uuid ~force voters =
    let* l = get_preferred_gettext () in
    let open (val l) in
    if List.length voters > !Web_config.maxmailsatonce then
      Lwt.fail (Failure (Printf.sprintf (f_ "Cannot send passwords, there are too many voters (max is %d)") !Web_config.maxmailsatonce))
    else if se.se_questions.t_name = default_name then
      Lwt.fail (Failure (s_ "The election name has not been edited!"))
    else
      let title = se.se_questions.t_name in
      let langs = get_languages se.se_metadata.e_languages in
      let show_weight =
        List.exists
          (fun id ->
            let _, _, weight = split_identity_opt id.sv_id in
            weight <> None
          ) voters
      in
      let* jobs =
        Lwt_list.fold_left_s (fun jobs id ->
            match id.sv_password with
            | Some _ when not force -> Lwt.return jobs
            | None | Some _ ->
               let* email, x =
                 Mails_voter.generate_password_email se.se_metadata langs title uuid
                   id.sv_id show_weight
               in
               id.sv_password <- Some x;
               Lwt.return (email :: jobs)
          ) [] voters
      in
      let* () = Mails_voter.submit_bulk_emails jobs in
      let service = preapply ~service:election_draft uuid in
      Pages_common.generic_page ~title:(s_ "Success") ~service
        (s_ "Passwords have been generated and mailed!") () >>= Html.send

  let () =
    Any.register ~service:election_draft_auth_genpwd
      (fun uuid () ->
        let@ se = with_draft_election uuid in
        handle_password se uuid ~force:false se.se_voters
      )

  let () =
    Any.register ~service:election_regenpwd
      (fun uuid () ->
        Pages_admin.regenpwd uuid () >>= Html.send)

  let () =
    Any.register ~service:election_regenpwd_post
      (fun uuid user ->
        let@ metadata = with_metadata_check_owner uuid in
        let* l = get_preferred_gettext () in
        let open (val l) in
        let@ election = with_election uuid in
        let service = preapply ~service:election_admin uuid in
        let* b = Api_elections.regenpwd election metadata user in
        if b then (
          Pages_common.generic_page ~title:(s_ "Success") ~service
            (Printf.sprintf (f_ "A new password has been mailed to %s.") user) ()
          >>= Html.send
        ) else (
          Pages_common.generic_page ~title:(s_ "Error") ~service
            (Printf.sprintf (f_ "%s is not a registered user for this election.") user) ()
          >>= Html.send
        )
      )

  let () =
    Any.register ~service:election_draft_questions
      (fun uuid () ->
        let@ se = with_draft_election_ro uuid in
        Pages_admin.election_draft_questions uuid se ()
        >>= Html.send
      )

  let () =
    Any.register ~service:election_draft_questions_post
      (fun uuid (template, booth_version) ->
        let@ se = with_draft_election uuid in
        let* l = get_preferred_gettext () in
        let open (val l) in
        let template = template_of_string template in
        let fixed_group = is_group_fixed se in
        (match get_suitable_group_kind se.se_questions, get_suitable_group_kind template with
         | `NH, `NH | `H, `H -> ()
         | `NH, `H when fixed_group -> ()
         | `NH, `H -> se.se_group <- !Web_config.default_group
         | `H, `NH when fixed_group -> failwith (s_ "This kind of change is not allowed now!")
         | `H, `NH -> se.se_group <- !Web_config.nh_group
        );
        se.se_questions <- template;
        let e_booth_version = match booth_version with 1 -> None | x -> Some x in
        se.se_metadata <- { se.se_metadata with e_booth_version };
        redir_preapply election_draft uuid ()
      )

  let () =
    Any.register ~service:election_draft_preview
      (fun (uuid, ()) () ->
        let@ se = with_draft_election_ro uuid in
        let version = Option.value se.se_version ~default:0 in
        let group = se.se_group in
        let module G = (val Group.of_string ~version group : GROUP) in
        let params = {
            e_version = Option.value se.se_version ~default:0;
            e_description = se.se_questions.t_description;
            e_name = se.se_questions.t_name;
            e_questions = se.se_questions.t_questions;
            e_uuid = uuid;
            e_administrator = se.se_administrator;
            e_credential_authority = se.se_metadata.e_cred_authority;
          }
        in
        let public_key = G.to_string G.g in
        let raw_election = Election.make_raw_election params ~group ~public_key in
        let* x = String.send (raw_election, "application/json") in
        return @@ Eliom_registration.cast_unknown_content_kind x
      )

  let () =
    Any.register ~service:election_draft_voters
      (fun uuid () ->
        let@ se = with_draft_election_ro uuid in
        Pages_admin.election_draft_voters uuid se !Web_config.maxmailsatonce ()
        >>= Html.send
      )

  let bool_of_opt = function
    | None -> false
    | Some _ -> true

  let check_consistency voters =
    match voters with
    | [] -> true
    | voter :: voters ->
       let has_login, has_weight =
         let _, login, weight = split_identity_opt voter.sv_id in
         bool_of_opt login,
         bool_of_opt weight
       in
       let rec loop = function
         | [] -> true
         | voter :: voters ->
            let _, login, weight = split_identity_opt voter.sv_id in
            bool_of_opt login = has_login
            && bool_of_opt weight = has_weight
            && loop voters
       in
       loop voters

  let () =
    Any.register ~service:election_draft_voters_add
      (fun uuid voters ->
        let@ se = with_draft_election uuid in
        let* l = get_preferred_gettext () in
        let open (val l) in
        if se.se_public_creds_received then
          forbidden ()
        else (
          let voters = Pcre.split voters in
          let () =
            match List.find_opt (fun x -> not (is_identity x)) voters with
            | Some bad ->
               Printf.ksprintf failwith (f_ "%S is not a valid identity") bad
            | None -> ()
          in
          match Api_drafts.merge_voters se.se_voters voters (fun _ -> None) with
          | Error x ->
             Printf.ksprintf failwith (f_ "Duplicate voter: %s. This is not allowed. If two voters have the same address, use different logins.") x
          | Ok (voters, total_weight) ->
             let () =
               let expanded = Weight.expand ~total:total_weight total_weight in
               if Z.compare expanded Weight.max_expanded_weight > 0 then
                 Printf.ksprintf failwith
                   (f_ "The total weight (%s) cannot be handled. Its expanded value must be less than %s.")
                   Weight.(to_string total_weight)
                   (Z.to_string Weight.max_expanded_weight)
             in
             if not (check_consistency voters) then
               failwith
                 (s_ "The voter list is not consistent (a login or a weight is missing).");
             let uses_password_auth =
               match se.se_metadata.e_auth_config with
               | Some configs ->
                  List.exists
                    (fun {auth_system; _} -> auth_system = "password")
                    configs
               | None -> false
             in
             let cred_auth_is_server =
               se.se_metadata.e_cred_authority = Some "server"
             in
             if
               (uses_password_auth || cred_auth_is_server)
               && List.length voters > !Web_config.maxmailsatonce
             then
               Lwt.fail
                 (Failure
                    (Printf.sprintf (f_ "There are too many voters (max is %d)")
                       !Web_config.maxmailsatonce))
             else (
               se.se_voters <- voters;
               redir_preapply election_draft_voters uuid ()
             )
        )
      )

  let () =
    Any.register ~service:election_draft_voters_remove
      (fun uuid voter ->
        let@ se = with_draft_election uuid in
        if se.se_public_creds_received then
          forbidden ()
        else (
          se.se_voters <- List.filter (fun v -> v.sv_id <> voter) se.se_voters;
          redir_preapply election_draft_voters uuid ()
        )
      )

  let () =
    Any.register ~service:election_draft_voters_remove_all
      (fun uuid () ->
        let@ se = with_draft_election uuid in
        if se.se_public_creds_received then
          forbidden ()
        else (
          se.se_voters <- [];
          redir_preapply election_draft_voters uuid ()
        )
      )

  let () =
    Any.register ~service:election_draft_voters_passwd
      (fun uuid voter ->
        let@ se = with_draft_election uuid in
        let voter = List.filter (fun v -> v.sv_id = voter) se.se_voters in
        handle_password se uuid ~force:true voter
      )

  let ensure_trustees_mode uuid se mode =
    match se.se_threshold_trustees, mode with
    | None, `Basic | Some _, `Threshold _ -> Lwt.return se
    | Some _, `Basic | None, `Threshold _ ->
       let* () = Api_drafts.put_draft_trustees_mode uuid se mode in
       let* x = Web_persist.get_draft_election uuid in
       match x with
       | Some se -> Lwt.return se
       | None -> Lwt.fail (Failure "inconsistency in ensure_trustees_mode")

  let handle_trustee_add mode uuid (trustee_address, trustee_name) =
    let@ se = with_draft_election ~save:false uuid in
    let* l = get_preferred_gettext () in
    let open (val l) in
    if is_email trustee_address then (
      let* se = ensure_trustees_mode uuid se mode in
      let open Belenios_api.Serializable_t in
      let trustee = {trustee_address; trustee_name; trustee_token = None; trustee_state = None} in
      let* () = Api_drafts.post_draft_trustees uuid se trustee in
      redir_preapply election_draft_trustees uuid ()
    ) else (
      let msg = Printf.sprintf (f_ "%s is not a valid e-mail address!") trustee_address in
      let service = preapply ~service:election_draft_trustees uuid in
      Pages_common.generic_page ~title:(s_ "Error") ~service msg () >>= Html.send
    )

  let handle_trustee_del service uuid address =
    let@ se = with_draft_election ~save:false uuid in
    let* _ = Api_drafts.delete_draft_trustee uuid se address in
    redir_preapply service uuid ()

  let () =
    Any.register ~service:election_draft_trustee_add
      (handle_trustee_add `Basic)

  let () =
    Any.register ~service:election_draft_trustee_del
      (handle_trustee_del election_draft_trustees)

  let () =
    Any.register ~service:election_draft_credentials
      (fun (uuid, token) () ->
        let@ () = without_site_user () in
        let* election = Web_persist.get_draft_election uuid in
        match election with
        | None -> fail_http `Not_found
        | Some se ->
           if se.se_public_creds_received then (
             Pages_admin.election_draft_credentials_already_generated ()
             >>= Html.send
           ) else (
             Printf.sprintf "%s/draft/credentials.html#%s-%s"
               !Web_config.prefix (raw_string_of_uuid uuid) token
             |> String_redirection.send
           )
      )

  let () =
    Html.register ~service:election_draft_credentials_static
      (fun () () -> Pages_admin.election_draft_credentials_static ())

  let handle_credentials_post uuid token creds =
    let* election = Web_persist.get_draft_election uuid in
    match election with
    | None -> fail_http `Not_found
    | Some se ->
       if se.se_public_creds <> token then forbidden () else
         if se.se_public_creds_received then forbidden () else
           let* creds = Lwt_stream.to_string creds in
           let creds = split_lines creds in
           let* () = Api_drafts.submit_public_credentials uuid se creds in
           Pages_admin.election_draft_credentials_done se () >>= Html.send

  let () =
    Any.register ~service:election_draft_credentials_post
      (fun (uuid, token) creds ->
        let@ () = without_site_user () in
        let s = Lwt_stream.of_string creds in
        wrap_handler (fun () -> handle_credentials_post uuid token s)
      )

  let () =
    Any.register ~service:election_draft_credentials_post_file
      (fun (uuid, token) creds ->
        let@ () = without_site_user () in
        let s = Lwt_io.chars_of_file creds.Ocsigen_extensions.tmp_filename in
        wrap_handler (fun () -> handle_credentials_post uuid token s)
      )

  let () =
    Any.register ~service:election_draft_credentials_server
      (fun uuid () ->
        let@ se = with_draft_election uuid in
        let* l = get_preferred_gettext () in
        let open (val l) in
        if se.se_questions.t_name = default_name then
          Lwt.fail (Failure (s_ "The election name has not been edited!"))
        else (
          let send = Mails_voter.generate_credential_email uuid se in
          let* x = Api_drafts.generate_credentials_on_server send uuid se in
          match x with
          | Ok jobs ->
             let* () = Mails_voter.submit_bulk_emails jobs in
             let service = preapply ~service:election_draft uuid in
             Pages_common.generic_page ~title:(s_ "Success") ~service
               (s_ "Credentials have been generated and mailed! You should download private credentials (and store them securely), in case someone loses his/her credential.") () >>= Html.send
          | Error `Already ->
             Lwt.fail (Failure (s_ "The credentials were already sent"))
          | Error `NoVoters ->
             Lwt.fail (Failure (s_ "No voters"))
          | Error `TooManyVoters ->
             Lwt.fail (Failure (Printf.sprintf (f_ "Cannot send credentials, there are too many voters (max is %d)") !Web_config.maxmailsatonce))
          | Error `NoServer ->
             Lwt.fail (Failure (s_ "The authority is not the server"))
        )
      )

  let () =
    Any.register ~service:election_draft_credentials_get
      (fun uuid () ->
        let@ _ = with_draft_election_ro uuid in
        let* () = Api_drafts.set_downloaded uuid in
        File.send ~content_type:"text/plain" (uuid /// "private_creds.txt")
      )

  let () =
    Any.register ~service:election_draft_trustee
      (fun (uuid, token) () ->
        let* l = get_preferred_gettext () in
        let open (val l) in
        let@ () =
          without_site_user
            ~fallback:(fun u ->
              let* election = Web_persist.get_draft_election uuid in
              match election with
              | None -> fail_http `Not_found
              | Some se ->
                 if Accounts.check u se.se_owner then (
                   Pages_admin.election_draft_trustees ~token uuid se () >>= Html.send
                 ) else forbidden ()
            ) ()
        in
        let* election = Web_persist.get_draft_election uuid in
        match election with
        | None -> fail_http `Not_found
        | Some se ->
           match List.find_opt (fun t -> t.st_token = token) se.se_public_keys with
           | None -> forbidden ()
           | Some t ->
              if t.st_public_key <> "" then
                let msg = s_ "Your public key has already been received!" in
                let title = s_ "Error" in
                Pages_common.generic_page ~title msg () >>= Html.send ~code:403
              else (
                Printf.sprintf "%s/draft/trustee.html#%s-%s"
                  !Web_config.prefix (raw_string_of_uuid uuid) token
                |> String_redirection.send
              )
      )

  let () =
    Html.register ~service:election_draft_trustee_static
      (fun () () -> Pages_admin.election_draft_trustee_static ())

  let () =
    Any.register ~service:election_draft_trustee_post
      (fun (uuid, token) public_key ->
        let@ () = without_site_user () in
        let* l = get_preferred_gettext () in
        let open (val l) in
        if token = "" then
          forbidden ()
        else
          let* x =
            Web_election_mutex.with_lock uuid
              (fun () ->
                let* election = Web_persist.get_draft_election uuid in
                match election with
                | None -> fail_http `Not_found
                | Some se ->
                   match List.find_opt (fun x -> token = x.st_token) se.se_public_keys with
                   | None -> return_none
                   | Some t ->
                      if t.st_public_key <> "" then
                        let msg = s_ "A public key already existed, the key you've just uploaded has been ignored!" in
                        let title = s_ "Error" in
                        return_some (title, msg, 400)
                      else
                        let version = Option.value se.se_version ~default:0 in
                        let module G = (val Group.of_string ~version se.se_group : GROUP) in
                        let module Trustees = (val Trustees.get_by_version (Option.value se.se_version ~default:0)) in
                        let pk = trustee_public_key_of_string G.read public_key in
                        let module K = Trustees.MakeCombinator (G) in
                        if not (K.check [`Single pk]) then
                          let msg = s_ "Invalid public key!" in
                          let title = s_ "Error" in
                          return_some (title, msg, 400)
                        else (
                          (* we keep pk as a string because of G.t *)
                          t.st_public_key <- public_key;
                          let* () = Web_persist.set_draft_election uuid se in
                          let msg = s_ "Your key has been received and checked!" in
                          let title = s_ "Success" in
                          return_some (title, msg, 200)
                        )
              )
          in
          match x with
          | None -> forbidden ()
          | Some (title, msg, code) -> Pages_common.generic_page ~title msg () >>= Html.send ~code
      )

  let () =
    Any.register ~service:election_draft_confirm
      (fun uuid () ->
        let@ se = with_draft_election_ro uuid in
        Pages_admin.election_draft_confirm uuid se () >>= Html.send
      )

  let () =
    Any.register ~service:election_draft_create
      (fun uuid () ->
        let@ _, account, _ = with_site_user in
        let@ se = with_draft_election ~save:false uuid in
        Lwt.catch
          (fun () ->
            let* () = Api_drafts.validate_election account uuid se in
            redir_preapply election_admin uuid ()
          )
          (fun e ->
            Pages_admin.new_election_failure (`Exception e) () >>= Html.send
          )
      )

  let () =
    Any.register ~service:election_draft_destroy
      (fun uuid () ->
        let@ _ = with_draft_election ~save:false uuid in
        let* () = Api_drafts.delete_draft uuid in
        Redirection.send (Redirection admin)
      )

  let () =
    Any.register ~service:election_draft_import
      (fun uuid () ->
        let@ _, account, _ = with_site_user in
        let@ se = with_draft_election_ro uuid in
        let* _, a, b, c = get_elections_by_owner_sorted account.account_id in
        Pages_admin.election_draft_import uuid se (a, b, c) ()
        >>= Html.send
      )

  let () =
    Any.register ~service:election_draft_import_post
      (fun uuid from_s ->
        let from = uuid_of_raw_string from_s in
        let@ se = with_draft_election ~save:false uuid in
        let@ _ = with_metadata_check_owner from in
        let* l = get_preferred_gettext () in
        let open (val l) in
        let* x = Api_drafts.import_voters uuid se from in
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
                (f_ "The total weight (%s) cannot be handled. Its expanded value must be less than %s.")
                Weight.(to_string total_weight)
                (Z.to_string Weight.max_expanded_weight)
             ) ()
           >>= Html.send
        | Error (`Duplicate x) ->
           Pages_common.generic_page ~title:(s_ "Error")
             ~service:(preapply ~service:election_draft_voters uuid)
             (Printf.sprintf (f_ "Duplicate voter: %s. This is not allowed. If two voters have the same address, use different logins.") x) ()
           >>= Html.send
      )

  let () =
    Any.register ~service:election_draft_import_trustees
      (fun uuid () ->
        let@ _, account, _ = with_site_user in
        let@ se = with_draft_election_ro uuid in
        let* _, a, b, c = get_elections_by_owner_sorted account.account_id in
        Pages_admin.election_draft_import_trustees uuid se (a, b, c) ()
        >>= Html.send
      )

  let () =
    Any.register ~service:election_draft_import_trustees_post
      (fun uuid from ->
        let from = uuid_of_raw_string from in
        let@ se = with_draft_election ~save:false uuid in
        let@ _ = with_metadata_check_owner from in
        let* metadata = Web_persist.get_election_metadata from in
        let* x = Api_drafts.import_trustees uuid se from metadata in
        match x with
        | Ok `Basic -> redir_preapply election_draft_trustees uuid ()
        | Ok `Threshold -> redir_preapply election_draft_threshold_trustees uuid ()
        | Stdlib.Error e ->
           let* l = get_preferred_gettext () in
           let open (val l) in
           let msg =
             match e with
             | `None -> s_ "Could not retrieve trustees from selected election!"
             | `Invalid -> s_ "Imported trustees are invalid for this election!"
             | `Inconsistent -> s_ "Inconsistency in imported election!"
             | `MissingPrivateKeys -> s_ "Encrypted decryption keys are missing!"
             | `Unsupported -> s_ "Unsupported trustees!"
           in
           Pages_common.generic_page ~title:(s_ "Error")
             ~service:(preapply ~service:election_draft_trustees uuid)
             msg ()
           >>= Html.send
      )

  let election_admin_handler ?shuffle_token ?tally_token uuid =
    let* l = get_preferred_gettext () in
    let open (val l) in
    let@ election = with_election uuid in
    let* metadata = Web_persist.get_election_metadata uuid in
    let* site_user = Eliom_reference.get Web_state.site_user in
    match site_user with
    | Some x when (match metadata.e_owner with None -> false | Some o -> Accounts.check x o) ->
       let* status = Api_elections.get_election_status uuid in
       let module W = (val election) in
       let* pending_server_shuffle =
         match status.status_state with
         | `Shuffling ->
            if Election.has_nh_questions W.election then
              let* x = Web_persist.get_shuffles uuid in
              match x with
              | None -> return_true
              | Some _ -> return_false
            else return_false
         | _ -> return_false
       in
       let* () =
         if pending_server_shuffle then (
           let* cc = Web_persist.get_nh_ciphertexts election in
           let cc = nh_ciphertexts_of_string W.G.read cc in
           let* shuffle = W.E.shuffle_ciphertexts cc in
           let shuffle = string_of_shuffle W.G.write shuffle in
           let* x = Web_persist.append_to_shuffles election shuffle in
           match x with
           | Some h ->
              let sh = {sh_trustee = "server"; sh_hash = h; sh_name = Some "server"} in
              let* () = Web_persist.add_shuffle_hash uuid sh in
              Web_persist.remove_audit_cache uuid
           | None ->
              Lwt.fail (Failure (Printf.sprintf (f_ "Automatic shuffle by server has failed for election %s!") (raw_string_of_uuid uuid)))
         ) else return_unit
       in
       Pages_admin.election_admin ?shuffle_token ?tally_token election metadata status () >>= Html.send
    | Some _ ->
       let msg = s_ "You are not allowed to administer this election!" in
       Pages_common.generic_page ~title:(s_ "Forbidden") msg ()
       >>= Html.send ~code:403
    | _ ->
       redir_preapply site_login (None, ContSiteElection uuid) ()

  let () =
    Any.register ~service:election_admin
      (fun uuid () -> election_admin_handler uuid)

  let election_set_state state uuid () =
    let@ _ = with_metadata_check_owner uuid in
    let set = Api_elections.(if state then open_election else close_election) in
    let* b = set uuid in
    if b then redir_preapply election_admin uuid () else forbidden ()

  let () = Any.register ~service:election_open (election_set_state true)
  let () = Any.register ~service:election_close (election_set_state false)

  let election_set_result_hidden uuid date =
    let@ _ = with_metadata_check_owner uuid in
    let* b = Api_elections.set_postpone_date uuid date in
    if b then (
      redir_preapply election_admin uuid ()
    ) else (
      let* l = get_preferred_gettext () in
      let open (val l) in
      let service = preapply ~service:election_admin uuid in
      let msg = Printf.sprintf (f_ "The date must be less than %d days in the future!") days_to_publish_result in
      Pages_common.generic_page ~title:(s_ "Error") ~service msg () >>= Html.send
    )

  let parse_datetime_from_post l x =
    let open (val l : Belenios_ui.I18n.GETTEXT) in
    try raw_datetime_of_string x with
    | _ -> Printf.ksprintf failwith (f_ "%s is not a valid date!") x

  let () =
    Any.register ~service:election_hide_result
      (fun uuid date ->
        let@ date = fun cont ->
          match Option.wrap raw_datetime_of_string date with
          | None ->
             let* l = get_preferred_gettext () in
             let open (val l) in
             let service = preapply ~service:election_admin uuid in
             let msg = Printf.sprintf (f_ "%s is not a valid date!") date in
             Pages_common.generic_page ~title:(s_ "Error") ~service msg () >>= Html.send
          | Some t ->
             cont @@ unixfloat_of_datetime t
        in
        election_set_result_hidden uuid (Some date)
      )

  let () =
    Any.register ~service:election_show_result
      (fun uuid () -> election_set_result_hidden uuid None)

  let () =
    Any.register ~service:election_auto_post
      (fun uuid (auto_open, auto_close) ->
        let@ _ = with_metadata_check_owner uuid in
        let* l = get_preferred_gettext () in
        let open (val l) in
        let auto_dates =
          try
            let format x =
              if x = "" then None
              else Some (parse_datetime_from_post l x)
            in
            Ok (format auto_open, format auto_close)
          with Failure e -> Error e
        in
        match auto_dates with
        | Ok (e_auto_open, e_auto_close) ->
           let open Belenios_api.Serializable_t in
           let dates =
             {
               auto_date_open = Option.map unixfloat_of_datetime e_auto_open;
               auto_date_close = Option.map unixfloat_of_datetime e_auto_close;
             }
           in
           let* () = Api_elections.set_election_auto_dates uuid dates in
           redir_preapply election_admin uuid ()
        | Error msg ->
           let service = preapply ~service:election_admin uuid in
           Pages_common.generic_page ~title:(s_ "Error") ~service msg () >>= Html.send
      )

  let () =
    Any.register ~service:election_delete
      (fun uuid () ->
        let@ _ = with_metadata_check_owner uuid in
        let* () = delete_election uuid in
        redir_preapply admin () ()
      )

  let () =
    let rex = Pcre.regexp "\".*\" \".*:(.*)\"" in
    Any.register ~service:election_missing_voters
      (fun (uuid, ()) () ->
        let@ _ = with_metadata_check_owner uuid in
        let* voters =
          let* file = read_file ~uuid (string_of_election_file ESVoters) in
          match file with
          | Some vs ->
             return (
                 List.fold_left (fun accu v ->
                     let _, login, _ = split_identity v in
                     SSet.add (PString.lowercase_ascii login) accu
                   ) SSet.empty vs
               )
          | None -> return SSet.empty
        in
        let* voters =
          let* file = read_file ~uuid (string_of_election_file ESRecords) in
          match file with
          | Some rs ->
             return (
                 List.fold_left (fun accu r ->
                     let s = Pcre.exec ~rex r in
                     let v = Pcre.get_substring s 1 in
                     SSet.remove (PString.lowercase_ascii v) accu
                   ) voters rs
               )
          | None -> return voters
        in
        let buf = Buffer.create 128 in
        SSet.iter (fun v ->
            Buffer.add_string buf v;
            Buffer.add_char buf '\n'
          ) voters;
        let* x = String.send (Buffer.contents buf, "text/plain") in
        return @@ Eliom_registration.cast_unknown_content_kind x
      )

  let () =
    Any.register ~service:election_pretty_records
      (fun (uuid, ()) () ->
        let@ _ = with_metadata_check_owner uuid in
        let@ election = with_election uuid in
        let* records = Api_elections.get_records uuid in
        Pages_admin.pretty_records election records () >>= Html.send
      )

  let () =
    Any.register ~service:election_project_result
      (fun ((uuid, ()), index) () ->
        if index < 0 then (
          fail_http `Not_found
        ) else (
          let* hidden =
            let* x = Web_persist.get_election_result_hidden uuid in
            match x with
            | None -> return_false
            | Some _ -> return_true
          in
          let* allow =
            if hidden then (
              let* metadata = Web_persist.get_election_metadata uuid in
              let* site_user = Eliom_reference.get Web_state.site_user in
              match site_user with
              | Some x when (match metadata.e_owner with None -> false | Some o -> Accounts.check x o) -> return_true
              | _ -> return_false
            ) else return_true
          in
          if allow then (
            let* result = Web_persist.get_election_result uuid in
            match result with
            | None -> fail_http `Not_found
            | Some result ->
               let result =
                 election_result_of_string
                   Yojson.Safe.read_json Yojson.Safe.read_json
                   Yojson.Safe.read_json Yojson.Safe.read_json
                   result
               in
               match result.result with
               | `List xs ->
                  (match List.nth_opt xs index with
                   | None -> fail_http `Not_found
                   | Some x ->
                      let* x = String.send (Yojson.Safe.to_string x, "application/json") in
                      return @@ Eliom_registration.cast_unknown_content_kind x
                  )
               | _ -> fail_http `Not_found
          ) else forbidden ()
        )
      )

  let copy_file src dst =
    let open Lwt_io in
    chars_of_file src |> chars_to_file dst

  let try_copy_file src dst =
    let* b = file_exists src in
    if b then copy_file src dst else return_unit

  let make_archive uuid =
    let uuid_s = raw_string_of_uuid uuid in
    let* temp_dir =
      Lwt_preemptive.detach (fun () ->
          let temp_dir = Filename.temp_file "belenios" "archive" in
          Sys.remove temp_dir;
          Unix.mkdir temp_dir 0o700;
          Unix.mkdir (temp_dir // "public") 0o755;
          Unix.mkdir (temp_dir // "restricted") 0o700;
          temp_dir
        ) ()
    in
    let* () =
      Lwt_list.iter_p (fun x ->
          try_copy_file (uuid /// x) (temp_dir // "public" // x)
        ) [
          "election.json";
          "trustees.json";
          "public_creds.txt";
          "ballots.jsons";
          "result.json";
        ]
    in
    let* () =
      Lwt_list.iter_p (fun x ->
          try_copy_file (uuid /// x) (temp_dir // "restricted" // x)
        ) [
          "voters.txt";
          "records";
        ]
    in
    let command =
      Printf.ksprintf Lwt_process.shell
        "cd \"%s\" && zip -r archive public restricted" temp_dir
    in
    let* r = Lwt_process.exec command in
    match r with
    | Unix.WEXITED 0 ->
       let fname = uuid /// "archive.zip" in
       let fname_new = fname ^ ".new" in
       let* () = copy_file (temp_dir // "archive.zip") fname_new in
       let* () = Lwt_unix.rename fname_new fname in
       rmdir temp_dir
    | _ ->
       Printf.ksprintf Ocsigen_messages.errlog
         "Error while creating archive.zip for election %s, temporary directory left in %s"
         uuid_s temp_dir;
       return_unit

  let () =
    Any.register ~service:election_download_archive
      (fun (uuid, ()) () ->
        let@ _ = with_metadata_check_owner uuid in
        let* l = get_preferred_gettext () in
        let open (val l) in
        let* state = Web_persist.get_election_state uuid in
        if state = `Archived then (
          let archive_name = uuid /// "archive.zip" in
          let* b = file_exists archive_name in
          let* () = if not b then make_archive uuid else return_unit in
          File.send ~content_type:"application/zip" archive_name
        ) else (
          let service = preapply ~service:election_admin uuid in
          Pages_common.generic_page ~title:(s_ "Error") ~service
            (s_ "The election is not archived!") () >>= Html.send
        )
      )

  let find_trustee_id uuid token =
    let* x = Web_persist.get_decryption_tokens uuid in
    match x with
    | None -> return (int_of_string_opt token)
    | Some tokens ->
       let rec find i = function
         | [] -> None
         | t :: ts -> if t = token then Some i else find (i+1) ts
       in
       return (find 1 tokens)

  let () =
    Any.register ~service:election_tally_trustees
      (fun (uuid, token) () ->
        let@ () =
          without_site_user
            ~fallback:(fun _ ->
              election_admin_handler ~tally_token:token uuid
            ) ()
        in
        let* l = get_preferred_gettext () in
        let open (val l) in
        let* state = Web_persist.get_election_state uuid in
        match state with
        | `EncryptedTally _ ->
           let* x = find_trustee_id uuid token in
           (match x with
            | Some trustee_id ->
               let* pds = Web_persist.get_partial_decryptions uuid in
               if List.mem_assoc trustee_id pds then (
                 Pages_common.generic_page ~title:(s_ "Error")
                   (s_ "Your partial decryption has already been received and checked!")
                   () >>= Html.send
               ) else (
                 Printf.sprintf "%s/election/trustees.html#%s-%s"
                   !Web_config.prefix (raw_string_of_uuid uuid) token
                 |> String_redirection.send
               )
            | None -> forbidden ()
           )
        | `Open | `Closed | `Shuffling ->
           let msg = s_ "The election is not ready to be tallied. Please come back later." in
           Pages_common.generic_page ~title:(s_ "Forbidden") msg () >>= Html.send ~code:403
        | `Tallied | `Archived ->
           let msg = s_ "The election has already been tallied." in
           Pages_common.generic_page ~title:(s_ "Forbidden") msg () >>= Html.send ~code:403
      )

  let () =
    Html.register ~service:election_tally_trustees_static
      (fun () () -> Pages_admin.tally_trustees_static ())

  exception TallyEarlyError

  let render_tally_early_error_as_forbidden f =
    Lwt.catch f
      (function
       | TallyEarlyError -> forbidden ()
       | e -> Lwt.fail e)

  let () =
    Any.register ~service:election_tally_trustees_post
      (fun (uuid, token) partial_decryption ->
        let@ () = render_tally_early_error_as_forbidden in
        let* l = get_preferred_gettext () in
        let open (val l) in
        let* () =
          let* state = Web_persist.get_election_state uuid in
          match state with
          | `EncryptedTally _ -> return ()
          | _ -> Lwt.fail TallyEarlyError
        in
        let* trustee_id =
          let* x = find_trustee_id uuid token in
          match x with
          | Some x -> return x
          | None -> Lwt.fail TallyEarlyError
        in
        let* pds = Web_persist.get_partial_decryptions uuid in
        let* () =
          if List.mem_assoc trustee_id pds then Lwt.fail TallyEarlyError else return ()
        in
        let* () =
          if trustee_id > 0 then return () else fail_http `Not_found
        in
        let@ election = with_election uuid in
        let module W = (val election) in
        let* pks =
          let* trustees = Web_persist.get_trustees uuid in
          let trustees = trustees_of_string W.G.read trustees in
          trustees
          |> List.map
               (function
                | `Single x -> [x]
                | `Pedersen t -> Array.to_list t.t_verification_keys
               )
          |> List.flatten
          |> Array.of_list
          |> return
        in
        let pk = pks.(trustee_id-1).trustee_public_key in
        let pd = partial_decryption_of_string W.G.read partial_decryption in
        let et = uuid /// string_of_election_file ESETally in
        let* et = Lwt_io.chars_of_file et |> Lwt_stream.to_string in
        let et = encrypted_tally_of_string W.G.read et in
        if string_of_partial_decryption W.G.write pd = partial_decryption && W.E.check_factor et pk pd then (
          let pd = trustee_id, partial_decryption in
          let* () = Web_persist.add_partial_decryption uuid pd in
          Pages_common.generic_page ~title:(s_ "Success")
            (s_ "Your partial decryption has been received and checked!") () >>=
            Html.send
        ) else (
          let service = preapply ~service:election_tally_trustees (uuid, token) in
          Pages_common.generic_page ~title:(s_ "Error") ~service
            (s_ "The partial decryption didn't pass validation!") () >>=
            Html.send
      ))

  let handle_election_tally_release uuid () =
    let@ () = render_tally_early_error_as_forbidden in
    let@ _ = with_metadata_check_owner uuid in
    let* l = get_preferred_gettext () in
    let open (val l) in
    let@ election = with_election uuid in
    let* x = Api_elections.release_tally election in
    match x with
    | Ok () -> redir_preapply election_home (uuid, ()) ()
    | Error `Forbidden -> forbidden ()
    | Error (`CombinationError e) ->
       let msg =
         Printf.sprintf
           (f_ "An error occurred while computing the result (%s). Most likely, it means that some trustee has not done his/her job.")
           (Trustees.string_of_combination_error e)
       in
       Pages_common.generic_page ~title:(s_ "Error") msg () >>= Html.send

  let () =
    Any.register ~service:election_tally_release
      handle_election_tally_release

  let handle_api_elections_return uuid metadata = function
    | false -> forbidden ()
    | true ->
       let* state = Web_persist.get_election_state uuid in
       match state with
       | `EncryptedTally _ ->
          let trustees = Option.value metadata.e_trustees ~default:[] in
          if List.exists (fun x -> x <> "server") trustees then
            redir_preapply election_admin uuid ()
          else
            handle_election_tally_release uuid ()
       | _ -> redir_preapply election_admin uuid ()

  let () =
    Any.register ~service:election_compute_encrypted_tally
      (fun uuid () ->
        let@ metadata = with_metadata_check_owner uuid in
        let@ election = with_election uuid in
        let* x = Api_elections.compute_encrypted_tally election metadata in
        handle_api_elections_return uuid metadata x
      )

  let () =
    Any.register ~service:election_shuffle_link
      (fun (uuid, token) () ->
        let@ () =
          without_site_user
            ~fallback:(fun _ ->
              election_admin_handler ~shuffle_token:token uuid
            ) ()
        in
        let* expected_token = Web_persist.get_shuffle_token uuid in
        match expected_token with
        | Some x when token = x.tk_token ->
           Printf.sprintf "%s/election/shuffle.html#%s-%s"
             !Web_config.prefix (raw_string_of_uuid uuid) token
           |> String_redirection.send
        | _ -> forbidden ()
      )

  let () =
    Html.register ~service:election_shuffle_link_static
      (fun () () -> Pages_admin.shuffle_static ())

  let () =
    Any.register ~service:election_shuffle_post
      (fun (uuid, token) shuffle ->
        let@ election = with_election uuid in
        let@ () = without_site_user () in
        let* l = get_preferred_gettext () in
        let open (val l) in
        let* expected_token = Web_persist.get_shuffle_token uuid in
        match expected_token with
        | Some x when token = x.tk_token ->
           Lwt.catch
             (fun () ->
               let* y = Web_persist.append_to_shuffles election shuffle in
               match y with
               | Some h ->
                  let* () = Web_persist.clear_shuffle_token uuid in
                  let sh = {sh_trustee = x.tk_trustee; sh_hash = h; sh_name = x.tk_name} in
                  let* () = Web_persist.add_shuffle_hash uuid sh in
                  let* () = Web_persist.remove_audit_cache uuid in
                  Pages_common.generic_page ~title:(s_ "Success") (s_ "The shuffle has been successfully applied!") () >>= Html.send
               | None ->
                  Pages_common.generic_page ~title:(s_ "Error") (s_ "An error occurred while applying the shuffle.") () >>= Html.send
             )
             (fun e ->
               Pages_common.generic_page ~title:(s_ "Error") (Printf.sprintf (f_ "Data is invalid! (%s)") (Printexc.to_string e)) () >>= Html.send
             )
        | _ -> forbidden ()
      )

  let () =
    Any.register ~service:election_shuffler_select
      (fun () (uuid, trustee) ->
        let@ metadata = with_metadata_check_owner uuid in
        let* () = Api_elections.select_shuffler uuid metadata trustee in
        redir_preapply election_admin uuid ()
      )

  let () =
    Any.register ~service:election_shuffler_skip_confirm
      (fun () (uuid, trustee) ->
        let@ _ = with_metadata_check_owner uuid in
        Pages_admin.election_shuffler_skip_confirm uuid trustee >>= Html.send
      )

  let () =
    Any.register ~service:election_shuffler_skip
      (fun () (uuid, trustee) ->
        let@ metadata = with_metadata_check_owner uuid in
        let* () = Api_elections.skip_shuffler uuid metadata trustee in
        redir_preapply election_admin uuid ()
      )

  let () =
    Any.register ~service:election_decrypt (fun uuid () ->
        let@ metadata = with_metadata_check_owner uuid in
        let@ election = with_election uuid in
        let* x = Api_elections.finish_shuffling election metadata in
        handle_api_elections_return uuid metadata x
      )

  let () =
    Any.register ~service:election_draft_threshold_set
      (fun uuid threshold ->
        let@ se = with_draft_election ~save:false uuid in
        let* l = get_preferred_gettext () in
        let open (val l) in
        let* x = Api_drafts.set_threshold uuid se threshold in
        match x with
        | Ok () -> redir_preapply election_draft_threshold_trustees uuid ()
        | Error `NoTrustees ->
           let msg = s_ "Please add some trustees first!" in
           let service = preapply ~service:election_draft_threshold_trustees uuid in
           Pages_common.generic_page ~title:(s_ "Error") ~service msg () >>= Html.send
        | Error `OutOfBounds ->
           let msg = s_ "The threshold must be positive and smaller than the number of trustees!" in
           let service = preapply ~service:election_draft_threshold_trustees uuid in
           Pages_common.generic_page ~title:(s_ "Error") ~service msg () >>= Html.send
      )

  let () =
    Any.register ~service:election_draft_threshold_trustee_add
      (handle_trustee_add (`Threshold 0))

  let () =
    Any.register ~service:election_draft_threshold_trustee_del
      (handle_trustee_del election_draft_threshold_trustees)

  let () =
    Any.register ~service:election_draft_threshold_trustee
      (fun (uuid, token) () ->
        let@ () =
          without_site_user
            ~fallback:(fun u ->
              let* election = Web_persist.get_draft_election uuid in
              match election with
              | None -> fail_http `Not_found
              | Some se ->
                 if Accounts.check u se.se_owner then (
                   Pages_admin.election_draft_threshold_trustees ~token uuid se () >>= Html.send
                 ) else forbidden ()
            ) ()
        in
        let* election = Web_persist.get_draft_election uuid in
        match election with
        | None -> fail_http `Not_found
        | Some _ ->
           Printf.sprintf "%s/draft/threshold-trustee.html#%s-%s"
             !Web_config.prefix (raw_string_of_uuid uuid) token
           |> String_redirection.send
      )

  let () =
    Html.register ~service:election_draft_threshold_trustee_static
      (fun () () -> Pages_admin.election_draft_threshold_trustee_static ())

  let wrap_handler_without_site_user f =
    without_site_user () (fun () -> wrap_handler f)

  let () =
    Any.register ~service:election_draft_threshold_trustee_post
      (fun (uuid, token) data ->
        let@ () = wrap_handler_without_site_user in
        let* () =
          Web_election_mutex.with_lock uuid
            (fun () ->
              let* election = Web_persist.get_draft_election uuid in
              match election with
              | None -> fail_http `Not_found
              | Some se ->
                 let ts =
                   match se.se_threshold_trustees with
                   | None -> failwith "No threshold trustees"
                   | Some xs -> Array.of_list xs
                 in
                 let i, t =
                   match Array.findi (fun i x ->
                             if token = x.stt_token then Some (i, x) else None
                           ) ts with
                   | Some (i, t) -> i, t
                   | None -> failwith "Trustee not found"
                 in
                 let get_certs () =
                   let certs = Array.map (fun x ->
                                   match x.stt_cert with
                                   | None -> failwith "Missing certificate"
                                   | Some y -> y
                                 ) ts in
                   {certs}
                 in
                 let get_polynomials () =
                   Array.map (fun x ->
                       match x.stt_polynomial with
                       | None -> failwith "Missing polynomial"
                       | Some y -> y
                     ) ts
                 in
                 let version = Option.value se.se_version ~default:0 in
                 let module G = (val Group.of_string ~version se.se_group : GROUP) in
                 let module Trustees = (val Trustees.get_by_version (Option.value se.se_version ~default:0)) in
                 let module P = Trustees.MakePKI (G) (LwtRandom) in
                 let module C = Trustees.MakeChannels (G) (LwtRandom) (P) in
                 let module K = Trustees.MakePedersen (G) (LwtRandom) (P) (C) in
                 let* () =
                   match t.stt_step with
                   | Some 1 ->
                      let cert = cert_of_string data in
                      if K.step1_check cert then (
                        t.stt_cert <- Some cert;
                        t.stt_step <- Some 2;
                        return_unit
                      ) else (
                        failwith "Invalid certificate"
                      )
                   | Some 3 ->
                      let certs = get_certs () in
                      let polynomial = polynomial_of_string data in
                      if K.step3_check certs i polynomial then (
                        t.stt_polynomial <- Some polynomial;
                        t.stt_step <- Some 4;
                        return_unit
                      ) else (
                        failwith "Invalid polynomial"
                      )
                   | Some 5 ->
                      let certs = get_certs () in
                      let polynomials = get_polynomials () in
                      let voutput = voutput_of_string G.read data in
                      if K.step5_check certs i polynomials voutput then (
                        t.stt_voutput <- Some data;
                        t.stt_step <- Some 6;
                        return_unit
                      ) else (
                        failwith "Invalid voutput"
                      )
                   | _ -> failwith "Unknown step"
                 in
                 let* () =
                   if Array.for_all (fun x -> x.stt_step = Some 2) ts then (
                     (try
                        K.step2 (get_certs ());
                        Array.iter (fun x -> x.stt_step <- Some 3) ts;
                      with e ->
                        se.se_threshold_error <- Some (Printexc.to_string e)
                     ); return_unit
                   ) else return_unit
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
                      with e ->
                        se.se_threshold_error <- Some (Printexc.to_string e)
                     ); return_unit
                   ) else return_unit
                 in
                 let* () =
                   if Array.for_all (fun x -> x.stt_step = Some 6) ts then (
                     (try
                        let certs = get_certs () in
                        let polynomials = get_polynomials () in
                        let voutputs = Array.map (fun x ->
                                           match x.stt_voutput with
                                           | None -> failwith "Missing voutput"
                                           | Some y -> voutput_of_string G.read y
                                         ) ts in
                        let p = K.step6 certs polynomials voutputs in
                        se.se_threshold_parameters <- Some (string_of_threshold_parameters G.write p);
                        Array.iter (fun x -> x.stt_step <- Some 7) ts
                      with e ->
                        se.se_threshold_error <- Some (Printexc.to_string e)
                     ); return_unit
                   ) else return_unit
                 in
                 Web_persist.set_draft_election uuid se
            )
        in
        redir_preapply election_draft_threshold_trustee (uuid, token) ()
      )

  module HashedInt = struct
    type t = int
    let equal = (=)
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
      Pages_common.generic_page ~title:(s_ "Account creation") ~service
        (s_ "You cannot create an account now. Please try later.") ()

  let () =
    Html.register ~service:signup_captcha
      (fun service () ->
        let* b = Eliom_reference.get Web_state.show_cookie_disclaimer in
        if b then
          Pages_admin.privacy_notice (ContSignup service)
        else
          signup_captcha_handler service None ""
      )

  let () =
    Html.register ~service:signup_captcha_post
      (fun service (challenge, (response, email)) ->
        let* l = get_preferred_gettext () in
        let open (val l) in
        let* error =
          let* ok = Web_captcha.check_captcha ~challenge ~response in
          if ok then
            if is_email email then return_none else return_some BadAddress
          else return_some BadCaptcha
        in
        match error with
        | None ->
           let* () = Web_signup.send_confirmation_link l ~service email in
           let* () = Eliom_reference.set Web_state.signup_address (Some email) in
           Pages_admin.signup_login ()
        | _ -> signup_captcha_handler service error email
      )

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
        (s_ "You cannot change your password now. Please try later.") ()

  let () =
    Html.register ~service:changepw_captcha
      (fun service () -> changepw_captcha_handler service None "" "")

  let () =
    Html.register ~service:changepw_captcha_post
      (fun service (challenge, (response, (email, username))) ->
        let* l = get_preferred_gettext () in
        let open (val l) in
        let* error =
          let* ok = Web_captcha.check_captcha ~challenge ~response in
          if ok then return_none
          else return_some BadCaptcha
        in
        match error with
        | None ->
           let* () =
             let* x = Web_auth_password.lookup_account ~service ~email ~username in
             match x with
             | None ->
                return (
                    Printf.ksprintf Ocsigen_messages.warning
                      "Unsuccessful attempt to change the password of %S (%S) for service %s"
                      username email service
                  )
             | Some (username, address) ->
                let* () = Eliom_reference.set Web_state.signup_address (Some address) in
                Web_signup.send_changepw_link l ~service ~address ~username
           in
           Pages_admin.signup_login ()
        | _ -> changepw_captcha_handler service error email username
      )

  let () =
    Any.register ~service:signup_login_post
      (fun () code ->
        let code = PString.trim code in
        let* address = Eliom_reference.get Web_state.signup_address in
        match address with
        | None -> forbidden ()
        | Some address ->
           let* x = Web_signup.confirm_link address in
           match x with
           | Some (code2, service, kind) when code = code2 ->
              let* () = Eliom_reference.set Web_state.signup_env (Some (service, kind)) in
              redir_preapply signup () ()
           | _ -> forbidden ()
      )

  let () =
    Any.register ~service:signup
      (fun () () ->
        let* address = Eliom_reference.get Web_state.signup_address in
        let* x = Eliom_reference.get Web_state.signup_env in
        match address, x with
        | Some address, Some (_, `CreateAccount) -> Pages_admin.signup address None "" >>= Html.send
        | Some address, Some (_, `ChangePassword username) -> Pages_admin.changepw ~username ~address None >>= Html.send
        | _ -> forbidden ()
      )

  let () =
    Any.register ~service:signup_post
      (fun () (username, (password, password2)) ->
        let* l = get_preferred_gettext () in
        let open (val l) in
        let* address = Eliom_reference.get Web_state.signup_address in
        let* x = Eliom_reference.get Web_state.signup_env in
        match address, x with
        | Some email, Some (service, `CreateAccount) ->
           if password = password2 then (
             let user = { user_name = username; user_domain = service } in
             let* x = Web_auth_password.add_account user ~password ~email in
             match x with
             | Ok () ->
                let* () = Web_signup.remove_link email in
                let* () = Eliom_reference.unset Web_state.signup_address in
                let* () = Eliom_reference.unset Web_state.signup_env in
                let service = preapply ~service:site_login (Some service, ContSiteAdmin) in
                Pages_common.generic_page ~title:(s_ "Account creation") ~service (s_ "The account has been created.") ()
                >>= Html.send
             | Error e -> Pages_admin.signup email (Some e) username >>= Html.send
           ) else Pages_admin.signup email (Some PasswordMismatch) username >>= Html.send
        | _ -> forbidden ()
      )

  let () =
    Any.register ~service:changepw_post
      (fun () (password, password2) ->
        let* l = get_preferred_gettext () in
        let open (val l) in
        let* address = Eliom_reference.get Web_state.signup_address in
        let* x = Eliom_reference.get Web_state.signup_env in
        match address, x with
        | Some address, Some (service, `ChangePassword username) ->
           if password = password2 then (
             let user = { user_name = username; user_domain = service } in
             let* x = Web_auth_password.change_password user ~password in
             match x with
             | Ok () ->
                let* () = Web_signup.remove_link address in
                let* () = Eliom_reference.unset Web_state.signup_address in
                let* () = Eliom_reference.unset Web_state.signup_env in
                let service = preapply ~service:site_login (Some service, ContSiteAdmin) in
                Pages_common.generic_page ~title:(s_ "Change password") ~service (s_ "The password has been changed.") ()
                >>= Html.send
             | Error e -> Pages_admin.changepw ~username ~address (Some e) >>= Html.send
           ) else Pages_admin.changepw ~username ~address (Some PasswordMismatch) >>= Html.send
        | _ -> forbidden ()
      )

  let () =
    Html.register ~service:compute_fingerprint
      (fun () () -> Pages_admin.compute_fingerprint ())

  let has_sudo_capability f =
    let* x = Eliom_reference.get Web_state.site_user in
    match x with
    | Some (_, a, token) when Accounts.(has_capability Sudo a) -> f token
    | _ -> forbidden ()

  let () =
    Any.register ~service:sudo
      (fun () () ->
        let@ _ = has_sudo_capability in
        Pages_admin.sudo () >>= Html.send
      )

  let () =
    Any.register ~service:sudo_post
      (fun () (user_domain, user_name) ->
        let@ token = has_sudo_capability in
        let u = {user_domain; user_name} in
        let* x = Accounts.get_account u in
        match x with
        | None ->
           let* l = get_preferred_gettext () in
           let open (val l) in
           let msg = s_ "This account does not exist" in
           let title = s_ "Account not found" in
           Pages_common.generic_page ~title ~service:sudo msg ()
           >>= Html.send
        | Some a ->
           let () = Api_generic.invalidate_token token in
           let* token = Api_generic.new_token a in
           let* () = Eliom_reference.set Web_state.site_user (Some (u, a, token)) in
           Redirection.send (Redirection admin)
      )

  let with_user_and_account f =
    let* x = Eliom_reference.get Web_state.site_user in
    match x with
    | Some x -> f x
    | None -> forbidden ()

  let () =
    Any.register ~service:account
      (fun () () ->
        let@ _, a, _ = with_user_and_account in
        Pages_admin.account a >>= Html.send
      )

  let () =
    Any.register ~service:account_post
      (fun () account_name ->
        let@ _, a, _ = with_user_and_account in
        let a = {a with account_name} in
        let* () = Accounts.update_account a in
        Redirection.send (Redirection admin)
      )

  let () =
    Any.register ~service:api_token
      (fun () () ->
        let* x = Eliom_reference.get Web_state.site_user in
        let code, content =
          match x with
          | None -> 403, "Forbidden"
          | Some (_, _, token) -> 200, token
        in
        String.send ~code (content, "text/plain")
      )

  let extract_automatic_data_draft uuid_s =
    let uuid = uuid_of_raw_string uuid_s in
    let* election = Web_persist.get_draft_election uuid in
    match election with
    | None -> return_none
    | Some se ->
       let t = Option.value se.se_creation_date ~default:default_creation_date in
       let next_t = datetime_add t (day days_to_delete) in
       return_some (`Destroy, uuid, next_t)

  let extract_automatic_data_validated uuid_s =
    let uuid = uuid_of_raw_string uuid_s in
    let* election = Web_persist.get_raw_election uuid in
    match election with
    | None -> return_none
    | Some _ ->
       let* state = Web_persist.get_election_state uuid in
       let* dates = Web_persist.get_election_dates uuid in
       match state with
       | `Open | `Closed | `Shuffling | `EncryptedTally _ ->
          let t = Option.value dates.e_finalization ~default:default_validation_date in
          let next_t = datetime_add t (day days_to_delete) in
          return_some (`Delete, uuid, next_t)
       | `Tallied ->
          let t = Option.value dates.e_tally ~default:default_tally_date in
          let next_t = datetime_add t (day days_to_archive) in
          return_some (`Archive, uuid, next_t)
       | `Archived ->
          let t = Option.value dates.e_archive ~default:default_archive_date in
          let next_t = datetime_add t (day days_to_delete) in
          return_some (`Delete, uuid, next_t)

  let try_extract extract x =
    Lwt.catch
      (fun () -> extract x)
      (fun _ -> return_none)

  let get_next_actions () =
    Lwt_unix.files_of_directory !Web_config.spool_dir |>
      Lwt_stream.to_list >>=
      Lwt_list.filter_map_s
        (fun x ->
          if x = "." || x = ".." then return_none
          else (
            let* r = try_extract extract_automatic_data_draft x in
            match r with
            | None -> try_extract extract_automatic_data_validated x
            | x -> return x
          )
        )

  let process_election_for_data_policy (action, uuid, next_t) =
    let uuid_s = raw_string_of_uuid uuid in
    let now = now () in
    let action, comment = match action with
      | `Destroy -> Api_drafts.delete_draft, "destroyed"
      | `Delete -> delete_election, "deleted"
      | `Archive -> Api_elections.archive_election, "archived"
    in
    if datetime_compare now next_t > 0 then (
      let* () = action uuid in
      return (
          Printf.ksprintf Ocsigen_messages.warning
            "Election %s has been automatically %s" uuid_s comment
        )
    ) else return_unit

  let rec data_policy_loop () =
    let open Ocsigen_messages in
    let () = accesslog "Data policy process started" in
    let* elections = get_next_actions () in
    let* () = Lwt_list.iter_s process_election_for_data_policy elections in
    let () = accesslog "Data policy process completed" in
    let* () = Lwt_unix.sleep 3600. in
    data_policy_loop ()

end
