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

open Lwt.Syntax
open Belenios
open Belenios_server_core

let mailer =
  match Sys.getenv_opt "BELENIOS_SENDMAIL" with
  | None -> "/usr/lib/sendmail"
  | Some x -> x

let sendmail ?return_path message =
  let mailer =
    match return_path with
    | None -> mailer
    | Some x -> Printf.sprintf "%s -f %s" mailer x
  in
  Netsendmail.sendmail ~mailer message

type mail_kind =
  | MailCredential of uuid
  | MailPassword of uuid
  | MailConfirmation of uuid
  | MailAccountCreation
  | MailPasswordChange
  | MailLogin
  | MailSetEmail

let stringuuid_of_mail_kind = function
  | MailCredential uuid -> ("credential", Some uuid)
  | MailPassword uuid -> ("password", Some uuid)
  | MailConfirmation uuid -> ("confirmation", Some uuid)
  | MailAccountCreation -> ("account-creation", None)
  | MailPasswordChange -> ("password-change", None)
  | MailLogin -> ("login", None)
  | MailSetEmail -> ("set-email", None)

type t =
  | Account_create of {
      lang : string;
      recipient : string * string;
      code : string;
    }
  | Account_change_password of {
      lang : string;
      recipient : string * string;
      code : string;
    }
  | Account_set_email of {
      lang : string;
      recipient : string * string;
      code : string;
    }
  | Voter_password of password_email
  | Voter_credential of credential_email
  | Vote_confirmation of {
      lang : string;
      uuid : uuid;
      title : string;
      contact : string option;
      confirmation : Belenios_web_api.confirmation;
    }
  | Mail_login of { lang : string; recipient : string * string; code : string }

let send msg =
  let* kind, recipient, subject, body =
    match msg with
    | Account_create { lang; recipient; code } ->
        let* l = Web_i18n.get ~component:"admin" ~lang in
        let subject, body =
          Mails_admin.mail_confirmation_link l ~recipient code
        in
        Lwt.return (MailAccountCreation, recipient, subject, body)
    | Account_change_password { lang; recipient; code } ->
        let* l = Web_i18n.get ~component:"admin" ~lang in
        let subject, body = Mails_admin.mail_changepw_link l ~recipient code in
        Lwt.return (MailPasswordChange, recipient, subject, body)
    | Account_set_email { lang; recipient; code } ->
        let* l = Web_i18n.get ~component:"admin" ~lang in
        let subject, body = Mails_admin.mail_set_email l ~recipient code in
        Lwt.return (MailSetEmail, recipient, subject, body)
    | Voter_password x ->
        let* subject, body = Mails_voter.format_password_email x in
        Lwt.return (MailPassword x.uuid, (x.login, x.recipient), subject, body)
    | Voter_credential x ->
        let* subject, body = Mails_voter.format_credential_email x in
        Lwt.return (MailCredential x.uuid, (x.login, x.recipient), subject, body)
    | Vote_confirmation { lang; uuid; title; confirmation; contact } ->
        let* l = Web_i18n.get ~component:"voter" ~lang in
        let subject, body =
          Mails_voter.mail_confirmation l uuid ~title confirmation contact
        in
        Lwt.return
          ( MailConfirmation uuid,
            (confirmation.user, confirmation.recipient),
            subject,
            body )
    | Mail_login { lang; recipient; code } ->
        let* l = Web_i18n.get ~component:"voter" ~lang in
        let subject, body = Mails_voter.email_login l ~recipient ~code in
        Lwt.return (MailLogin, recipient, subject, body)
  in
  let contents =
    Netsendmail.compose
      ~from_addr:(!Web_config.server_name, !Web_config.server_mail)
      ~to_addrs:[ recipient ] ~in_charset:`Enc_utf8 ~out_charset:`Enc_utf8
      ~subject body
  in
  let headers, _ = contents in
  let token = generate_token ~length:6 () in
  let date = Unix.gettimeofday () |> Float.round |> Float.to_string in
  let message_id = Printf.sprintf "<%s%s@%s>" date token !Web_config.domain in
  headers#update_field "Message-ID" message_id;
  headers#update_field "Belenios-Domain" !Web_config.domain;
  let reason, uuid = stringuuid_of_mail_kind kind in
  headers#update_field "Belenios-Reason" reason;
  let () =
    match uuid with
    | None -> ()
    | Some uuid -> headers#update_field "Belenios-UUID" (Uuid.unwrap uuid)
  in
  let return_path = !Web_config.return_path in
  let sendmail = sendmail ?return_path in
  let rec loop retry =
    Lwt.catch
      (fun () -> Lwt_preemptive.detach sendmail contents)
      (function
        | Unix.Unix_error (Unix.EAGAIN, _, _) when retry > 0 ->
            Ocsigen_messages.warning
              "Failed to fork for sending an e-mail; will try again in 1s";
            let* () = sleep 1. in
            loop (retry - 1)
        | e ->
            let msg =
              Printf.sprintf "Failed to send an e-mail to %s: %s"
                (snd recipient) (Printexc.to_string e)
            in
            Ocsigen_messages.errlog msg;
            Lwt.return_unit)
  in
  loop 2
