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
open Belenios_messages

let mailer =
  match Sys.getenv_opt "BELENIOS_SENDMAIL" with
  | None -> "/usr/lib/sendmail"
  | Some x -> x

let split_address =
  let open Re in
  let rex = Pcre.regexp "^(.*)@([^@]+)$" in
  fun x ->
    match exec rex x with
    | exception Not_found -> Printf.ksprintf failwith "bad e-mail address: %s" x
    | g -> (Group.get g 1, Group.get g 2)

let encode_address address =
  address |> String.split_on_char '=' |> String.concat "=="
  |> String.split_on_char '@' |> String.concat "="

let sendmail ~recipient ~uuid message =
  let base_address =
    Option.value ~default:!Web_config.server_mail !Web_config.return_path
  in
  let envelope_from =
    match !Web_config.encode_recipient with
    | false -> base_address
    | true ->
        let recipient = encode_address recipient in
        let uuid =
          match uuid with
          | None -> ""
          | Some x -> Printf.sprintf "+%s" (Uuid.unwrap x)
        in
        let local, domain = split_address base_address in
        Printf.sprintf "%s+%s%s@%s" local recipient uuid domain
  in
  let mailer =
    Printf.sprintf "%s -f %s" mailer (Filename.quote envelope_from)
  in
  Netsendmail.sendmail ~mailer message

let send ?internal (msg : message) =
  let@ () =
   fun cont ->
    match (internal, !Web_config.send_message) with
    | None, None -> cont ()
    | Some true, _ -> cont ()
    | Some false, None -> Lwt.return_error ()
    | (None | Some false), Some (url, key) -> (
        let body =
          msg |> wrap_message ~key |> string_of_message_payload
          |> Cohttp_lwt.Body.of_string
        in
        let* response, x =
          let headers =
            Cohttp.Header.init_with "content-type" "application/json"
          in
          Cohttp_lwt_unix.Client.post ~headers ~body (Uri.of_string url)
        in
        let* hint = Cohttp_lwt.Body.to_string x in
        match Cohttp.Code.code_of_status response.status with
        | 200 -> (
            match Yojson.Safe.from_string hint with
            | `String hint -> Lwt.return_ok hint
            | _ | (exception _) -> Lwt.return_error ())
        | _ -> Lwt.return_error ())
  in
  let* reason, admin_id, uuid, { recipient; subject; body } =
    match msg with
    | `Account_create { lang; recipient; code; uuid; _ } ->
        let lang = Language.get lang in
        let* l = Web_i18n.get ~component:"admin" ~lang in
        let t = Mails_admin.mail_confirmation_link l ~recipient ~code in
        Lwt.return ("account-creation", "", uuid, t)
    | `Account_change_password { lang; recipient; code; uuid; _ } ->
        let lang = Language.get lang in
        let* l = Web_i18n.get ~component:"admin" ~lang in
        let t = Mails_admin.mail_changepw_link l ~recipient ~code in
        Lwt.return ("password-change", "", uuid, t)
    | `Account_set_email { lang; recipient; code; uuid; _ } ->
        let lang = Language.get lang in
        let* l = Web_i18n.get ~component:"admin" ~lang in
        let t = Mails_admin.mail_set_email l ~recipient ~code in
        Lwt.return ("set-email", "", uuid, t)
    | `Voter_password x ->
        let* t = Mails_voter.format_password_email x in
        let m = Option.value ~default:dummy_metadata x.metadata in
        Lwt.return ("password", string_of_int m.admin_id, Some m.uuid, t)
    | `Voter_credential x ->
        let* t = Mails_voter.format_credential_email x in
        let m = Option.value ~default:dummy_metadata x.metadata in
        Lwt.return ("credential", string_of_int m.admin_id, Some m.uuid, t)
    | `Vote_confirmation { lang; uuid; title; confirmation; contact } ->
        let lang = Language.get lang in
        let* l = Web_i18n.get ~component:"voter" ~lang in
        let t =
          Mails_voter.mail_confirmation l uuid ~title confirmation contact
        in
        Lwt.return ("confirmation", "", Some uuid, t)
    | `Mail_login { lang; recipient; state; code; uuid } ->
        let lang = Language.get lang in
        let* l = Web_i18n.get ~component:"voter" ~lang in
        let t = Mails_voter.email_login l ~recipient ?state ~code () in
        Lwt.return ("login", "", uuid, t)
    | `Credentials_seed m ->
        let lang = Language.get m.lang in
        let* l = Web_i18n.get ~component:"admin" ~lang in
        let t = Mails_admin.mail_credentials_seed l m in
        Lwt.return ("credentials-seed", string_of_int m.admin_id, Some m.uuid, t)
  in
  let@ _check_recipient cont =
    if String.contains recipient.address '@' then cont ()
    else Lwt.return_error ()
  in
  let contents =
    Netsendmail.compose
      ~from_addr:(!Web_config.server_name, !Web_config.server_mail)
      ~to_addrs:[ (recipient.name, recipient.address) ]
      ~in_charset:`Enc_utf8 ~out_charset:`Enc_utf8 ~subject body
  in
  let headers, _ = contents in
  let token = generate_token ~length:6 () in
  let date = Unix.gettimeofday () |> Float.round |> Float.to_string in
  let message_id = Printf.sprintf "<%s%s@%s>" date token !Web_config.domain in
  headers#update_field "Message-ID" message_id;
  headers#update_field "Belenios-Domain" !Web_config.domain;
  headers#update_field "Belenios-Reason" reason;
  let () =
    match uuid with
    | None -> ()
    | Some uuid -> headers#update_field "Belenios-UUID" (Uuid.unwrap uuid)
  in
  let () =
    match !Web_config.fbl_senderid with
    | None -> ()
    | Some senderid ->
        let uuid =
          match uuid with None -> "" | Some uuid -> Uuid.unwrap uuid
        in
        headers#update_field "Feedback-ID"
          (Printf.sprintf "%s:%s:%s:%s" uuid admin_id reason senderid)
  in
  let sendmail = sendmail ~uuid ~recipient:recipient.address in
  let rec loop retry =
    Lwt.catch
      (fun () ->
        let* () = Lwt_preemptive.detach sendmail contents in
        Lwt.return_ok recipient.address)
      (function
        | Unix.Unix_error (Unix.EAGAIN, _, _) when retry > 0 ->
            Ocsigen_messages.warning
              "Failed to fork for sending an e-mail; will try again in 1s";
            let* () = sleep 1. in
            loop (retry - 1)
        | e ->
            let msg =
              Printf.sprintf "Failed to send an e-mail to %s: %s"
                recipient.address (Printexc.to_string e)
            in
            Ocsigen_messages.errlog msg;
            Lwt.return_error ())
  in
  loop 2
