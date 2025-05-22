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

let hmac ~key x =
  let open Cryptokit in
  hash_string (MAC.hmac_sha256 key) x
  |> transform_string (Hexa.encode ())
  |> Belenios.Hash.of_hex

let wrap_message ~key (message : Belenios_web_api.message) =
  let timestamp = Unix.gettimeofday () in
  { timestamp; message; hmac = None }
  |> Belenios_web_api.string_of_message_payload |> hmac ~key
  |> fun x ->
  ({ timestamp; message; hmac = Some x } : Belenios_web_api.message_payload)

let check_message ~key (message : Belenios_web_api.message_payload) =
  { message with hmac = None }
  |> Belenios_web_api.string_of_message_payload |> hmac ~key
  |> fun x -> message.hmac = Some x

let send s ?internal (msg : Belenios_web_api.message) =
  let@ () =
   fun cont ->
    match (internal, !Web_config.send_message) with
    | None, None -> cont ()
    | Some true, _ -> cont ()
    | Some false, None -> Lwt.return_error ()
    | (None | Some false), Some (url, key) -> (
        let body =
          msg |> wrap_message ~key |> Belenios_web_api.string_of_message_payload
          |> Cohttp_lwt.Body.of_string
        in
        let* response, x =
          Cohttp_lwt_unix.Client.post ~body (Uri.of_string url)
        in
        let* hint = Cohttp_lwt.Body.to_string x in
        match Cohttp.Code.code_of_status response.status with
        | 200 -> Lwt.return_ok hint
        | _ -> Lwt.return_error ())
  in
  let* reason, uuid, { recipient; subject; body } =
    match msg with
    | `Account_create { lang; recipient; code } ->
        let* l = Web_i18n.get ~component:"admin" ~lang in
        let t = Mails_admin.mail_confirmation_link l ~recipient ~code in
        Lwt.return ("account-creation", None, t)
    | `Account_change_password { lang; recipient; code } ->
        let* l = Web_i18n.get ~component:"admin" ~lang in
        let t = Mails_admin.mail_changepw_link l ~recipient ~code in
        Lwt.return ("password-change", None, t)
    | `Account_set_email { lang; recipient; code } ->
        let* l = Web_i18n.get ~component:"admin" ~lang in
        let t = Mails_admin.mail_set_email l ~recipient ~code in
        Lwt.return ("set-email", None, t)
    | `Voter_password x ->
        let* t = Mails_voter.format_password_email s x in
        Lwt.return ("password", Some x.uuid, t)
    | `Voter_credential x ->
        let* t = Mails_voter.format_credential_email s x in
        Lwt.return ("credential", Some x.uuid, t)
    | `Vote_confirmation { lang; uuid; title; confirmation; contact } ->
        let* l = Web_i18n.get ~component:"voter" ~lang in
        let t =
          Mails_voter.mail_confirmation l uuid ~title confirmation contact
        in
        Lwt.return ("confirmation", Some uuid, t)
    | `Mail_login { lang; recipient; code } ->
        let* l = Web_i18n.get ~component:"voter" ~lang in
        let t = Mails_voter.email_login l ~recipient ~code in
        Lwt.return ("login", None, t)
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
  let return_path = !Web_config.return_path in
  let sendmail = sendmail ?return_path in
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
