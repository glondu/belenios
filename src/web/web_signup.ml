(**************************************************************************)
(*                                BELENIOS                                *)
(*                                                                        *)
(*  Copyright © 2012-2020 Inria                                           *)
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
open Belenios_platform
open Belenios
open Platform
open Common
open Web_serializable_builtin_t
open Web_common

type captcha = {
    content_type : string;
    contents : string;
    response : string;
    c_expiration_time : datetime;
  }

let captchas = ref SMap.empty

let filter_captchas_by_time table =
  let now = now () in
  SMap.filter (fun _ {c_expiration_time; _} ->
      datetime_compare now c_expiration_time <= 0
    ) table

let format_content_type = function
  | "png" -> "image/png"
  | x -> Printf.ksprintf failwith "Unknown captcha type: %s" x

let captcha =
  let x = "belenios-captcha" in (x, [| x |])

let create_captcha () =
  let* raw = Lwt_process.pread_lines captcha |> Lwt_stream.to_list in
  match raw with
  | content_type :: response :: contents ->
     let content_type = format_content_type content_type in
     let contents =
       let open Cryptokit in
       String.concat "\n" contents |> transform_string (Base64.decode ())
     in
     let challenge = sha256_b64 contents in
     let c_expiration_time = datetime_add (now ()) (second 300.) in
     let x = { content_type; contents; response; c_expiration_time } in
     captchas := SMap.add challenge x !captchas;
     Lwt.return challenge
  | _ ->
     Lwt.fail (Failure "Captcha generation failed")

let get challenge =
  captchas := filter_captchas_by_time !captchas;
  SMap.find_opt challenge !captchas

let get_captcha ~challenge =
  match get challenge with
  | None -> fail_http 404
  | Some {content_type; contents; _} -> Lwt.return (contents, content_type)

let check_captcha ~challenge ~response =
  match get challenge with
  | None -> Lwt.return false
  | Some x ->
     captchas := SMap.remove challenge !captchas;
     Lwt.return (response = x.response)

type link_kind =
  | CreateAccount
  | ChangePassword of string

type link = {
    service : string;
    code : string;
    l_expiration_time : datetime;
    kind : link_kind;
}

let links = ref SMap.empty

let filter_links_by_time table =
  let now = now () in
  SMap.filter (fun _ {l_expiration_time; _} ->
      datetime_compare now l_expiration_time <= 0
    ) table

let mail_confirmation_link l address code =
  let open (val l : Web_i18n_sig.GETTEXT) in
  let open Mail_formatter in
  let b = create () in
  add_sentence b (Printf.sprintf (f_ "Dear %s,") address);
  add_newline b; add_newline b;
  add_sentence b (s_ "Your e-mail address has been used to create an account on our Belenios server.");
  add_sentence b (s_ "To confirm this creation, please use the following code:");
  add_newline b; add_newline b;
  add_string b "  "; add_string b code;
  add_newline b; add_newline b;
  add_sentence b (s_ "Warning: this code is valid for 15 minutes, and previous codes sent to this address are no longer valid.");
  add_newline b; add_newline b;
  add_sentence b (s_ "Best regards,");
  add_newline b; add_newline b;
  add_string b "-- ";
  add_newline b;
  add_string b (s_ "Belenios Server");
  let body = contents b in
  let subject = s_ "Belenios account creation" in
  subject, body

let send_confirmation_link l ~service address =
  let* code = generate_numeric () in
  let l_expiration_time = datetime_add (now ()) (second 900.) in
  let kind = CreateAccount in
  let link = {service; code; l_expiration_time; kind} in
  let nlinks = filter_links_by_time !links in
  links := SMap.add address link nlinks;
  let subject, body = mail_confirmation_link l address code in
  let* () = send_email MailAccountCreation ~recipient:address ~subject ~body in
  Lwt.return_unit

let mail_changepw_link l address code =
  let open (val l : Web_i18n_sig.GETTEXT) in
  let open Mail_formatter in
  let b = create () in
  add_sentence b (Printf.sprintf (f_ "Dear %s,") address);
  add_newline b; add_newline b;
  add_sentence b (s_ "There has been a request to change the password of your account on our Belenios server.");
  add_sentence b (s_ "To confirm this, please use the following code:");
  add_newline b; add_newline b;
  add_string b "  "; add_string b code;
  add_newline b; add_newline b;
  add_sentence b (s_ "Warning: this code is valid for 15 minutes, and previous codes sent to this address are no longer valid.");
  add_newline b; add_newline b;
  add_sentence b (s_ "Best regards,");
  add_newline b; add_newline b;
  add_string b "-- ";
  add_newline b;
  add_string b (s_ "Belenios Server");
  let body = contents b in
  let subject = s_ "Belenios password change" in
  subject, body

let send_changepw_link l ~service ~address ~username =
  let* code = generate_numeric () in
  let l_expiration_time = datetime_add (now ()) (second 900.) in
  let kind = ChangePassword username in
  let link = {service; code; l_expiration_time; kind} in
  let nlinks = filter_links_by_time !links in
  links := SMap.add address link nlinks;
  let subject, body = mail_changepw_link l address code in
  let* () = send_email MailPasswordChange ~recipient:address ~subject ~body in
  Lwt.return_unit

let confirm_link address =
  links := filter_links_by_time !links;
  match SMap.find_opt address !links with
  | None -> Lwt.return_none
  | Some x -> Lwt.return_some (x.code, x.service, x.kind)

let remove_link address =
  links := filter_links_by_time !links;
  links := SMap.remove address !links;
  Lwt.return_unit

let cracklib =
  let x = "cracklib-check" in (x, [| x |])

let extract_comment x =
  let n = String.length x in
  match String.rindex_opt x ':' with
  | Some i when i < n - 2 ->
     let x = String.sub x (i + 2) (n - i - 3) in
     if x = "OK" then None else Some x
  | _ -> Some "unknown error"

let cracklib_check password =
  match String.index_opt password '\n' with
  | None ->
     let* x = Lwt_process.pmap ~env:[| "LANG=C" |] cracklib password in
     Lwt.return (extract_comment x)
  | Some _ -> Lwt.return_some "newline in password"
