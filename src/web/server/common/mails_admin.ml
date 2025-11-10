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

open Belenios
open Belenios_messages

let mail_confirmation_link l ~(recipient : recipient) ~code =
  let open (val l : Belenios_ui.I18n.GETTEXT) in
  let open Belenios_ui.Mail_formatter in
  let b = create () in
  add_sentence b (Printf.sprintf (f_ "Dear %s,") recipient.name);
  add_newline b;
  add_newline b;
  add_sentence b
    (s_
       "Your e-mail address has been used to create an account on our Belenios \
        server.");
  add_sentence b (s_ "To confirm this creation, please use the following code:");
  add_newline b;
  add_newline b;
  add_string b "  ";
  add_string b code;
  add_newline b;
  add_newline b;
  add_sentence b
    (s_
       "Warning: this code is valid for 15 minutes, and previous codes sent to \
        this address are no longer valid.");
  add_newline b;
  add_newline b;
  add_sentence b (s_ "Best regards,");
  add_newline b;
  add_newline b;
  add_string b "-- ";
  add_newline b;
  add_string b !Web_config.server_name;
  let body = contents b in
  let subject = !Web_config.vendor ^^^ s_ "Create account" in
  ({ recipient; subject; body } : text_message)

let mail_changepw_link l ~(recipient : recipient) ~code =
  let open (val l : Belenios_ui.I18n.GETTEXT) in
  let open Belenios_ui.Mail_formatter in
  let b = create () in
  add_sentence b (Printf.sprintf (f_ "Dear %s,") recipient.name);
  add_newline b;
  add_newline b;
  add_sentence b
    (s_
       "There has been a request to change the password of your account on our \
        Belenios server.");
  add_sentence b (s_ "To confirm this, please use the following code:");
  add_newline b;
  add_newline b;
  add_string b "  ";
  add_string b code;
  add_newline b;
  add_newline b;
  add_sentence b
    (s_
       "Warning: this code is valid for 15 minutes, and previous codes sent to \
        this address are no longer valid.");
  add_newline b;
  add_newline b;
  add_sentence b (s_ "Best regards,");
  add_newline b;
  add_newline b;
  add_string b "-- ";
  add_newline b;
  add_string b !Web_config.server_name;
  let body = contents b in
  let subject = !Web_config.vendor ^^^ s_ "Change password" in
  ({ recipient; subject; body } : text_message)

let mail_set_email l ~(recipient : recipient) ~code =
  let open (val l : Belenios_ui.I18n.GETTEXT) in
  let open Belenios_ui.Mail_formatter in
  let b = create () in
  add_sentence b (Printf.sprintf (f_ "Dear %s,") recipient.name);
  add_newline b;
  add_newline b;
  add_sentence b
    (s_
       "Someone is trying to associate your e-mail address to an account on \
        our Belenios server.");
  add_sentence b (s_ "To confirm this, please use the following code:");
  add_newline b;
  add_newline b;
  add_string b "  ";
  add_string b code;
  add_newline b;
  add_newline b;
  add_sentence b
    (s_
       "Warning: this code is valid for 15 minutes, and previous codes sent to \
        this address are no longer valid.");
  add_newline b;
  add_newline b;
  add_sentence b (s_ "Best regards,");
  add_newline b;
  add_newline b;
  add_string b "-- ";
  add_newline b;
  add_string b !Web_config.server_name;
  let body = contents b in
  let subject = !Web_config.vendor ^^^ s_ "Change e-mail address" in
  ({ recipient; subject; body } : text_message)

let add_field b k v =
  let open Belenios_ui.Mail_formatter in
  add_string b k;
  add_string b " ";
  add_string b v;
  add_newline b

let extract_prefix =
  let rex = Re.Pcre.regexp {|^(.*)/api/credentials/server$|} in
  fun x ->
    match Re.exec rex x with
    | exception Not_found -> None
    | g -> Some (Re.Group.get g 1)

let mail_credentials_seed l (m : credentials_seed_message) =
  let address = m.cred_authority_info.cred_operator in
  let recipient : recipient = { name = address; address } in
  let open (val l : Belenios_ui.I18n.GETTEXT) in
  let open Belenios_ui.Mail_formatter in
  let b = create () in
  add_sentence b (Printf.sprintf (f_ "Dear %s,") address);
  add_newline b;
  add_newline b;
  add_sentence b
  @@ s_
       "You have been designated as the operator in the credential authority \
        protocol for the following Belenios election:";
  add_newline b;
  add_newline b;
  add_field b (s_ "Election server:") m.belenios_url;
  let uuid = Uuid.unwrap m.uuid in
  add_field b (s_ "Election identifier:") uuid;
  add_field b (s_ "Secret key:") m.seed;
  let () =
    match extract_prefix m.cred_authority_info.cred_server with
    | None ->
        add_field b (s_ "Credential server:") m.cred_authority_info.cred_server
    | Some prefix ->
        let url = Printf.sprintf "%s/credop#%s/%s" prefix uuid m.seed in
        add_field b (s_ "Operator interface:") url
  in
  add_newline b;
  add_sentence b (s_ "Best regards,");
  add_newline b;
  add_newline b;
  add_string b "-- ";
  add_newline b;
  add_string b !Web_config.server_name;
  let body = contents b in
  let subject = !Web_config.vendor ^^^ s_ "Credential authority protocol" in
  ({ recipient; subject; body } : text_message)
