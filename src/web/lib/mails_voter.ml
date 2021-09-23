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
open Belenios_core
open Serializable_builtin_t
open Serializable_j
open Common
open Web_serializable_j
open Web_common

let contact_footer l metadata =
  let open (val l : Belenios_ui.I18n.GETTEXT) in
  match metadata.e_contact with
  | None -> fun _ -> ()
  | Some x ->
     fun b ->
     let open Mail_formatter in
     add_newline b;
     add_newline b;
     add_sentence b (s_ "To get more information, please contact:");
     add_newline b;
     add_string b "  ";
     add_string b x

let mail_password l title login password weight url metadata =
  let open (val l : Belenios_ui.I18n.GETTEXT) in
  let open Mail_formatter in
  let b = create () in
  add_sentence b (s_ "Please find below your login and password for the election"); add_newline b;
  add_newline b;
  add_string b "  "; add_string b title; add_newline b;
  add_newline b;
  add_sentence b (s_ "Note that you also need a credential, sent in a separate email, to start voting.");
  add_newline b;
  add_newline b;
  add_string b (s_ "Username:"); add_string b " "; add_string b login; add_newline b;
  add_string b (s_ "Password:"); add_string b " "; add_string b password; add_newline b;
  add_newline b;
  (match weight with
   | Some weight ->
      add_string b (s_ "Number of votes:"); add_string b " "; add_string b (Weight.to_string weight); add_newline b
   | None -> ()
  );
  add_string b (s_ "Page of the election:"); add_string b " "; add_string b url; add_newline b;
  add_newline b;
  add_sentence b (s_ "You are allowed to vote several times.");
  add_sentence b (s_ "Only the last vote counts.");
  contact_footer l metadata b;
  contents b

open Belenios_platform.Platform

let generate_password metadata langs title uuid url id show_weight =
  let recipient, login, weight = split_identity id in
  let weight = if show_weight then Some weight else None in
  let* salt = generate_token () in
  let* password =
    let* x = generate_token ~length:15 () in
    return (format_password x)
  in
  let hashed = sha256_hex (salt ^ password) in
  let* bodies = Lwt_list.map_s (fun lang ->
                    let* l = Web_i18n.get ~component:"voter" ~lang in
                    return (mail_password l title login password weight url metadata)
                  ) langs in
  let body = String.concat "\n\n----------\n\n" bodies in
  let body = body ^ "\n\n-- \nBelenios" in
  let* subject =
    let* l = Web_i18n.get ~component:"voter" ~lang:(List.hd langs) in
    let open (val l) in
    Printf.kprintf return (f_ "Your password for election %s") title
  in
  let* () = send_email (MailPassword uuid) ~recipient ~subject ~body in
  return (salt, hashed)

let mail_credential l has_passwords title ~login cred weight url metadata =
  let open (val l : Belenios_ui.I18n.GETTEXT) in
  let open Mail_formatter in
  let b = create () in
  add_sentence b (s_ "You are listed as a voter for the election"); add_newline b;
  add_newline b;
  add_string b "  "; add_string b title; add_newline b;
  add_newline b;
  add_sentence b (s_ "You will find below your credential.");
  add_sentence b (s_ "You will be asked to enter your credential before entering the voting booth.");
  if has_passwords then (
    add_sentence b (s_ "To cast a vote, you will also need a password, sent in a separate email.");
  );
  add_newline b;
  add_newline b;
  add_string b (s_ "Credential:"); add_string b " "; add_string b cred; add_newline b;
  add_newline b;
  add_string b (s_ "Username:"); add_string b " "; add_string b login; add_newline b;
  (match weight with
   | Some weight ->
      add_string b (s_ "Number of votes:"); add_string b " "; add_string b (Weight.to_string weight); add_newline b
   | None -> ()
  );
  add_string b (s_ "Page of the election:"); add_string b " "; add_string b url; add_newline b;
  add_newline b;
  add_sentence b (s_ "You are allowed to vote several times.");
  add_sentence b (s_ "Only the last vote counts.");
  contact_footer l metadata b;
  contents b

let generate_mail_credential langs has_passwords title ~login cred weight url metadata =
  let* bodies =
    Lwt_list.map_s
      (fun lang ->
        let* l = Web_i18n.get ~component:"voter" ~lang in
        return (mail_credential l has_passwords title ~login cred weight url metadata)
      ) langs
  in
  let body = String.concat "\n\n----------\n\n" bodies in
  let body = body ^ "\n\n-- \nBelenios" in
  let* subject =
    let* l = Web_i18n.get ~component:"voter" ~lang:(List.hd langs) in
    let open (val l) in
    Printf.ksprintf return (f_ "Your credential for election %s") title
  in
  return (subject, body)

let send_mail_credential uuid se =
  let title = se.se_questions.t_name in
  let url = get_election_home_url uuid in
  let show_weight =
    List.exists
      (fun v ->
        let _, _, weight = split_identity_opt v.sv_id in
        weight <> None
      ) se.se_voters
  in
  let has_passwords =
    match se.se_metadata.e_auth_config with
    | Some [{auth_system = "password"; _}] -> true
    | _ -> false
  in
  let langs = get_languages se.se_metadata.e_languages in
  fun ~recipient ~login ~weight ~cred ->
  let oweight = if show_weight then Some weight else None in
  let* subject, body =
    generate_mail_credential langs has_passwords
      title ~login cred oweight url se.se_metadata
  in
  send_email (MailCredential uuid) ~recipient ~subject ~body

let mail_confirmation l user title weight hash revote url1 url2 metadata =
  let open (val l : Belenios_ui.I18n.GETTEXT) in
  let open Mail_formatter in
  let b = create () in
  add_sentence b (Printf.sprintf (f_ "Dear %s,") user); add_newline b;
  add_newline b;
  add_sentence b (s_ "Your vote for election"); add_newline b;
  add_newline b;
  add_string b "  "; add_string b title; add_newline b;
  add_newline b;
  add_sentence b (s_ "has been recorded.");
  (match weight with
   | Some weight ->
      add_sentence b (Printf.sprintf (f_ "Your weight is %s.") (Weight.to_string weight))
   | None -> ()
  );
  add_sentence b (s_ "Your smart ballot tracker is"); add_newline b;
  add_newline b;
  add_string b "  "; add_string b hash; add_newline b;
  if revote then (
    add_newline b;
    add_sentence b (s_ "This vote replaces any previous vote.");
    add_newline b;
  );
  add_newline b;
  add_sentence b (s_ "You can check its presence in the ballot box, accessible at");
  add_newline b;
  add_string b "  "; add_string b url1; add_newline b;
  add_newline b;
  add_sentence b (s_ "Results will be published on the election page");
  add_newline b;
  add_string b "  "; add_string b url2;
  contact_footer l metadata b;
  add_newline b;
  add_newline b;
  add_string b "-- "; add_newline b;
  add_string b "Belenios";
  contents b
