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
open Belenios_server_core
open Web_common

type metadata = {
  title : string;
  contact : string option;
  has_passwords : bool;
  langs : string list;
}

let extract_metadata template metadata =
  let has_passwords =
    match metadata.e_auth_config with
    | Some [ { auth_system = "password"; _ } ] -> true
    | _ -> false
  in
  {
    title = template.t_name;
    contact = metadata.e_contact;
    has_passwords;
    langs = get_languages metadata.e_languages;
  }

let get_metadata s uuid =
  let* raw = Public_archive.get_election s uuid in
  match raw with
  | Some raw -> (
      let* metadata = Storage.get s (Election (uuid, Metadata)) in
      match Lopt.get_value metadata with
      | None ->
          Printf.ksprintf failwith "Mails_voter.get_metadata(%s)/running"
            (Uuid.unwrap uuid)
      | Some metadata ->
          let election = Election.of_string (module Random) raw in
          let module W = (val election) in
          Lwt.return @@ extract_metadata W.template metadata)
  | None -> (
      let* se = Storage.get s (Election (uuid, Draft)) in
      match Lopt.get_value se with
      | None ->
          Printf.ksprintf failwith "Mails_voter.get_metadata(%s)/draft"
            (Uuid.unwrap uuid)
      | Some (Draft (_, se)) ->
          Lwt.return @@ extract_metadata se.se_questions se.se_metadata)

let contact_footer l contact =
  let open (val l : Belenios_ui.I18n.GETTEXT) in
  match contact with
  | None -> fun _ -> ()
  | Some x ->
      fun b ->
        let open Belenios_ui.Mail_formatter in
        add_newline b;
        add_newline b;
        add_sentence b (s_ "To get more information, please contact:");
        add_newline b;
        add_newline b;
        add_string b "  ";
        add_string b x

let mail_password l metadata login password weight url =
  let open (val l : Belenios_ui.I18n.GETTEXT) in
  let open Belenios_ui.Mail_formatter in
  let b = create () in
  add_sentence b
    (s_ "Please find below your login and password for the election");
  add_newline b;
  add_newline b;
  add_string b "  ";
  add_string b metadata.title;
  add_newline b;
  add_newline b;
  add_sentence b
    (s_
       "Note that you also need a credential, sent in a separate email, to \
        start voting.");
  add_newline b;
  add_newline b;
  add_string b (s_ "Username:");
  add_string b " ";
  add_string b login;
  add_newline b;
  add_string b (s_ "Password:");
  add_string b " ";
  add_string b password;
  add_newline b;
  add_newline b;
  (match weight with
  | Some weight ->
      add_string b (s_ "Number of votes:");
      add_string b " ";
      add_string b (Weight.to_string weight);
      add_newline b
  | None -> ());
  add_string b (s_ "Page of the election:");
  add_string b " ";
  add_string b url;
  add_newline b;
  add_newline b;
  add_sentence b (s_ "You are allowed to vote several times.");
  add_sentence b (s_ "Only the last vote counts.");
  contact_footer l metadata.contact b;
  contents b

let format_password_email s (x : material_message) =
  let* metadata = get_metadata s x.uuid in
  let url = get_election_home_url x.uuid in
  let* bodies =
    Lwt_list.map_s
      (fun lang ->
        let* l = Web_i18n.get ~component:"voter" ~lang in
        return
          (mail_password l metadata x.recipient.name x.material x.weight url))
      metadata.langs
  in
  let body = String.concat "\n\n----------\n\n" bodies in
  let body = body ^ "\n\n-- \n" ^ !Web_config.vendor in
  let* subject =
    let* l = Web_i18n.get ~component:"voter" ~lang:(List.hd metadata.langs) in
    let open (val l) in
    Printf.ksprintf return (f_ "Your password for election %s") metadata.title
  in
  Lwt.return
    ({ recipient = x.recipient; subject; body }
      : Belenios_messages.text_message)

let generate_password_email uuid ~admin_id v show_weight =
  let (_, { address; login; weight }) : Voter.t = v in
  let weight = if show_weight then weight else None in
  let salt = generate_token () in
  let* password =
    let x = generate_token ~length:15 () in
    return (format_password x)
  in
  let hashed = sha256_hex (salt ^ password) in
  let x : material_message =
    {
      admin_id;
      uuid;
      recipient = { name = Option.value login ~default:address; address };
      material = password;
      weight;
    }
  in
  return (`Password x, (salt, hashed))

let mail_credential l metadata (x : material_message) =
  let open (val l : Belenios_ui.I18n.GETTEXT) in
  let open Belenios_ui.Mail_formatter in
  let b = create () in
  add_sentence b
    (s_
       "Warning: this e-mail contains personal and confidential material and \
        should not be transmitted.");
  add_newline b;
  add_newline b;
  add_sentence b (s_ "You are listed as a voter for the election");
  add_newline b;
  add_newline b;
  add_string b "  ";
  add_string b metadata.title;
  add_newline b;
  add_newline b;
  add_sentence b (s_ "Please vote here using your personal link:");
  add_newline b;
  add_newline b;
  add_string b "  ";
  add_string b (get_election_home_url ~credential:x.material x.uuid);
  add_newline b;
  add_newline b;
  if metadata.has_passwords then (
    add_sentence b
      (s_
         "To cast a vote, you will also need a password, sent in a separate \
          email.");
    add_newline b;
    add_newline b);
  add_string b (s_ "If the above link does not work:");
  add_newline b;
  add_newline b;
  add_string b (s_ "Public page of the election:");
  add_string b " ";
  add_string b (get_election_home_url x.uuid);
  add_newline b;
  add_string b (s_ "Username:");
  add_string b " ";
  add_string b x.recipient.name;
  add_newline b;
  add_string b (s_ "Your credential:");
  add_string b " ";
  add_string b x.material;
  add_newline b;
  (match x.weight with
  | Some weight ->
      add_string b (s_ "Number of votes:");
      add_string b " ";
      add_string b (Weight.to_string weight);
      add_newline b
  | None -> ());
  add_newline b;
  add_sentence b (s_ "You are allowed to vote several times.");
  add_sentence b (s_ "Only the last vote counts.");
  contact_footer l metadata.contact b;
  contents b

let format_credential_email s (x : material_message) =
  let* metadata = get_metadata s x.uuid in
  let* bodies =
    Lwt_list.map_s
      (fun lang ->
        let* l = Web_i18n.get ~component:"voter" ~lang in
        return (mail_credential l metadata x))
      metadata.langs
  in
  let body = String.concat "\n\n----------\n\n" bodies in
  let body = body ^ "\n\n-- \n" ^ !Web_config.vendor in
  let* subject =
    let* l = Web_i18n.get ~component:"voter" ~lang:(List.hd metadata.langs) in
    let open (val l) in
    Printf.ksprintf return (f_ "Your credential for election %s") metadata.title
  in
  return
    ({ recipient = x.recipient; subject; body }
      : Belenios_messages.text_message)

let generate_credential_email uuid ~admin_id (Draft (_, se)) =
  let show_weight = has_explicit_weights se.se_voters in
  fun ~recipient ~login ~weight ~credential ->
    let oweight = if show_weight then Some weight else None in
    let x : material_message =
      {
        admin_id;
        uuid;
        recipient = { name = login; address = recipient };
        material = credential;
        weight = oweight;
      }
    in
    Lwt.return @@ `Credential x

let mail_confirmation l uuid ~title x contact =
  let url2 = Web_common.get_election_home_url uuid in
  let url1 = url2 ^ "/ballots" in
  let ({ recipient; weight; hash; revote; _ } : Belenios_web_api.confirmation) =
    x
  in
  let open (val l : Belenios_ui.I18n.GETTEXT) in
  let subject = Printf.sprintf (f_ "Your vote for election %s") title in
  let open Belenios_ui.Mail_formatter in
  let b = create () in
  add_sentence b (Printf.sprintf (f_ "Dear %s,") recipient.name);
  add_newline b;
  add_newline b;
  add_sentence b (s_ "Your vote for election");
  add_newline b;
  add_newline b;
  add_string b "  ";
  add_string b title;
  add_newline b;
  add_newline b;
  add_sentence b (s_ "has been recorded.");
  (match weight with
  | Some weight ->
      add_sentence b
        (Printf.sprintf (f_ "Your weight is %s.") (Weight.to_string weight))
  | None -> ());
  add_sentence b (s_ "Your smart ballot tracker is");
  add_newline b;
  add_newline b;
  add_string b "  ";
  add_string b (Hash.to_b64 hash);
  add_newline b;
  if revote then (
    add_newline b;
    add_sentence b (s_ "This vote replaces any previous vote.");
    add_newline b);
  add_newline b;
  add_sentence b
    (s_ "You can check its presence in the ballot box, accessible at");
  add_newline b;
  add_string b "  ";
  add_string b url1;
  add_newline b;
  add_newline b;
  add_sentence b (s_ "Results will be published on the election page");
  add_newline b;
  add_string b "  ";
  add_string b url2;
  contact_footer l contact b;
  add_newline b;
  add_newline b;
  add_string b "-- ";
  add_newline b;
  add_string b !Web_config.vendor;
  ({ recipient = x.recipient; subject; body = contents b }
    : Belenios_messages.text_message)

let email_login l ~(recipient : Belenios_web_api.recipient) ~code =
  let open (val l : Belenios_ui.I18n.GETTEXT) in
  let open Belenios_ui.Mail_formatter in
  let b = create () in
  add_sentence b (Printf.sprintf (f_ "Dear %s,") recipient.name);
  add_newline b;
  add_newline b;
  add_sentence b
    (s_
       "Your e-mail address has been used to authenticate with our Belenios \
        server.");
  add_sentence b (s_ "Use the following code:");
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
  let subject = !Web_config.vendor ^^^ s_ "Authentication" in
  ({ recipient; subject; body } : Belenios_messages.text_message)
