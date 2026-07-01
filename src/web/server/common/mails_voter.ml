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
open Belenios_messages
open Belenios
open Belenios_storage_api
open Web_common

let extract_metadata ~admin_id ~has_weights uuid (template : _ template)
    metadata : Belenios_messages.metadata =
  let has_passwords =
    match metadata.auth_config with
    | Some [ { auth_system = "password"; _ } ] -> true
    | _ -> false
  in
  {
    uuid;
    admin_id;
    title = template.name;
    contact = metadata.contact;
    has_weights;
    has_passwords;
    langs = get_languages metadata.languages;
  }

let get_metadata s ~admin_id =
  let uuid = Storage.E.get_uuid s in
  let* election = Public_archive.get_election s in
  match election with
  | Some election -> (
      let* has_weights =
        let* x = Storage.E.get s Voters_config in
        match Lopt.get_value x with
        | None -> Lwt.return_true
        | Some x -> Lwt.return x.has_explicit_weights
      in
      let* metadata = Storage.E.get s Metadata in
      match Lopt.get_value metadata with
      | None ->
          Printf.ksprintf failwith "Mails_voter.get_metadata(%s)/running"
            (Uuid.to_string uuid)
      | Some metadata ->
          let module W = (val election) in
          Lwt.return
          @@ extract_metadata ~admin_id ~has_weights W.uuid W.template metadata)
  | None -> (
      let* se = Storage.E.get s Draft in
      match Lopt.get_value se with
      | None ->
          Printf.ksprintf failwith "Mails_voter.get_metadata(%s)/draft"
            (Uuid.to_string uuid)
      | Some (W (_, Draft (_, se))) ->
          let has_weights = has_explicit_weights se.voters in
          Lwt.return
          @@ extract_metadata ~admin_id ~has_weights uuid se.questions
               se.metadata)

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

let mail_credential l (x : material_message) =
  let metadata =
    Option.value ~default:Belenios_messages.dummy_metadata x.metadata
  in
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
  add_string b (get_election_home_url ~credential:x.material metadata.uuid);
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
  add_string b (get_election_home_url metadata.uuid);
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

let format_credential_email (x : material_message) =
  let metadata =
    Option.value ~default:Belenios_messages.dummy_metadata x.metadata
  in
  let langs = metadata.langs |> List.map Language.get |> uniq_first in
  let* bodies =
    Lwt_list.map_s
      (fun lang ->
        let* l = Web_i18n.get ~component:"voter" ~lang in
        return (mail_credential l x))
      langs
  in
  let body = String.concat "\n\n----------\n\n" bodies in
  let body = body ^ "\n\n-- \n" ^ !Web_config.vendor in
  let* subject =
    let* l = Web_i18n.get ~component:"voter" ~lang:(List.hd langs) in
    let open (val l) in
    Printf.ksprintf return (f_ "Your credential for election %s") metadata.title
  in
  return
    ({ recipient = x.recipient; subject; body }
      : Belenios_messages.text_message)

let generate_credential_email (metadata : Belenios_messages.metadata) =
 fun ?recipient ?weight ~login credential ->
  let recipient = Option.value ~default:login recipient in
  let x : material_message =
    {
      recipient = { name = login; address = recipient };
      material = credential;
      weight;
      metadata = Some metadata;
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

(* Forward reference set in Web_main *)
let make_login_link = ref (fun _ -> failwith "make_login_link")

let email_login l ~(recipient : recipient) ?state ~code () =
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
  let () =
    match state with
    | None -> ()
    | Some state ->
        let link = !make_login_link (state, code) in
        add_sentence b (s_ "Alternatively, you can follow the link:");
        add_newline b;
        add_newline b;
        add_string b "  ";
        add_string b link;
        add_newline b;
        add_newline b
  in
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
