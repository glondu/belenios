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
open Common

let mail_trustee_generation_basic_body l link =
  let open (val l : Belenios_ui.I18n.GETTEXT) in
  let open Mail_formatter in
  let b = create () in
  add_sentence b (s_ "Dear trustee,");
  add_newline b; add_newline b;
  add_sentence b (s_ "You will find below the link to generate your private decryption key, used to tally the election.");
  add_newline b; add_newline b;
  add_string b "  "; add_string b link;
  add_newline b; add_newline b;
  add_sentence b (s_ "Here are the instructions:");
  add_newline b; add_string b "1. ";
  add_sentence b (s_ "Click on the link.");
  add_newline b; add_string b "2. ";
  add_sentence b (s_ "Click on \"Generate a new key pair\".");
  add_newline b; add_string b "3. ";
  add_sentence b (s_ "Download your private key. Make sure you SAVE IT properly otherwise it will not be possible to tally and the election will be canceled.");
  add_newline b; add_string b "4. ";
  add_sentence b (s_ "Save the fingerprint of your verification key. Once the election is open, you must check that it is present in the set of verification keys published by the server.");
  add_newline b; add_string b "5. ";
  add_sentence b (s_ "Click on \"Submit\" to send your verification key.");
  add_newline b; add_newline b;
  add_sentence b (s_ "Regarding your private key, it is crucial you save it (otherwise the election will be canceled) and store it securely (if your private key is known together with the private keys of the other trustees, then vote privacy is no longer guaranteed).");
  add_sentence b (s_ "We suggest two options:");
  add_newline b; add_string b "1. ";
  add_sentence b (s_ "you may store the key on a USB stick and store it in a safe;");
  add_newline b; add_string b "2. ";
  add_sentence b (s_ "or you may simply print it and store it in a safe.");
  add_newline b;
  add_sentence b (s_ "Of course, more cryptographic solutions are welcome as well.");
  add_newline b; add_newline b;
  add_sentence b (s_ "Thank you for your help,");
  add_newline b;
  contents b

let mail_trustee_generation_basic langs link =
  let* l = Web_i18n.get ~component:"admin" ~lang:(List.hd langs) in
  let open (val l) in
  let subject = s_ "Link to generate the decryption key" in
  let* bodies =
    Lwt_list.map_s (fun lang ->
        let* l = Web_i18n.get ~component:"admin" ~lang in
        return (mail_trustee_generation_basic_body l link)
      ) langs
  in
  let body = String.concat "\n\n----------\n\n" bodies in
  let body = body ^ "\n\n-- \n" ^ s_ "The election administrator" in
  return (subject, body)

let mail_trustee_generation_threshold_body l link =
  let open (val l : Belenios_ui.I18n.GETTEXT) in
  let open Mail_formatter in
  let b = create () in
  add_sentence b (s_ "Dear trustee,");
  add_newline b; add_newline b;
  add_sentence b (s_ "You will find below the link to generate your private decryption key, used to tally the election.");
  add_newline b; add_newline b;
  add_string b "  "; add_string b link;
  add_newline b; add_newline b;
  add_sentence b (s_ "Follow the instructions.");
  add_sentence b (s_ "There will be 3 steps.");
  add_sentence b (s_ "All trustees must have completed one step before you can proceed to the next one.");
  add_newline b; add_newline b;
  add_sentence b (s_ "Don't forget to save:");
  add_newline b; add_string b "1. ";
  add_sentence b (s_ "your private key. Make sure you SAVE IT properly otherwise you will not be able to participate to the tally and the election may be canceled;");
  add_newline b; add_string b "2. ";
  add_sentence b (s_ "the fingerprint of your public key;");
  add_sentence b (s_ "the fingerprint of your verification key.");
  add_newline b; add_newline b;
  add_sentence b (s_ "Once the election is open, you must check that the fingerprints of your two keys are present in the set of keys published by the server.");
  add_newline b; add_newline b;
  add_sentence b (s_ "Regarding your private key, it is crucial you save it (otherwise the election will be canceled) and store it securely (if your private key is known together with the private keys of the other trustees, then vote privacy is no longer guaranteed).");
  add_sentence b (s_ "We suggest two options:");
  add_newline b; add_string b "1. ";
  add_sentence b (s_ "you may store the key on a USB stick and store it in a safe;");
  add_newline b; add_string b "2. ";
  add_sentence b (s_ "or you may simply print it and store it in a safe.");
  add_newline b;
  add_sentence b (s_ "Of course, more cryptographic solutions are welcome as well.");
  add_newline b; add_newline b;
  add_sentence b (s_ "Thank you for your help,");
  add_newline b;
  contents b

let mail_trustee_generation_threshold langs link =
  let* l = Web_i18n.get ~component:"admin" ~lang:(List.hd langs) in
  let open (val l) in
  let subject = s_ "Link to generate the decryption key" in
  let* bodies =
    Lwt_list.map_s (fun lang ->
        let* l = Web_i18n.get ~component:"admin" ~lang in
        return (mail_trustee_generation_threshold_body l link)
      ) langs
  in
  let body = String.concat "\n\n----------\n\n" bodies in
  let body = body ^ "\n\n-- \n" ^ s_ "The election administrator" in
  return (subject, body)

let mail_trustee_tally_body l link =
  let open (val l : Belenios_ui.I18n.GETTEXT) in
  let open Mail_formatter in
  let b = create () in
  add_sentence b (s_ "Dear trustee,");
  add_newline b; add_newline b;
  add_sentence b (s_ "The election is now closed.");
  add_sentence b (s_ "Here is the link to proceed to tally:");
  add_newline b; add_newline b;
  add_string b "  "; add_string b link;
  add_newline b; add_newline b;
  add_sentence b (s_ "Instructions:");
  add_newline b; add_string b "1. ";
  add_sentence b (s_ "Follow the link.");
  add_newline b; add_string b "2. ";
  add_sentence b (s_ "Enter your private decryption key in the first box and click on \"Generate your contribution to decryption\".");
  add_newline b; add_string b "3. ";
  add_sentence b (s_ "The second box is now filled with crypto material. Please press the button \"Submit\".");
  add_newline b; add_newline b;
  add_sentence b (s_ "Thank you again for your help,");
  add_newline b;
  contents b

let mail_trustee_tally langs link =
  let* l = Web_i18n.get ~component:"admin" ~lang:(List.hd langs) in
  let open (val l) in
  let subject = s_ "Link to tally the election" in
  let* bodies =
    Lwt_list.map_s (fun lang ->
        let* l = Web_i18n.get ~component:"admin" ~lang in
        return (mail_trustee_tally_body l link)
      ) langs
  in
  let body = String.concat "\n\n----------\n\n" bodies in
  let body = body ^ "\n\n-- \n" ^ s_ "The election administrator" in
  return (subject, body)

let mail_shuffle_body l link =
  let open (val l : Belenios_ui.I18n.GETTEXT) in
  let open Mail_formatter in
  let b = create () in
  add_sentence b (s_ "Dear trustee,");
  add_newline b; add_newline b;
  add_sentence b (s_ "Below you will find the link to shuffle encrypted ballots.");
  add_newline b; add_newline b;
  add_string b "  "; add_string b link;
  add_newline b; add_newline b;
  add_sentence b (s_ "Instructions:");
  add_newline b; add_string b "1. ";
  add_sentence b (s_ "Follow the link.");
  add_newline b; add_string b "2. ";
  add_sentence b (s_ "Click on \"Compute shuffle\".");
  add_newline b; add_string b "3. ";
  add_sentence b (s_ "The fingerprint of your shuffle will appear. Save it.");
  add_newline b; add_string b "4. ";
  add_sentence b (s_ "When the election result is published, make sure that the fingerprint of your shuffle appears in the result page.");
  add_newline b; add_newline b;
  add_sentence b (s_ "Thank you for your help,");
  add_newline b;
  contents b

let mail_shuffle langs link =
  let* l = Web_i18n.get ~component:"admin" ~lang:(List.hd langs) in
  let open (val l) in
  let subject = s_ "Link to shuffle encrypted ballots" in
  let* bodies =
    Lwt_list.map_s (fun lang ->
        let* l = Web_i18n.get ~component:"admin" ~lang in
        return (mail_shuffle_body l link)
      ) langs
  in
  let body = String.concat "\n\n----------\n\n" bodies in
  let body = body ^ "\n\n-- \n" ^ s_ "The election administrator" in
  return (subject, body)
