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

open Belenios_server_core

type mail_kind =
  | MailCredential of uuid
  | MailPassword of uuid
  | MailConfirmation of uuid
  | MailAutomaticWarning of uuid
  | MailAccountCreation
  | MailPasswordChange
  | MailLogin
  | MailSetEmail

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
  | Generic of {
      kind : mail_kind;
      recipient : string * string;
      subject : string;
      body : string;
    }

val send : t -> unit Lwt.t
