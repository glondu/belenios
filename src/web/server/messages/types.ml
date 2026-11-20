(**************************************************************************)
(*                                BELENIOS                                *)
(*                                                                        *)
(*  Copyright © 2012-2025 Inria                                           *)
(*  Copyright © 2026 VCAST                                                *)
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

open Belenios_core
open Belenios_web_api

type material_message = {
  recipient : recipient;
  material : string;
  weight : weight option;
  metadata : message_metadata option;
}
[@@deriving desc]

type code_message = {
  lang : string;
  recipient : recipient;
  uuid : uuid option;
  state : string option;
  code : string;
}
[@@deriving desc]

type confirmation_message = {
  lang : string;
  uuid : uuid;
  title : string;
  contact : string option;
  confirmation : confirmation;
}
[@@deriving desc]

type trustee_link_message = {
  recipient : recipient;
  uuid : uuid;
  token : string;
  langs : string list;
  belenios_url : string;
  admin_id : int;
}
[@@deriving desc]

type credentials_seed_message = {
  uuid : uuid;
  admin_id : int;
  lang : string;
  belenios_url : string;
  cred_authority_info : cred_authority_info;
  seed : string;
}
[@@deriving desc]

type message =
  [ `Account_create of code_message
  | `Account_change_password of code_message
  | `Account_set_email of code_message
  | `Voter_credential of material_message
  | `Vote_confirmation of confirmation_message
  | `Mail_login of code_message
  | `Trustee_link of trustee_link_message
  | `Credentials_seed of credentials_seed_message ]
[@@deriving desc]

type message_payload = { timestamp : int64; message : message }
[@@deriving desc]
