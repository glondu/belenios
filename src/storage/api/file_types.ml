(**************************************************************************)
(*                                BELENIOS                                *)
(*                                                                        *)
(*  Copyright © 2024-2024 Inria                                           *)
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
open Types
open Extra

type abstract

type _ election_file =
  | State : election_state election_file
  | State_state : state_state election_file
  | Dates : election_dates election_file
  | Metadata : metadata election_file
  | Server_seed : string election_file
  | Private_keys :
      ('a, 'b) group
      -> ('a, 'b) sent_partial_decryption_key list election_file
  | Audit_cache : audit_cache election_file
  | Archive_header : Archive.header election_file
  | Last_event : last_event election_file
  | Draft : wrapped_draft_election election_file
  | Public_creds : ('a, 'b) group -> 'a public_credentials_with_id election_file
  | Private_creds : private_credentials election_file
  | Sealing_log : string election_file
  | Records : election_records election_file
  | Voters : Voter.t list election_file
  | Confidential_archive : abstract election_file
  | Extended_record : string -> extended_record election_file
  | Credential_mapping : string -> credential_mapping election_file
  | Data : hash -> string election_file
  | Roots : roots election_file
  | Voters_config : voters_config election_file
  | Voter : string -> Voter.t election_file
  | Credential_weight : string -> Weight.t election_file
  | Credential_user : string -> string election_file

type _ credentials_file =
  | Credentials_params : wrapped_credentials_params credentials_file
  | Credentials_metadata : Belenios_web_api.message_metadata credentials_file
  | Credentials_seed : credentials_seed credentials_file
  | Credentials_records :
      ('a, 'b) group
      -> ('a, 'b) credentials_records credentials_file
  | Credentials_credits : Belenios_web_api.credentials_credits credentials_file

type admin_password_kind = Username of string | Address of string

type _ account_file =
  | Account : int -> account account_file
  | Auth_db : string -> string list account_file
  | Admin_password :
      string * admin_password_kind
      -> password_record account_file
