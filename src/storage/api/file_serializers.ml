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
open File_types
open Types
open Extra

let get_election (type t) : t election_file -> t serializers = function
  | State ->
      {
        of_string = !*election_state_of_yojson;
        to_string = !+yojson_of_election_state;
      }
  | State_state ->
      {
        of_string = !*state_state_of_yojson;
        to_string = !+yojson_of_state_state;
      }
  | Dates ->
      {
        of_string = !*election_dates_of_yojson;
        to_string = !+yojson_of_election_dates;
      }
  | Metadata ->
      { of_string = !*metadata_of_yojson; to_string = !+yojson_of_metadata }
  | Server_seed -> { of_string = Fun.id; to_string = Fun.id }
  | Private_keys w ->
      let module G = (val w) in
      {
        of_string =
          (fun xs ->
            xs |> split_lines
            |> List.map !*[%group_of_yojson: _ sent_partial_decryption_key]);
        to_string =
          (fun xs ->
            xs
            |> List.map !+[%yojson_of_group: _ sent_partial_decryption_key]
            |> join_lines);
      }
  | Audit_cache ->
      {
        of_string = !*audit_cache_of_yojson;
        to_string = !+yojson_of_audit_cache;
      }
  | Archive_header ->
      {
        of_string = !*Archive.header_of_yojson;
        to_string = !+Archive.yojson_of_header;
      }
  | Last_event ->
      { of_string = !*last_event_of_yojson; to_string = !+yojson_of_last_event }
  | Draft ->
      {
        of_string = !*wrapped_draft_election_of_yojson;
        to_string = !+yojson_of_wrapped_draft_election;
      }
  | Public_creds w ->
      let module G = (val w) in
      {
        of_string = !*(public_credentials_with_id_of_yojson !$G.of_string);
        to_string = !+(yojson_of_public_credentials_with_id !&G.to_string);
      }
  | Private_creds ->
      {
        of_string = !*private_credentials_of_yojson;
        to_string = !+yojson_of_private_credentials;
      }
  | Sealing_log -> { of_string = Fun.id; to_string = Fun.id }
  | Records ->
      {
        of_string = !*election_records_of_yojson;
        to_string = !+yojson_of_election_records;
      }
  | Voters ->
      { of_string = Voter.list_of_string; to_string = Voter.list_to_string }
  | Confidential_archive ->
      {
        of_string = (fun _ -> invalid_arg "Confidential_archive.of_string");
        to_string = (fun _ -> invalid_arg "Confidential_archive.to_string");
      }
  | Extended_record _ ->
      {
        of_string = !*extended_record_of_yojson;
        to_string = !+yojson_of_extended_record;
      }
  | Credential_mapping _ ->
      {
        of_string = !*credential_mapping_of_yojson;
        to_string = !+yojson_of_credential_mapping;
      }
  | Data _ -> { of_string = Fun.id; to_string = Fun.id }
  | Roots -> { of_string = !*roots_of_yojson; to_string = !+yojson_of_roots }
  | Voters_config ->
      {
        of_string = !*voters_config_of_yojson;
        to_string = !+yojson_of_voters_config;
      }
  | Voter _ -> { of_string = Voter.of_string; to_string = Voter.to_string }
  | Credential_weight _ ->
      { of_string = Weight.of_string; to_string = Weight.to_string }
  | Credential_user _ -> { of_string = Fun.id; to_string = Fun.id }
  | Credentials_params ->
      {
        of_string = !*wrapped_credentials_params_of_yojson;
        to_string = !+yojson_of_wrapped_credentials_params;
      }
  | Credentials_metadata ->
      {
        of_string = !*Belenios_web_api.message_metadata_of_yojson;
        to_string = !+Belenios_web_api.yojson_of_message_metadata;
      }
  | Credentials_seed ->
      {
        of_string = !*credentials_seed_of_yojson;
        to_string = !+yojson_of_credentials_seed;
      }
  | Credentials_records w ->
      let module G = (val w) in
      {
        of_string = !*[%group_of_yojson: _ credentials_records];
        to_string = !+[%yojson_of_group: _ credentials_records];
      }
  | Credentials_credits ->
      {
        of_string = !*Belenios_web_api.credentials_credits_of_yojson;
        to_string = !+Belenios_web_api.yojson_of_credentials_credits;
      }

let get_account (type t) : t account_file -> t serializers = function
  | Account _ ->
      { of_string = !*account_of_yojson; to_string = !+yojson_of_account }
  | Auth_db _ -> { of_string = split_lines; to_string = join_lines }
  | Admin_password _ ->
      {
        of_string = !*password_record_of_yojson;
        to_string = !+yojson_of_password_record;
      }
