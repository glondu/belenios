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
        of_string = !*stored_election_state_d;
        to_string = !+stored_election_state_d;
      }
  | Dates -> { of_string = !*election_dates_d; to_string = !+election_dates_d }
  | Metadata -> { of_string = !*metadata_d; to_string = !+metadata_d }
  | Server_seed -> { of_string = Fun.id; to_string = Fun.id }
  | Audit_cache -> { of_string = !*audit_cache_d; to_string = !+audit_cache_d }
  | Last_event -> { of_string = !*last_event_d; to_string = !+last_event_d }
  | Draft ->
      {
        of_string = !*wrapped_draft_election_d;
        to_string = !+wrapped_draft_election_d;
      }
  | Public_creds w ->
      let module G = (val Group.coerce w) in
      {
        of_string = !*(public_credentials_with_id_d G.t_d);
        to_string = !+(public_credentials_with_id_d G.t_d);
      }
  | Private_creds ->
      {
        of_string = !*private_credentials_d;
        to_string = !+private_credentials_d;
      }
  | Sealing_log -> { of_string = Fun.id; to_string = Fun.id }
  | Records ->
      {
        of_string = !*Belenios_web_api.election_records_d;
        to_string = !+Belenios_web_api.election_records_d;
      }
  | Voters -> { of_string = !*voters_d; to_string = !+voters_d }
  | Confidential_archive ->
      {
        of_string = (fun _ -> invalid_arg "Confidential_archive.of_string");
        to_string = (fun _ -> invalid_arg "Confidential_archive.to_string");
      }
  | Ballots_info ->
      {
        of_string = !*Belenios_web_api.ballots_info_d;
        to_string = !+Belenios_web_api.ballots_info_d;
      }

let get_trustees (type t) : t trustees_file -> t serializers = function
  | Trustees_metadata ->
      {
        of_string = !*Belenios_web_api.trustees_metadata_d;
        to_string = !+Belenios_web_api.trustees_metadata_d;
      }
  | Trustees w ->
      let module G = (val Group.coerce w) in
      {
        of_string = !*[%desc_of_group: _ trustees];
        to_string = !+[%desc_of_group: _ trustees];
      }
  | Trustees_draft w ->
      let module G = (val Group.coerce w) in
      {
        of_string = !*[%desc_of_group: _ draft_trustees];
        to_string = !+[%desc_of_group: _ draft_trustees];
      }
  | Trustees_private_keys w ->
      let module G = (val Group.coerce w) in
      {
        of_string =
          (fun xs ->
            xs |> split_lines
            |> List.map !*[%desc_of_group: _ sent_partial_decryption_key]);
        to_string =
          (fun xs ->
            xs
            |> List.map !+[%desc_of_group: _ sent_partial_decryption_key]
            |> join_lines);
      }

let get_credentials (type t) : t credentials_file -> t serializers = function
  | Credentials_params ->
      {
        of_string = !*wrapped_credentials_params_d;
        to_string = !+wrapped_credentials_params_d;
      }
  | Credentials_metadata ->
      {
        of_string = !*Belenios_web_api.message_metadata_d;
        to_string = !+Belenios_web_api.message_metadata_d;
      }
  | Credentials_seed ->
      { of_string = !*credentials_seed_d; to_string = !+credentials_seed_d }
  | Credentials_records w ->
      let module G = (val Group.coerce w) in
      {
        of_string = !*[%desc_of_group: _ credentials_records];
        to_string = !+[%desc_of_group: _ credentials_records];
      }
  | Credentials_credits ->
      {
        of_string = !*Belenios_web_api.credentials_credits_d;
        to_string = !+Belenios_web_api.credentials_credits_d;
      }

let get_account (type t) : t account_file -> t serializers = function
  | Account _ -> { of_string = !*account_d; to_string = !+account_d }
  | Auth_db _ -> { of_string = split_lines; to_string = join_lines }
  | Admin_password _ ->
      { of_string = !*password_record_d; to_string = !+password_record_d }
