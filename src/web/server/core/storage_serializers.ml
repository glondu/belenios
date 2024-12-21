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
open Serializable_j
open Storage_sig

let get_election_file_serializers (type t) :
    t election_file -> t string_serializers = function
  | State ->
      {
        of_string = election_state_of_string;
        to_string = string_of_election_state;
      }
  | State_state ->
      Belenios_storage_api.
        { of_string = state_state_of_string; to_string = string_of_state_state }
  | Dates_full ->
      Belenios_storage_api.
        {
          of_string = election_dates_of_string;
          to_string = string_of_election_dates;
        }
  | Metadata ->
      { of_string = metadata_of_string; to_string = string_of_metadata }
  | Private_key ->
      { of_string = Yojson.Safe.from_string; to_string = Yojson.Safe.to_string }
  | Private_keys -> { of_string = split_lines; to_string = join_lines }
  | Audit_cache ->
      { of_string = audit_cache_of_string; to_string = string_of_audit_cache }
  | Last_event ->
      { of_string = last_event_of_string; to_string = string_of_last_event }
  | Deleted ->
      {
        of_string = deleted_election_of_string;
        to_string = string_of_deleted_election;
      }
  | Private_creds_downloaded ->
      { of_string = (fun _ -> ()); to_string = (fun () -> "") }
  | Draft ->
      Core.
        {
          of_string = draft_election_of_string;
          to_string = string_of_draft_election;
        }
  | Public_creds ->
      {
        of_string = public_credentials_of_string;
        to_string = string_of_public_credentials;
      }
  | Private_creds ->
      {
        of_string = private_credentials_of_string;
        to_string = string_of_private_credentials;
      }
  | Public_archive ->
      {
        of_string = (fun _ -> invalid_arg "Public_archive.of_string");
        to_string = (fun _ -> invalid_arg "Public_archive.to_string");
      }
  | Passwords -> Core.{ of_string = csv_of_string; to_string = string_of_csv }
  | Records_new ->
      Belenios_storage_api.
        {
          of_string = election_records_of_string;
          to_string = string_of_election_records;
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
        of_string = extended_record_of_string;
        to_string = string_of_extended_record;
      }
  | Credential_mapping _ ->
      {
        of_string = credential_mapping_of_string;
        to_string = string_of_credential_mapping;
      }
  | Data _ -> { of_string = Fun.id; to_string = Fun.id }
  | Roots -> { of_string = roots_of_string; to_string = string_of_roots }
  | Voters_config ->
      {
        of_string = voters_config_of_string;
        to_string = string_of_voters_config;
      }
  | Voter _ -> { of_string = Voter.of_string; to_string = Voter.to_string }
  | Credential_weight _ ->
      { of_string = Weight.of_string; to_string = Weight.to_string }
  | Credential_user _ -> { of_string = Fun.id; to_string = Fun.id }
  | Password _ ->
      {
        of_string = password_record_of_string;
        to_string = string_of_password_record;
      }

let get_file_serializers (type t) : t file -> t string_serializers = function
  | Spool_version -> { of_string = int_of_string; to_string = string_of_int }
  | Account_counter -> { of_string = int_of_string; to_string = string_of_int }
  | Account _ ->
      { of_string = account_of_string; to_string = string_of_account }
  | Election (_, f) -> get_election_file_serializers f
  | Auth_db _ -> { of_string = split_lines; to_string = join_lines }
  | Admin_password _ ->
      {
        of_string = password_record_of_string;
        to_string = string_of_password_record;
      }

let some (type a b) (f : a file) (spec : (a, b) string_or_value_spec) (x : b) =
  let s = get_file_serializers f in
  match spec with
  | String -> Types.Lopt.some_string s.of_string x
  | Value -> Types.Lopt.some_value s.to_string x
