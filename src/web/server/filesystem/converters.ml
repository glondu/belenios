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

type 'a t = 'a string_serializers

let raw_draft_election_of_concrete x se_private_creds_downloaded =
  let open Serializable_j in
  let {
    se_version;
    se_owners;
    se_group;
    se_voters;
    se_questions;
    se_trustees;
    se_metadata;
    se_public_creds;
    se_public_creds_received;
    se_public_creds_certificate;
    se_creation_date;
    se_administrator;
    se_credential_authority_visited;
    se_voter_authentication_visited;
    se_trustees_setup_step;
    se_pending_credentials;
  } =
    x
  in
  let open Belenios_storage_api in
  {
    se_version;
    se_owners;
    se_group;
    se_voters;
    se_questions;
    se_trustees;
    se_metadata;
    se_public_creds;
    se_public_creds_received;
    se_public_creds_certificate;
    se_creation_date = Datetime.to_unixfloat se_creation_date;
    se_administrator;
    se_credential_authority_visited;
    se_voter_authentication_visited;
    se_trustees_setup_step;
    se_pending_credentials;
    se_private_creds_downloaded;
  }

let raw_draft_election_to_concrete x =
  let open Belenios_storage_api in
  let {
    se_version;
    se_owners;
    se_group;
    se_voters;
    se_questions;
    se_trustees;
    se_metadata;
    se_public_creds;
    se_public_creds_received;
    se_public_creds_certificate;
    se_creation_date;
    se_administrator;
    se_credential_authority_visited;
    se_voter_authentication_visited;
    se_trustees_setup_step;
    se_pending_credentials;
    se_private_creds_downloaded;
  } =
    x
  in
  let open Serializable_j in
  ( {
      se_version;
      se_owners;
      se_group;
      se_voters;
      se_questions;
      se_trustees;
      se_metadata;
      se_public_creds;
      se_public_creds_received;
      se_public_creds_certificate;
      se_creation_date = Datetime.from_unixfloat se_creation_date;
      se_administrator;
      se_credential_authority_visited;
      se_voter_authentication_visited;
      se_trustees_setup_step;
      se_pending_credentials;
    },
    se_private_creds_downloaded )

let account =
  {
    of_string =
      (fun x ->
        let open Serializable_j in
        let {
          id;
          name;
          email;
          last_connected;
          authentications;
          consent;
          capabilities;
          language;
          default_voter_languages;
          default_contact;
          voters_limit;
        } =
          account_of_string x
        in
        let open Belenios_storage_api in
        {
          id;
          name;
          email;
          last_connected = Datetime.to_unixfloat last_connected;
          authentications;
          consent = Option.map Datetime.to_unixfloat consent;
          capabilities;
          language;
          default_voter_languages;
          default_contact;
          voters_limit;
        });
    to_string =
      (fun x ->
        let open Belenios_storage_api in
        let {
          id;
          name;
          email;
          last_connected;
          authentications;
          consent;
          capabilities;
          language;
          default_voter_languages;
          default_contact;
          voters_limit;
        } =
          x
        in
        let open Serializable_j in
        let x =
          {
            id;
            name;
            email;
            last_connected = Datetime.from_unixfloat last_connected;
            authentications;
            consent = Option.map Datetime.from_unixfloat consent;
            capabilities;
            language;
            default_voter_languages;
            default_contact;
            voters_limit;
          }
        in
        string_of_account x);
  }
