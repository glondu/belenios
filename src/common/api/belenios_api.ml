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

open Belenios
module Serializable_t = Serializable_t
module Serializable_j = Serializable_j
open Serializable_j

type draft =
  | Draft : 'a Belenios.Election.version * 'a Serializable_t.draft -> draft

let draft_of_string x =
  let abstract = Serializable_j.draft_of_string Yojson.Safe.read_json x in
  let open Belenios.Election in
  match version_of_int abstract.draft_version with
  | Version v ->
      let open (val get_serializers v) in
      let x = Serializable_j.draft_of_string read_question x in
      Draft (v, x)

let string_of_draft (Draft (v, x)) =
  let open (val Belenios.Election.get_serializers v) in
  Serializable_j.string_of_draft write_question x

let string_of_unit () = ""
let unit_of_string _ = ()

module Endpoints = struct
  type ('a, 'b) t = {
    path : string;
    of_string : string -> 'a;
    to_string : 'a -> string;
    to_string_post : 'b -> string;
  }

  let configuration =
    {
      path = "configuration";
      of_string = configuration_of_string;
      to_string = string_of_configuration;
      to_string_post = string_of_unit;
    }

  let account =
    {
      path = "account";
      of_string = api_account_of_string;
      to_string = string_of_api_account;
      to_string_post = string_of_unit;
    }

  let drafts =
    {
      path = "drafts";
      of_string = summary_list_of_string;
      to_string = string_of_summary_list;
      to_string_post = string_of_draft;
    }

  let draft uuid =
    {
      path = Printf.sprintf "drafts/%s" (Uuid.unwrap uuid);
      of_string = draft_of_string;
      to_string = string_of_draft;
      to_string_post = string_of_draft_request;
    }

  let draft_status uuid =
    {
      path = Printf.sprintf "drafts/%s/status" (Uuid.unwrap uuid);
      of_string = draft_status_of_string;
      to_string = string_of_draft_status;
      to_string_post = string_of_unit;
    }

  let draft_voters uuid =
    {
      path = Printf.sprintf "drafts/%s/voters" (Uuid.unwrap uuid);
      of_string = voter_list_of_string;
      to_string = string_of_voter_list;
      to_string_post = string_of_voters_request;
    }

  let draft_passwords uuid =
    {
      path = Printf.sprintf "drafts/%s/passwords" (Uuid.unwrap uuid);
      of_string = string_list_of_string;
      to_string = string_of_string_list;
      to_string_post = string_of_voter_list;
    }

  let draft_credentials uuid =
    {
      path = Printf.sprintf "drafts/%s/credentials" (Uuid.unwrap uuid);
      of_string = unit_of_string;
      to_string = string_of_unit;
      to_string_post = string_of_public_credentials;
    }

  let draft_public_credentials uuid =
    {
      path = Printf.sprintf "drafts/%s/credentials/public" (Uuid.unwrap uuid);
      of_string = public_credentials_of_string;
      to_string = string_of_public_credentials;
      to_string_post = string_of_public_credentials;
    }

  let draft_private_credentials uuid =
    {
      path = Printf.sprintf "drafts/%s/credentials/private" (Uuid.unwrap uuid);
      of_string = private_credentials_of_string;
      to_string = string_of_private_credentials;
      to_string_post = string_of_unit;
    }

  let draft_credentials_token uuid =
    {
      path = Printf.sprintf "drafts/%s/credentials/token" (Uuid.unwrap uuid);
      of_string = Fun.id;
      to_string = Fun.id;
      to_string_post = string_of_unit;
    }

  let draft_trustees uuid =
    {
      path = Printf.sprintf "drafts/%s/trustees" (Uuid.unwrap uuid);
      of_string =
        draft_trustees_of_string Yojson.Safe.read_json Yojson.Safe.read_json;
      to_string =
        string_of_draft_trustees Yojson.Safe.write_json Yojson.Safe.write_json;
      to_string_post = string_of_trustees_request;
    }

  let draft_trustee uuid x =
    {
      path =
        Printf.sprintf "drafts/%s/trustees/%s" (Uuid.unwrap uuid)
          (Uri.pct_encode x);
      of_string = unit_of_string;
      to_string = string_of_unit;
      to_string_post = string_of_unit;
    }

  let elections =
    {
      path = "elections";
      of_string = summary_list_of_string;
      to_string = string_of_summary_list;
      to_string_post = string_of_unit;
    }

  let election uuid =
    {
      path = Printf.sprintf "elections/%s/election" (Uuid.unwrap uuid);
      of_string = Fun.id;
      to_string = Fun.id;
      to_string_post = string_of_unit;
    }

  let election_status uuid =
    {
      path = Printf.sprintf "elections/%s" (Uuid.unwrap uuid);
      of_string = election_status_of_string;
      to_string = string_of_election_status;
      to_string_post = string_of_admin_request;
    }

  let election_auto_dates uuid =
    {
      path = Printf.sprintf "elections/%s/automatic-dates" (Uuid.unwrap uuid);
      of_string = election_auto_dates_of_string;
      to_string = string_of_election_auto_dates;
      to_string_post = string_of_unit;
    }

  let election_voters uuid =
    {
      path = Printf.sprintf "elections/%s/voters" (Uuid.unwrap uuid);
      of_string = voter_list_of_string;
      to_string = string_of_voter_list;
      to_string_post = string_of_unit;
    }

  let election_records uuid =
    {
      path = Printf.sprintf "elections/%s/records" (Uuid.unwrap uuid);
      of_string = records_of_string;
      to_string = string_of_records;
      to_string_post = string_of_unit;
    }

  let election_partial_decryptions uuid =
    {
      path =
        Printf.sprintf "elections/%s/partial-decryptions" (Uuid.unwrap uuid);
      of_string = partial_decryptions_of_string;
      to_string = string_of_partial_decryptions;
      to_string_post = string_of_unit;
    }

  let election_shuffles uuid =
    {
      path = Printf.sprintf "elections/%s/shuffles" (Uuid.unwrap uuid);
      of_string = shuffles_of_string;
      to_string = string_of_shuffles;
      to_string_post = string_of_unit;
    }

  let election_shuffle uuid x =
    {
      path =
        Printf.sprintf "elections/%s/shuffles/%s" (Uuid.unwrap uuid)
          (Uri.pct_encode x);
      of_string = unit_of_string;
      to_string = string_of_unit;
      to_string_post = string_of_shuffler_request;
    }
end
