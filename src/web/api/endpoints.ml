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
open Serializable_j
open Common

let string_of_unit () = ""
let unit_of_string _ = ()

type ('user, 'get, 'post) t = {
  path : string;
  of_string : string -> 'get;
  to_string : 'get -> string;
  to_string_post : 'post -> string;
}

type nobody = [ `Nobody ]
type admin = [ `Admin of string ]
type credauth = [ `Credauth of string ]
type trustee = [ `Trustee of string ]
type user = [ nobody | admin | credauth | trustee ]

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

let draft uuid =
  {
    path = Printf.sprintf "elections/%s/draft" (Uuid.unwrap uuid);
    of_string = draft_of_string;
    to_string = string_of_draft;
    to_string_post = string_of_draft_request;
  }

let draft_status uuid =
  {
    path = Printf.sprintf "elections/%s/draft/status" (Uuid.unwrap uuid);
    of_string = draft_status_of_string;
    to_string = string_of_draft_status;
    to_string_post = string_of_unit;
  }

let draft_voters uuid =
  {
    path = Printf.sprintf "elections/%s/draft/voters" (Uuid.unwrap uuid);
    of_string = voter_list_of_string;
    to_string = string_of_voter_list;
    to_string_post = string_of_voters_request;
  }

let draft_passwords uuid =
  {
    path = Printf.sprintf "elections/%s/draft/passwords" (Uuid.unwrap uuid);
    of_string = string_list_of_string;
    to_string = string_of_string_list;
    to_string_post = string_of_voter_list;
  }

let draft_public_credentials uuid =
  {
    path =
      Printf.sprintf "elections/%s/draft/credentials/public" (Uuid.unwrap uuid);
    of_string = public_credentials_of_string;
    to_string = string_of_public_credentials;
    to_string_post = string_of_public_credentials;
  }

let draft_private_credentials uuid =
  {
    path =
      Printf.sprintf "elections/%s/draft/credentials/private" (Uuid.unwrap uuid);
    of_string = private_credentials_of_string;
    to_string = string_of_private_credentials;
    to_string_post = string_of_unit;
  }

let draft_credentials_token uuid =
  {
    path =
      Printf.sprintf "elections/%s/draft/credentials/token" (Uuid.unwrap uuid);
    of_string = Fun.id;
    to_string = Fun.id;
    to_string_post = string_of_unit;
  }

let draft_trustees uuid =
  {
    path = Printf.sprintf "elections/%s/draft/trustees" (Uuid.unwrap uuid);
    of_string = Fun.id;
    to_string = Fun.id;
    to_string_post = string_of_trustees_request;
  }

let draft_trustee uuid x =
  {
    path =
      Printf.sprintf "elections/%s/draft/trustees/%s" (Uuid.unwrap uuid)
        (Uri.pct_encode x);
    of_string = unit_of_string;
    to_string = string_of_unit;
    to_string_post = string_of_unit;
  }

let trustee_draft uuid =
  {
    path = Printf.sprintf "elections/%s/draft/trustee" (Uuid.unwrap uuid);
    of_string = Fun.id;
    to_string = Fun.id;
    to_string_post = Fun.id;
  }

let trustee_election uuid =
  {
    path = Printf.sprintf "elections/%s/trustee" (Uuid.unwrap uuid);
    of_string = tally_trustee_of_string;
    to_string = string_of_tally_trustee;
    to_string_post = Fun.id;
  }

let elections =
  {
    path = "elections";
    of_string = summary_list_of_string;
    to_string = string_of_summary_list;
    to_string_post = string_of_draft;
  }

let election uuid =
  {
    path = Printf.sprintf "elections/%s/election" (Uuid.unwrap uuid);
    of_string = Fun.id;
    to_string = Fun.id;
    to_string_post = string_of_unit;
  }

let election_logo uuid =
  {
    path = Printf.sprintf "elections/%s/logo" (Uuid.unwrap uuid);
    of_string = Fun.id;
    to_string = Fun.id;
    to_string_post = string_of_unit;
  }

let election_trustees uuid =
  {
    path = Printf.sprintf "elections/%s/trustees" (Uuid.unwrap uuid);
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

let election_sealing_log uuid =
  {
    path = Printf.sprintf "elections/%s/sealing-log" (Uuid.unwrap uuid);
    of_string = Fun.id;
    to_string = Fun.id;
    to_string_post = string_of_unit;
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

let election_nh_ciphertexts uuid =
  {
    path = Printf.sprintf "elections/%s/nh-ciphertexts" (Uuid.unwrap uuid);
    of_string = Fun.id;
    to_string = Fun.id;
    to_string_post = string_of_unit;
  }

let election_encrypted_tally uuid =
  {
    path = Printf.sprintf "elections/%s/encrypted-tally" (Uuid.unwrap uuid);
    of_string = Fun.id;
    to_string = Fun.id;
    to_string_post = string_of_unit;
  }

let election_partial_decryptions uuid =
  {
    path = Printf.sprintf "elections/%s/partial-decryptions" (Uuid.unwrap uuid);
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

let election_roots uuid =
  {
    path = Printf.sprintf "elections/%s/roots" (Uuid.unwrap uuid);
    of_string = roots_of_string;
    to_string = string_of_roots;
    to_string_post = string_of_unit;
  }

let election_last_event uuid =
  {
    path = Printf.sprintf "elections/%s/last-event" (Uuid.unwrap uuid);
    of_string = last_event_of_string;
    to_string = string_of_last_event;
    to_string_post = string_of_unit;
  }

let election_object uuid x =
  {
    path =
      Printf.sprintf "elections/%s/objects/%s" (Uuid.unwrap uuid)
        (Hash.to_hex x);
    of_string = Fun.id;
    to_string = Fun.id;
    to_string_post = string_of_unit;
  }

let election_audit_cache uuid =
  {
    path = Printf.sprintf "elections/%s/audit-cache" (Uuid.unwrap uuid);
    of_string = audit_cache_of_string;
    to_string = string_of_audit_cache;
    to_string_post = string_of_unit;
  }

let election_ballots uuid =
  {
    path = Printf.sprintf "elections/%s/ballots" (Uuid.unwrap uuid);
    of_string = ballots_with_weights_of_string;
    to_string = string_of_ballots_with_weights;
    to_string_post = Fun.id;
  }

let credentials_server =
  {
    path = "credentials/server";
    of_string = unit_of_string;
    to_string = string_of_unit;
    to_string_post = string_of_credentials_request;
  }
