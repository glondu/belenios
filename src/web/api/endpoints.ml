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
open Types
open Common
open Ppx_yojson_conv_lib.Yojson_conv

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
    of_string = !*configuration_of_yojson;
    to_string = !+yojson_of_configuration;
    to_string_post = !+yojson_of_unit;
  }

let account =
  {
    path = "account";
    of_string = !*api_account_of_yojson;
    to_string = !+yojson_of_api_account;
    to_string_post = !+yojson_of_unit;
  }

let draft uuid =
  {
    path = Printf.sprintf "elections/%s/draft" (Uuid.to_string uuid);
    of_string = !*draft_of_yojson;
    to_string = !+yojson_of_draft;
    to_string_post = !+yojson_of_draft_request;
  }

let draft_status uuid =
  {
    path = Printf.sprintf "elections/%s/draft/status" (Uuid.to_string uuid);
    of_string = !*draft_status_of_yojson;
    to_string = !+yojson_of_draft_status;
    to_string_post = !+yojson_of_unit;
  }

let draft_voters uuid =
  {
    path = Printf.sprintf "elections/%s/draft/voters" (Uuid.to_string uuid);
    of_string = !*voter_list_of_yojson;
    to_string = !+yojson_of_voter_list;
    to_string_post = !+yojson_of_voters_request;
  }

let draft_public_credentials uuid (type a b) (w : (a, b) group) =
  let module G = (val w) in
  {
    path =
      Printf.sprintf "elections/%s/draft/credentials/public"
        (Uuid.to_string uuid);
    of_string = !*(public_credentials_of_yojson !$G.of_string);
    to_string = !+(yojson_of_public_credentials !&G.to_string);
    to_string_post = !+(yojson_of_public_credentials_with_id !&G.to_string);
  }

let draft_private_credentials uuid =
  {
    path =
      Printf.sprintf "elections/%s/draft/credentials/private"
        (Uuid.to_string uuid);
    of_string = !*private_credentials_of_yojson;
    to_string = !+yojson_of_private_credentials;
    to_string_post = !+yojson_of_unit;
  }

let draft_credentials_token uuid =
  {
    path =
      Printf.sprintf "elections/%s/draft/credentials/token"
        (Uuid.to_string uuid);
    of_string = Fun.id;
    to_string = Fun.id;
    to_string_post = !+yojson_of_unit;
  }

let draft_trustees uuid (type a b) (w : (a, b) group) =
  let module G = (val w) in
  {
    path = Printf.sprintf "elections/%s/draft/trustees" (Uuid.to_string uuid);
    of_string = !*[%group_of_yojson: _ draft_trustees];
    to_string = !+[%yojson_of_group: _ draft_trustees];
    to_string_post = !+yojson_of_trustees_request;
  }

let draft_trustee uuid x =
  {
    path =
      Printf.sprintf "elections/%s/draft/trustees/%s" (Uuid.to_string uuid)
        (Uri.pct_encode x);
    of_string = !*unit_of_yojson;
    to_string = !+yojson_of_unit;
    to_string_post = !+yojson_of_unit;
  }

let trustee_draft uuid (type a b) (w : (a, b) group) =
  let module G = (val w) in
  {
    path = Printf.sprintf "elections/%s/draft/trustee" (Uuid.to_string uuid);
    of_string = !*[%group_of_yojson: _ trustee_status];
    to_string = !+[%yojson_of_group: _ trustee_status];
    to_string_post = Json.to_string;
  }

let trustee_election uuid (type a b) (w : (a, b) group) =
  let module G = (val w) in
  {
    path = Printf.sprintf "elections/%s/trustee" (Uuid.to_string uuid);
    of_string = !*[%group_of_yojson: _ tally_trustee];
    to_string = !+[%yojson_of_group: _ tally_trustee];
    to_string_post = Json.to_string;
  }

let elections =
  {
    path = "elections";
    of_string = !*summary_list_of_yojson;
    to_string = !+yojson_of_summary_list;
    to_string_post = !+yojson_of_draft;
  }

let election uuid =
  {
    path = Printf.sprintf "elections/%s/election" (Uuid.to_string uuid);
    of_string = !*Election.t_of_yojson;
    to_string = !+Election.yojson_of_t;
    to_string_post = !+yojson_of_unit;
  }

let election_logo uuid =
  {
    path = Printf.sprintf "elections/%s/logo" (Uuid.to_string uuid);
    of_string = Fun.id;
    to_string = Fun.id;
    to_string_post = !+yojson_of_unit;
  }

let election_trustees uuid (type a b) (w : (a, b) group) =
  let module G = (val w) in
  {
    path = Printf.sprintf "elections/%s/trustees" (Uuid.to_string uuid);
    of_string = !*[%group_of_yojson: _ trustees];
    to_string = !+[%yojson_of_group: _ trustees];
    to_string_post = !+yojson_of_unit;
  }

let election_status uuid =
  {
    path = Printf.sprintf "elections/%s" (Uuid.to_string uuid);
    of_string = !*election_status_of_yojson;
    to_string = !+yojson_of_election_status;
    to_string_post = !+yojson_of_admin_request;
  }

let election_sealing_log uuid =
  {
    path = Printf.sprintf "elections/%s/sealing-log" (Uuid.to_string uuid);
    of_string = Fun.id;
    to_string = Fun.id;
    to_string_post = !+yojson_of_unit;
  }

let election_auto_dates uuid =
  {
    path = Printf.sprintf "elections/%s/automatic-dates" (Uuid.to_string uuid);
    of_string = !*election_auto_dates_of_yojson;
    to_string = !+yojson_of_election_auto_dates;
    to_string_post = !+yojson_of_unit;
  }

let election_voters uuid =
  {
    path = Printf.sprintf "elections/%s/voters" (Uuid.to_string uuid);
    of_string = !*voter_list_of_yojson;
    to_string = !+yojson_of_voter_list;
    to_string_post = !+yojson_of_unit;
  }

let election_records uuid =
  {
    path = Printf.sprintf "elections/%s/records" (Uuid.to_string uuid);
    of_string = !*records_of_yojson;
    to_string = !+yojson_of_records;
    to_string_post = !+yojson_of_unit;
  }

let election_nh_ciphertexts uuid =
  {
    path = Printf.sprintf "elections/%s/nh-ciphertexts" (Uuid.to_string uuid);
    of_string = !*Fun.id;
    to_string = !+Fun.id;
    to_string_post = !+yojson_of_unit;
  }

let election_encrypted_tally uuid =
  {
    path = Printf.sprintf "elections/%s/encrypted-tally" (Uuid.to_string uuid);
    of_string = !*Fun.id;
    to_string = !+Fun.id;
    to_string_post = !+yojson_of_unit;
  }

let election_partial_decryptions uuid =
  {
    path =
      Printf.sprintf "elections/%s/partial-decryptions" (Uuid.to_string uuid);
    of_string = !*partial_decryptions_of_yojson;
    to_string = !+yojson_of_partial_decryptions;
    to_string_post = !+yojson_of_unit;
  }

let election_shuffles uuid =
  {
    path = Printf.sprintf "elections/%s/shuffles" (Uuid.to_string uuid);
    of_string = !*shuffles_of_yojson;
    to_string = !+yojson_of_shuffles;
    to_string_post = !+yojson_of_unit;
  }

let election_shuffle uuid x =
  {
    path =
      Printf.sprintf "elections/%s/shuffles/%s" (Uuid.to_string uuid)
        (Uri.pct_encode x);
    of_string = !*unit_of_yojson;
    to_string = !+yojson_of_unit;
    to_string_post = !+yojson_of_shuffler_request;
  }

let election_roots uuid =
  {
    path = Printf.sprintf "elections/%s/roots" (Uuid.to_string uuid);
    of_string = !*roots_of_yojson;
    to_string = !+yojson_of_roots;
    to_string_post = !+yojson_of_unit;
  }

let election_last_event uuid =
  {
    path = Printf.sprintf "elections/%s/last-event" (Uuid.to_string uuid);
    of_string = !*last_event_of_yojson;
    to_string = !+yojson_of_last_event;
    to_string_post = !+yojson_of_unit;
  }

let election_object uuid x =
  {
    path =
      Printf.sprintf "elections/%s/objects/%s" (Uuid.to_string uuid)
        (Hash.to_hex x);
    of_string = !*Fun.id;
    to_string = !+Fun.id;
    to_string_post = !+yojson_of_unit;
  }

let election_audit_cache uuid =
  {
    path = Printf.sprintf "elections/%s/audit-cache" (Uuid.to_string uuid);
    of_string = !*audit_cache_of_yojson;
    to_string = !+yojson_of_audit_cache;
    to_string_post = !+yojson_of_unit;
  }

let election_ballots uuid =
  {
    path = Printf.sprintf "elections/%s/ballots" (Uuid.to_string uuid);
    of_string = !*ballots_with_weights_of_yojson;
    to_string = !+yojson_of_ballots_with_weights;
    to_string_post = !+Fun.id;
  }

let credentials_server =
  {
    path = "credentials/server";
    of_string = !*unit_of_yojson;
    to_string = !+yojson_of_unit;
    to_string_post = !+yojson_of_credentials_request;
  }

let credentials_credits uuid =
  {
    path = Printf.sprintf "credentials/server/credits/%s" (Uuid.to_string uuid);
    of_string = !*credentials_credits_of_yojson;
    to_string = !+yojson_of_credentials_credits;
    to_string_post = !+yojson_of_unit;
  }
