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
open Serializable_t
open Common

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

val configuration : ([< nobody | admin ], configuration, unit) t
val account : (admin, api_account, unit) t
val drafts : (admin, summary_list, draft) t
val draft : uuid -> ([< nobody | admin ], draft, draft_request) t
val draft_status : uuid -> (admin, draft_status, unit) t
val draft_voters : uuid -> ([< admin | credauth ], voter_list, voters_request) t
val draft_passwords : uuid -> (admin, string_list, voter_list) t

val draft_public_credentials :
  uuid -> ([< admin | credauth ], public_credentials, public_credentials) t

val draft_private_credentials : uuid -> (admin, private_credentials, unit) t
val draft_credentials_token : uuid -> (admin, string, unit) t
val draft_trustees : uuid -> ([< nobody | admin ], string, trustees_request) t
val draft_trustee : uuid -> string -> (admin, unit, unit) t
val trustee_draft : uuid -> (trustee, string, string) t
val trustee_election : uuid -> (trustee, tally_trustee, string) t
val elections : (admin, summary_list, unit) t
val election : uuid -> ([< nobody | admin ], string, unit) t
val election_archive : uuid -> (nobody, string, unit) t
val election_trustees : uuid -> (nobody, string, unit) t

val election_status :
  uuid -> ([< nobody | admin ], election_status, admin_request) t

val election_auto_dates :
  uuid -> ([< nobody | admin ], election_auto_dates, unit) t

val election_voters : uuid -> (admin, voter_list, unit) t
val election_salt : uuid -> int -> (nobody, string, unit) t
val election_records : uuid -> (admin, records, unit) t
val election_nh_ciphertexts : uuid -> (nobody, string, unit) t
val election_encrypted_tally : uuid -> (nobody, string, unit) t
val election_partial_decryptions : uuid -> (admin, partial_decryptions, unit) t
val election_shuffles : uuid -> (admin, shuffles, unit) t
val election_shuffle : uuid -> string -> (admin, unit, shuffler_request) t
val election_roots : uuid -> (nobody, roots, unit) t
val election_object : uuid -> hash -> (nobody, string, unit) t
val election_audit_cache : uuid -> (nobody, audit_cache, unit) t
