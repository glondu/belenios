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

module Serializable_t = Serializable_t
module Serializable_j = Serializable_j
open Serializable_t

type draft =
  | Draft : 'a Belenios.Election.version * 'a Serializable_t.draft -> draft

val draft_of_string : string -> draft
val string_of_draft : draft -> string

module Endpoints : sig
  type ('a, 'b) t = {
    path : string;
    of_string : string -> 'a;
    to_string : 'a -> string;
    to_string_post : 'b -> string;
  }

  val configuration : (configuration, unit) t
  val account : (api_account, unit) t
  val drafts : (summary_list, draft) t
  val draft : uuid -> (draft, draft_request) t
  val draft_status : uuid -> (draft_status, unit) t
  val draft_voters : uuid -> (voter_list, voters_request) t
  val draft_passwords : uuid -> (string_list, voter_list) t
  val draft_credentials : uuid -> (unit, public_credentials) t

  val draft_public_credentials :
    uuid -> (public_credentials, public_credentials) t

  val draft_private_credentials : uuid -> (private_credentials, unit) t
  val draft_credentials_token : uuid -> (string, unit) t

  val draft_trustees :
    uuid -> ((Yojson.Safe.t, Yojson.Safe.t) draft_trustees, trustees_request) t

  val draft_trustee : uuid -> string -> (unit, unit) t
  val elections : (summary_list, unit) t
  val election : uuid -> (string, unit) t
  val election_status : uuid -> (election_status, admin_request) t
  val election_auto_dates : uuid -> (election_auto_dates, unit) t
  val election_voters : uuid -> (voter_list, unit) t
  val election_records : uuid -> (records, unit) t
  val election_partial_decryptions : uuid -> (partial_decryptions, unit) t
  val election_shuffles : uuid -> (shuffles, unit) t
  val election_shuffle : uuid -> string -> (unit, shuffler_request) t
end
