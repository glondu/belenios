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
open Serializable_t
open Extra

type abstract

type _ u =
  | State : election_state u
  | State_state : state_state u
  | Dates : election_dates u
  | Metadata : metadata u
  | Private_key : Yojson.Safe.t u
  | Private_keys : string list u
  | Audit_cache : audit_cache u
  | Archive_header : archive_header u
  | Last_event : last_event u
  | Draft : draft_election u
  | Public_creds : public_credentials u
  | Private_creds : private_credentials u
  | Sealing_log : string u
  | Passwords : string list list u
  | Records : election_records u
  | Voters : Voter.t list u
  | Confidential_archive : abstract u
  | Extended_record : string -> extended_record u
  | Credential_mapping : string -> credential_mapping u
  | Data : hash -> string u
  | Roots : roots u
  | Voters_config : voters_config u
  | Voter : string -> Voter.t u
  | Credential_weight : string -> Weight.t u
  | Credential_user : string -> string u
  | Password : string -> password_record u

type kind = Username of string | Address of string

type _ t =
  | Account : int -> account t
  | Election : uuid * 'a u -> 'a t
  | Auth_db : string -> string list t
  | Admin_password : string * kind -> password_record t
