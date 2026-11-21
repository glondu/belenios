(**************************************************************************)
(*                                BELENIOS                                *)
(*                                                                        *)
(*  Copyright © 2012-2023 Inria                                           *)
(*  Copyright © 2026 VCAST                                                *)
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

open Common_types

type ('a, 'b) public_credential_props = {
  credential : 'a;
  weight : weight option;
  id : 'b option;
}
[@@deriving desc]

type 'a public_credential = ('a, unit) public_credential_props [@@deriving desc]
type 'a public_credentials = 'a public_credential hmap [@@deriving desc]

type 'a public_credential_with_id = ('a, string) public_credential_props
[@@deriving desc]

type 'a public_credentials_with_id = 'a public_credential_with_id hmap
[@@deriving desc]

type private_credentials = string smap [@@deriving desc]
type lang_dir = [ `Ltr | `Rtl ] [@@deriving desc]
type credential_authority = [ `Server | `External of string ] [@@deriving desc]

type template = {
  description : string;
  name : string;
  questions : Question.t array;
  administrator : string;
  credential_authority : credential_authority;
  language : (string * lang_dir) option;
  deny_revote : bool; [@default false]
}
[@@deriving desc]
(** Election template. *)

type 'result election_result = { result : 'result } [@@deriving desc]

type trustee_checksum = { checksum : hash; name : string option }
[@@deriving desc]

type trustee_threshold_set = {
  trustees : trustee_checksum list;
  threshold : int;
}
[@@deriving desc]

type weight_checksums = { total : weight; min : weight; max : weight }
[@@deriving desc]

type election_checksums = {
  election : hash;
  trustees_basic : trustee_checksum list;
  trustees_threshold : trustee_threshold_set list;
  num_voters : int;
  weights : weight_checksums option;
  public_credentials : hash;
  shuffles : trustee_checksum list option;
  encrypted_tally : hash option;
  latest_encrypted_tally : hash option;
  final : hash option;
}
[@@deriving desc]

type audit_cache = {
  voters_hash : hash;
  checksums : election_checksums;
  threshold : int option;
  sealing_log : hash option;
}
[@@deriving desc]

type ballot_dynamic_record = { weight : weight } [@@deriving desc]
type ballot_dynamic_records = ballot_dynamic_record hmap [@@deriving desc]
