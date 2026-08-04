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

open Ppx_yojson_conv_lib.Yojson_conv
open Common_types

type ('a, 'b) public_credential_props = {
  credential : 'a;
  weight : weight option; [@yojson.option]
  id : 'b option; [@yojson.option]
}
[@@deriving yojson]

type 'a public_credential = ('a, unit) public_credential_props
[@@deriving yojson]

type 'a public_credentials = 'a public_credential smap [@@deriving yojson]

type 'a public_credential_with_id = ('a, string) public_credential_props
[@@deriving yojson]

type 'a public_credentials_with_id = 'a public_credential_with_id smap
[@@deriving yojson]

type private_credentials = string smap [@@deriving yojson]
type lang_dir = [ `Ltr | `Rtl ] [@@deriving yojson]

type credential_authority = [ `Server | `External of string ]
[@@deriving yojson]

type template = {
  description : string;
  name : string;
  questions : Question.t array;
  administrator : string;
  credential_authority : credential_authority;
  language : (string * lang_dir) option; [@yojson.option]
}
[@@deriving yojson]
(** Election template. *)

type 'result election_result = { result : 'result } [@@deriving yojson]

type trustee_checksum = {
  checksum : hash;
  name : string option; [@yojson.option]
}
[@@deriving yojson]

type trustee_threshold_set = {
  trustees : trustee_checksum list;
  threshold : int;
}
[@@deriving yojson]

type weight_checksums = { total : weight; min : weight; max : weight }
[@@deriving yojson]

type election_checksums = {
  election : hash;
  trustees_basic : trustee_checksum list;
  trustees_threshold : trustee_threshold_set list;
  num_voters : int;
  weights : weight_checksums option; [@yojson.option]
  public_credentials : hash;
  shuffles : trustee_checksum list option; [@yojson.option]
  encrypted_tally : hash option; [@yojson.option]
  final : hash option; [@yojson.option]
}
[@@deriving yojson]

type audit_cache = {
  voters_hash : hash;
  checksums : election_checksums;
  threshold : int option; [@yojson.option]
  sealing_log : hash option; [@yojson.option]
}
[@@deriving yojson]

type ballot_summary_item = {
  hash : hash;
  weight : weight option; [@yojson.option]
}
[@@deriving yojson]

type ballot_summary = ballot_summary_item list [@@deriving yojson]
