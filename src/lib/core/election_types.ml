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

type voter = Common.Voter.t [@@deriving yojson]
type voter_list = voter list [@@deriving yojson]
type public_credentials = string list [@@deriving yojson]
type private_credentials = (string * string) list

let private_credentials_of_yojson : json -> private_credentials = function
  | `Assoc o ->
      List.map
        (function
          | k, `String v -> (k, v) | _, x -> of_yojson_error "string expected" x)
        o
  | x -> of_yojson_error "object expected" x

let yojson_of_private_credentials : private_credentials -> json =
 fun x -> `Assoc (List.map (fun (k, v) -> (k, `String v)) x)

type lang_dir = [ `Ltr | `Rtl ] [@@deriving yojson]

type 'question template = {
  description : string;
  name : string;
  questions : 'question array;
  administrator : string option; [@yojson.option]
  credential_authority : string option; [@yojson.option]
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

type trustee_threshold_checksum = {
  pki_key : hash;
  verification_key : hash;
  name : string;
}
[@@deriving yojson]

type trustee_threshold_set = {
  trustees : trustee_threshold_checksum list;
  threshold : int;
}
[@@deriving yojson]

type weight_checksums = { total : weight; min : weight; max : weight }
[@@deriving yojson]

type election_checksums = {
  election : hash;
  trustees : trustee_checksum list;
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
