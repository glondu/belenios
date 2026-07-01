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

(** {2 Basic cryptographic datastructures} *)

type 'a ciphertext = { alpha : 'a; beta : 'a } [@@deriving yojson]
(** An ElGamal ciphertext. *)

type 'a proof = { challenge : 'a; response : 'a } [@@deriving yojson]
(** A Fiat-Shamir non-interactive zero-knowledge proof of knowledge (ZKP). *)

type 'a disjunctive_proof = 'a proof array [@@deriving yojson]
(** A disjunctive ZKP. The size of the array is the number of disjuncts. *)

(** {2 PKI support} *)

type ('a, 'b, 'c) channel_msg = { recipient : 'a; message : 'c }
[@@deriving yojson]

type ('a, 'b, 'c) signed_msg = { message : 'c; signature : 'b proof }
[@@deriving yojson]

type ('a, 'b, 'c) encrypted_msg = { alpha : 'a; beta : 'a; data : string }
[@@deriving yojson]

type ('a, 'b, 'c) sent_msg =
  ('a, 'b, ('a, 'b, ('a, 'b, 'c) channel_msg) signed_msg) encrypted_msg
[@@deriving yojson]

(** {2 Trustees} *)

type ('a, 'b) raw_trustee_public_key = {
  public_key : 'a;
  name : string option; [@yojson.option]
}
[@@deriving yojson]

type ('a, 'b) trustee_public_key =
  ('a, 'b, ('a, 'b) raw_trustee_public_key) signed_msg
[@@deriving yojson]

type ('a, 'context) cert_keys = {
  context : 'context;
  verification : 'a;
  encryption : 'a;
}
[@@deriving yojson]

(** {2 Finite fields} *)

type ff_embedding = { padding : int; bits_per_int : int } [@@deriving yojson]

type ff_params = {
  g : number;
  p : number;
  q : number;
  embedding : ff_embedding option; [@yojson.option]
}
[@@deriving yojson]
(** Parameters for a multiplicative subgroup of a finite field. *)

(** {2 Credentials protocol} *)

type ('a, 'b) raw_credentials_certificate = {
  uuid : uuid;
  voter_list_length : int;
  public_creds_hash : hash;
  verification_key : 'a;
  encryption_key : 'a;
}
[@@deriving yojson]

type ('a, 'b) credentials_certificate =
  ('a, 'b, ('a, 'b) raw_credentials_certificate) signed_msg
[@@deriving yojson]

(** {2 Tallying} *)

type ('a, 'b) partial_decryption_factor = { factor : 'a; proof : 'b proof }
[@@deriving yojson]

type ('a, 'b) raw_partial_decryption = ('a, 'b) partial_decryption_factor shape
[@@deriving yojson]

type ('a, 'b) partial_decryption =
  ('a, 'b, ('a, 'b) raw_partial_decryption) signed_msg
[@@deriving yojson]

type plaintext = int shape array [@@deriving yojson]
type 'a encrypted_tally = 'a ciphertext shape [@@deriving yojson]

type 'a sized_encrypted_tally = {
  num_tallied : int;
  total_weight : weight;
  encrypted_tally : 'a;
}
[@@deriving yojson]

(** {2 Mixnets} *)

type 'a nh_ciphertexts = 'a ciphertext array array [@@deriving yojson]

type ('a, 'b) shuffle_proof =
  ('a * 'a * 'a * ('a * 'a) * 'a array)
  * ('b * 'b * 'b * 'b * 'b array * 'b array)
  * 'a array
  * 'a array
[@@deriving yojson]

type ('a, 'b) shuffle_proofs = ('a, 'b) shuffle_proof array [@@deriving yojson]

type ('a, 'b) raw_shuffle = {
  ciphertexts : 'a nh_ciphertexts;
  proofs : ('a, 'b) shuffle_proofs;
}
[@@deriving yojson]

type ('a, 'b) shuffle = ('a, 'b, ('a, 'b) raw_shuffle) signed_msg
[@@deriving yojson]

(** {2 Basic decryption support} *)

type ('a, 'b) basic_cert = ('a, 'b, ('a, unit) cert_keys) signed_msg
[@@deriving yojson]

type ('a, 'b) basic_parameters = {
  cert : ('a, 'b) basic_cert;
  verification_key : ('a, 'b, ('a, 'b) trustee_public_key) signed_msg;
}
[@@deriving yojson]

(** {2 Threshold decryption support} *)

type common_context = {
  algorithm : string;
  group : string;
  names : string array;
  threshold : int;
}
[@@deriving yojson]

type index = int [@@deriving yojson]

type full_context = { context : common_context; index : index }
[@@deriving yojson]

type 'a pedersen_context = { context : 'a; coefexps : hash } [@@deriving yojson]

type ('a, 'b) pedersen_cert =
  ('a, 'b, ('a, index pedersen_context) cert_keys) signed_msg
[@@deriving yojson]

type 'a coefexps = { coefexps : 'a array } [@@deriving yojson]

type ('a, 'b) pedersen_certs = {
  context : common_context;
  certs : ('a, 'b) pedersen_cert array;
}
[@@deriving yojson]

type 'a secret = { secret : 'a } [@@deriving yojson]

type ('a, 'b) polynomial = {
  secrets : ('a, 'b, 'b secret) sent_msg array;
  coefexps : 'a coefexps;
  signature : 'b proof;
}
[@@deriving yojson]

type ('a, 'b) vinput = {
  secrets : ('a, 'b, 'b secret) sent_msg array;
  coefexps : 'a coefexps array;
  signatures : 'b proof array;
}
[@@deriving yojson]

type 'a partial_decryption_key = { decryption_key : 'a } [@@deriving yojson]

type ('a, 'b) sent_partial_decryption_key =
  ('a, 'b, 'b partial_decryption_key) sent_msg
[@@deriving yojson]

type ('a, 'b) threshold_verification_key =
  ('a, 'b, ('a, 'b) trustee_public_key) signed_msg
[@@deriving yojson]

type ('a, 'b) voutput = {
  private_key : ('a, 'b) sent_partial_decryption_key;
  public_key : ('a, 'b) threshold_verification_key;
}
[@@deriving yojson]

type ('a, 'b) threshold_parameters = {
  context : common_context;
  certs : ('a, 'b) pedersen_cert array;
  coefexps : 'a coefexps array;
  signatures : 'b proof array;
  verification_keys : ('a, 'b) threshold_verification_key array;
}
[@@deriving yojson]

(** {2 Trustees} *)

type ('a, 'b) trustee_kind =
  [ `Single of ('a, 'b) basic_parameters
  | `Pedersen of ('a, 'b) threshold_parameters ]
[@@deriving yojson]

type ('a, 'b) trustees = ('a, 'b) trustee_kind list [@@deriving yojson]
type 'a owned = { owner : int; payload : 'a } [@@deriving yojson]
