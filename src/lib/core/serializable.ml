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

(** {1 Serializable datatypes} *)

open Ppx_yojson_conv_lib.Yojson_conv
open Common_types

(** {2 PKI support} *)

type ('a, 'b, 'c) channel_msg = { recipient : 'a; message : 'c }
[@@deriving yojson]

type ('a, 'b, 'c) signed_msg = { message : 'c; signature : 'b proof }
[@@deriving yojson]

type ('a, 'b, 'c) encrypted_msg = {
  algorithm : string;
  alpha : 'a;
  beta : 'a;
  data : string;
}
[@@deriving yojson]

type ('a, 'b, 'c) sent_msg =
  ('a, 'b, ('a, 'b, ('a, 'b, 'c) channel_msg) signed_msg) encrypted_msg
[@@deriving yojson]

(** {2 Trustees} *)

type ('a, 'b) raw_trustee_public_key = { public_key : 'a; name : string }
[@@deriving yojson]

type ('a, 'b) trustee_public_key =
  ('a, 'b, ('a, 'b) raw_trustee_public_key) signed_msg
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

(** {2 Errors} *)

type cast_error =
  [ `DuplicateBallot
  | `ExpiredBallot
  | `InvalidBallot
  | `InvalidCredential
  | `NonCanonical
  | `RevoteNotAllowed
  | `SerializationError of string
  | `UsedCredential
  | `WrongCredential
  | `WrongUsername
  | `WrongWeight
  | `UnauthorizedVoter
  | `ElectionClosed
  | `UnexpectedResponse ]
[@@deriving yojson]

(** {2 Elections} *)

type voter = Common.Voter.t [@@deriving yojson]
type voter_list = voter list [@@deriving yojson]
type public_credentials = string list [@@deriving yojson]
type private_credentials = (string * string) list

let private_credentials_of_yojson : Yojson.Safe.t -> private_credentials =
  function
  | `Assoc o ->
      List.map
        (function
          | k, `String v -> (k, v) | _, x -> of_yojson_error "string expected" x)
        o
  | x -> of_yojson_error "object expected" x

let yojson_of_private_credentials : private_credentials -> Yojson.Safe.t =
 fun x -> `Assoc (List.map (fun (k, v) -> (k, `String v)) x)

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

type sub_batch_item = { base : string; public : string } [@@deriving yojson]
type sub_batch = sub_batch_item list [@@deriving yojson]
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

type ('a, 'b) partial_decryption = {
  decryption_factors : 'a shape;
  decryption_proofs : 'b proof shape;
}
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

type ('a, 'b) shuffle = {
  ciphertexts : 'a nh_ciphertexts;
  proofs : ('a, 'b) shuffle_proofs;
}
[@@deriving yojson]

(** {2 Election result} *)

type 'result election_result = { result : 'result } [@@deriving yojson]

(** {2 Election report} *)

type trustee_checksum = { checksum : hash; name : string } [@@deriving yojson]

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

(** {2 Threshold decryption support} *)

type common_context = { group : string; names : string array; threshold : int }
[@@deriving yojson]

type index = int [@@deriving yojson]

type full_context = { context : common_context; index : index }
[@@deriving yojson]

type ('a, 'b) cert_keys = {
  context : 'b;
  verification : 'a;
  encryption : 'a;
  coefexps : hash;
}
[@@deriving yojson]

type ('a, 'b) cert = ('a, 'b, ('a, index) cert_keys) signed_msg
[@@deriving yojson]

type 'a coefexps = { coefexps : 'a array } [@@deriving yojson]

type ('a, 'b) certs = { context : common_context; certs : ('a, 'b) cert array }
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
  threshold : int;
  certs : ('a, 'b) cert array;
  coefexps : 'a coefexps array;
  signatures : 'b proof array;
  verification_keys : ('a, 'b) threshold_verification_key array;
}
[@@deriving yojson]

type ('a, 'b) trustee_kind =
  [ `Single of ('a, 'b) trustee_public_key
  | `Pedersen of ('a, 'b) threshold_parameters ]
[@@deriving yojson]

type ('a, 'b) trustees = ('a, 'b) trustee_kind list [@@deriving yojson]

(** {2 Condorcet} *)

type condorcet_ballots = int array array [@@deriving yojson]
type condorcet_matrix = int array array [@@deriving yojson]
type condorcet_beatpaths = (int * int) array array [@@deriving yojson]

type schulze_result = {
  valid : int;
  blank : int option; [@yojson.option]
  raw : condorcet_matrix;
  beatpaths : condorcet_beatpaths;
  winners : int list list;
}
[@@deriving yojson]

(** {2 Majority judgment} *)

type mj_ballots = int array array [@@deriving yojson]
type mj_matrix = int array array [@@deriving yojson]

type mj_result = {
  raw : mj_matrix;
  valid : int;
  blank : int option; [@yojson.option]
  invalid : mj_ballots;
  winners : int list list;
}
[@@deriving yojson]

(** {2 Single Transferable Vote} *)

type stv_raw_ballots = int array array [@@deriving yojson]
type stv_processed_ballots = int list list [@@deriving yojson]

type stv_event =
  [ `Win of int list
  | `Lose of int
  | `TieWin of int list
  | `TieLose of int list ]
[@@deriving yojson]

type stv_events = stv_event list [@@deriving yojson]

type stv_result = {
  ballots : stv_processed_ballots;
  invalid : stv_raw_ballots;
  events : stv_events;
  winners : int list;
}
[@@deriving yojson]

(** {2 Event-related types} *)

type location = { offset : int64; length : int64 } [@@deriving yojson]
type archive_header = { version : int; timestamp : json } [@@deriving yojson]

type event_type =
  [ `Setup
  | `Ballot
  | `EndBallots
  | `EncryptedTally
  | `Shuffle
  | `EndShuffles
  | `PartialDecryption
  | `Result ]
[@@deriving yojson]

type setup_data = {
  election : hash;
  trustees : hash;
  credentials : hash;
  credentials_certificate : hash option; [@yojson.option]
}
[@@deriving yojson]

type event = {
  parent : hash option; [@yojson.option]
  height : int;
  typ : event_type; [@key "type"]
  payload : hash option; [@yojson.option]
}
[@@deriving yojson]

type last_event = { height : int; hash : hash; pos : int64 } [@@deriving yojson]

type roots = {
  setup_data : hash option; [@yojson.option]
  last_ballot_event : hash option; [@yojson.option]
  encrypted_tally : hash option; [@yojson.option]
  last_shuffle_event : hash option; [@yojson.option]
  last_pd_event : hash option; [@yojson.option]
  result : hash option; [@yojson.option]
}
[@@deriving yojson]

type 'a owned = { owner : int; payload : 'a } [@@deriving yojson]

(** {2 Sealing} *)

type sealing_config_item = string list [@@deriving yojson]
type sealing_config = (string * sealing_config_item) list

let sealing_config_of_yojson = function
  | `Assoc o -> List.map (fun (k, v) -> (k, sealing_config_item_of_yojson v)) o
  | x -> of_yojson_error "object expected" x

let yojson_of_sealing_config x =
  `Assoc (List.map (fun (k, v) -> (k, yojson_of_sealing_config_item v)) x)

type file_kind = [ `REG | `DIR | `CHR | `BLK | `LNK | `FIFO | `SOCK ]
[@@deriving yojson]

type 'a dir_contents = (string * 'a) list

let dir_contents_of_yojson of_yojson = function
  | `Assoc o -> List.map (fun (k, v) -> (k, of_yojson v)) o
  | x -> of_yojson_error "object expected" x

let yojson_of_dir_contents to_yojson x =
  `Assoc (List.map (fun (k, v) -> (k, to_yojson v)) x)

type 'a file_contents =
  [ `REG of hash | `DIR of 'a dir_contents | `LNK of string ]
[@@deriving yojson]

type stats = {
  dev : int option; [@yojson.option]
  ino : int option; [@yojson.option]
  kind : file_kind option; [@yojson.option]
  perm : int option; [@yojson.option]
  nlink : int option; [@yojson.option]
  uid : int option; [@yojson.option]
  gid : int option; [@yojson.option]
  rdev : int option; [@yojson.option]
  size : int64 option; [@yojson.option]
  atime : float option; [@yojson.option]
  mtime : float option; [@yojson.option]
  ctime : float option; [@yojson.option]
  contents : stats file_contents option; [@yojson.option]
}
[@@deriving yojson]
