(**************************************************************************)
(*                                BELENIOS                                *)
(*                                                                        *)
(*  Copyright © 2012-2022 Inria                                           *)
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

(** Signatures *)

open Serializable_t
open Common
include module type of Signatures_core

module type ELECTION_BASE = sig
  module G : GROUP

  val template : template
  val version : int
  val uuid : uuid
  val fingerprint : string
  val public_key : G.t

  type ballot

  val string_of_ballot : ballot -> string
  val ballot_of_string : string -> ballot
  val get_credential : ballot -> G.t option
end

module type ELECTION_RESULT = sig
  type result

  val of_generic_result : Question_sigs.generic_result array -> result
  val to_generic_result : result -> Question_sigs.generic_result array
  val write_result : result writer
  val read_result : result reader
end

module type ELECTION_DATA = sig
  include ELECTION_BASE
  include ELECTION_RESULT
end

type combination_error =
  | MissingPartialDecryption
  | NotEnoughPartialDecryptions
  | InvalidPartialDecryption

module type RAW_ELECTION = sig
  val raw_election : string
end

type cast_error =
  [ `SerializationError of exn
  | `NonCanonical
  | `InvalidBallot
  | `InvalidCredential
  | `WrongCredential
  | `WrongWeight
  | `UsedCredential
  | `RevoteNotAllowed
  | `DuplicateBallot
  | `ExpiredBallot
  | `WrongUsername ]

type rawballot_check = { rc_credential : string; rc_check : unit -> bool }

(** Cryptographic primitives for an election with homomorphic tally. *)
module type ELECTION_OPS = sig
  (** {2 Election parameters} *)

  (** Ballots are encrypted using public-key cryptography secured by
      the discrete logarithm problem. Here, we suppose private keys
      are integers modulo a large prime number. Public keys are
      members of a suitably chosen group. *)

  type elt
  type scalar
  type private_key = scalar
  type public_key = elt

  (** {2 Ballots} *)

  type plaintext = Serializable_t.plaintext
  (** The plaintext equivalent of [ciphertext], i.e. the contents of a
      ballot. When [x] is such a value, [x.(i).(j)] is the weight (0
      or 1) given to answer [j] in question [i]. *)

  type ballot
  (** A ballot ready to be transmitted, containing the encrypted
      answers and cryptographic proofs that they satisfy the election
      constraints. *)

  type weighted_ballot = Weight.t * ballot

  val create_ballot : sk:private_key -> plaintext -> ballot
  (** [create_ballot r answers] creates a ballot, or raises
      [Invalid_argument] if [answers] doesn't satisfy the election
      constraints. *)

  val check_ballot : ballot -> bool
  (** [check_ballot b] checks all the cryptographic proofs in [b]. All
      ballots produced by [create_ballot] should pass this check. *)

  val check_rawballot : string -> (rawballot_check, cast_error) Stdlib.result

  (** {2 Tally} *)

  val process_ballots : weighted_ballot list -> elt encrypted_tally
  val extract_nh_ciphertexts : elt encrypted_tally -> elt nh_ciphertexts

  val merge_nh_ciphertexts :
    elt nh_ciphertexts -> elt encrypted_tally -> elt encrypted_tally

  val shuffle_ciphertexts : elt nh_ciphertexts -> (elt, scalar) shuffle
  val check_shuffle : elt nh_ciphertexts -> (elt, scalar) shuffle -> bool

  (** {2 Partial decryptions} *)

  type factor = (elt, scalar) partial_decryption
  (** A decryption share. It is computed by a trustee from his or her
      private key share and the encrypted tally, and contains a
      cryptographic proof that he or she didn't cheat. *)

  val compute_factor :
    elt Serializable_t.ciphertext shape -> private_key -> factor

  val check_factor :
    elt Serializable_t.ciphertext shape -> public_key -> factor -> bool
  (** [check_factor c pk f] checks that [f], supposedly submitted by a
      trustee whose public_key is [pk], is valid with respect to the
      encrypted tally [c]. *)

  (** {2 Result} *)

  type result_type

  type result = result_type Serializable_t.election_result
  (** The election result. It contains the needed data to validate the
      result from the encrypted tally. *)

  val compute_result :
    elt encrypted_tally sized_encrypted_tally ->
    factor owned list ->
    (elt, scalar) trustees ->
    (result, combination_error) Stdlib.result
  (** Combine the encrypted tally and the factors from all trustees to
      produce the election result. The first argument is the number of
      tallied ballots. May raise [Invalid_argument]. *)

  val check_result :
    elt encrypted_tally sized_encrypted_tally ->
    factor owned list ->
    (elt, scalar) trustees ->
    result ->
    bool
end

module type ELECTION = sig
  include ELECTION_DATA

  module E :
    ELECTION_OPS
      with type elt = G.t
       and type scalar = G.Zq.t
       and type ballot = ballot
       and type result_type = result
end

module type PKI = sig
  type private_key
  type public_key

  val genkey : unit -> string
  val derive_sk : string -> private_key
  val derive_dk : string -> private_key
  val sign : private_key -> string -> private_key signed_msg
  val verify : public_key -> private_key signed_msg -> bool
  val encrypt : public_key -> string -> public_key encrypted_msg
  val decrypt : private_key -> public_key encrypted_msg -> string
  val make_cert : sk:private_key -> dk:private_key -> private_key cert
  val verify_cert : private_key cert -> bool
end

module type CHANNELS = sig
  type private_key
  type public_key

  val send : private_key -> public_key -> string -> public_key encrypted_msg
  val recv : private_key -> public_key -> public_key encrypted_msg -> string
end

module type PEDERSEN = sig
  type scalar
  type elt

  val step1 : unit -> string * scalar cert
  val step1_check : scalar cert -> bool
  val step2 : scalar certs -> unit
  val step3 : scalar certs -> string -> int -> scalar polynomial
  val step3_check : scalar certs -> int -> scalar polynomial -> bool
  val step4 : scalar certs -> scalar polynomial array -> scalar vinput array
  val step5 : scalar certs -> string -> scalar vinput -> (elt, scalar) voutput

  val step5_check :
    scalar certs ->
    int ->
    scalar polynomial array ->
    (elt, scalar) voutput ->
    bool

  val step6 :
    scalar certs ->
    scalar polynomial array ->
    (elt, scalar) voutput array ->
    (elt, scalar) threshold_parameters
end

module type MIXNET = sig
  type elt
  type scalar
  type 'a proof

  val gen_shuffle :
    elt ->
    elt ciphertext array ->
    elt ciphertext array * scalar array * int array

  val gen_shuffle_proof :
    elt ->
    elt ciphertext array ->
    elt ciphertext array ->
    scalar array ->
    int array ->
    elt proof

  val check_shuffle_proof :
    elt -> elt ciphertext array -> elt ciphertext array -> elt proof -> bool
end
