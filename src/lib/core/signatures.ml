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
include Signatures_core

module type ELECTION_BASE = sig
  type question

  val read_question : question reader
  val write_question : question writer
  val erase_question : question -> question

  module G : GROUP

  val template : question template
  val has_nh_questions : bool
  val version : int
  val uuid : uuid
  val fingerprint : string
  val public_key : G.t
end

module type ELECTION_BALLOT = sig
  type element
  type ballot

  val read_ballot : ballot reader
  val write_ballot : ballot writer
  val get_credential : ballot -> element option
end

module type ELECTION_RESULT = sig
  type result

  val of_generic_result : string array -> result
  val to_generic_result : result -> string array
  val write_result : result writer
  val read_result : result reader
end

module type ELECTION_DATA = sig
  include ELECTION_BASE
  include ELECTION_BALLOT with type element := G.t
  include ELECTION_RESULT
end

type combination_error =
  | MissingPartialDecryption
  | NotEnoughPartialDecryptions
  | InvalidPartialDecryption

module type RAW_ELECTION = sig
  val raw_election : string
end

type rawballot_check = { rc_credential : string; rc_check : unit -> bool }

(** Cryptographic primitives for an election with homomorphic tally. *)
module type ELECTION_OPS = sig
  (** {2 Election parameters} *)

  (** Ballots are encrypted using public-key cryptography secured by the
      discrete logarithm problem. Here, we suppose private keys are integers
      modulo a large prime number. Public keys are members of a suitably chosen
      group. *)

  type element
  type scalar
  type private_key = scalar
  type public_key = element

  (** {2 Ballots} *)

  type plaintext = Serializable_t.plaintext
  (** The plaintext equivalent of [ciphertext], i.e. the contents of a ballot.
      When [x] is such a value, [x.(i).(j)] is the weight (0 or 1) given to
      answer [j] in question [i]. *)

  type ballot
  (** A ballot ready to be transmitted, containing the encrypted answers and
      cryptographic proofs that they satisfy the election constraints. *)

  type weighted_ballot = Weight.t * ballot

  val create_ballot : sk:private_key -> plaintext -> ballot
  (** [create_ballot r answers] creates a ballot, or raises [Invalid_argument]
      if [answers] doesn't satisfy the election constraints. *)

  val check_ballot : ballot -> bool
  (** [check_ballot b] checks all the cryptographic proofs in [b]. All ballots
      produced by [create_ballot] should pass this check. *)

  val check_rawballot : string -> (rawballot_check, cast_error) Stdlib.result

  (** {2 Tally} *)

  val process_ballots : weighted_ballot list -> element encrypted_tally
  val extract_nh_ciphertexts : element encrypted_tally -> element nh_ciphertexts

  val merge_nh_ciphertexts :
    element nh_ciphertexts -> element encrypted_tally -> element encrypted_tally

  val shuffle_ciphertexts : element nh_ciphertexts -> (element, scalar) shuffle

  val check_shuffle :
    element nh_ciphertexts -> (element, scalar) shuffle -> bool

  (** {2 Partial decryptions} *)

  type factor = (element, scalar) partial_decryption
  (** A decryption share. It is computed by a trustee from his or her private
      key share and the encrypted tally, and contains a cryptographic proof that
      he or she didn't cheat. *)

  val compute_factor :
    element Serializable_t.ciphertext shape -> private_key -> factor

  val check_factor :
    element Serializable_t.ciphertext shape -> public_key -> factor -> bool
  (** [check_factor c pk f] checks that [f], supposedly submitted by a trustee
      whose public_key is [pk], is valid with respect to the encrypted tally
      [c]. *)

  (** {2 Result} *)

  type result_type

  type result = result_type Serializable_t.election_result
  (** The election result. It contains the needed data to validate the result
      from the encrypted tally. *)

  val compute_result :
    element encrypted_tally sized_encrypted_tally ->
    factor owned list ->
    (element, scalar) trustees ->
    (result, combination_error) Stdlib.result
  (** Combine the encrypted tally and the factors from all trustees to produce
      the election result. The first argument is the number of tallied ballots.
      May raise [Invalid_argument]. *)

  val check_result :
    element encrypted_tally sized_encrypted_tally ->
    factor owned list ->
    (element, scalar) trustees ->
    result ->
    bool
end

module type ELECTION = sig
  include ELECTION_DATA

  module E :
    ELECTION_OPS
      with type element = G.t
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
  val encrypt : public_key -> string -> public_key encrypted_msg Lwt.t
  val decrypt : private_key -> public_key encrypted_msg -> string option Lwt.t

  val make_cert :
    sk:private_key -> dk:private_key -> context:context -> private_key cert

  val get_context : private_key cert -> context
  val verify_cert : context -> private_key cert -> bool
end

module type CHANNELS = sig
  type private_key
  type public_key

  val send :
    private_key -> public_key -> string -> public_key encrypted_msg Lwt.t

  val recv :
    private_key -> public_key -> public_key encrypted_msg -> string Lwt.t
end

module type PEDERSEN = sig
  type scalar
  type element

  val step1 : context -> string * scalar cert
  val step1_check : context -> scalar cert -> bool
  val step2 : scalar cert array -> int
  val step3 : scalar cert array -> string -> scalar polynomial Lwt.t
  val step3_check : scalar cert array -> int -> scalar polynomial -> bool

  val step4 :
    scalar cert array -> scalar polynomial array -> scalar vinput array

  val step5 :
    scalar cert array ->
    string ->
    scalar vinput ->
    (element, scalar) voutput Lwt.t

  val step5_check :
    scalar cert array ->
    int ->
    scalar polynomial array ->
    (element, scalar) voutput ->
    bool

  val step6 :
    scalar cert array ->
    scalar polynomial array ->
    (element, scalar) voutput array ->
    (element, scalar) threshold_parameters
end

module type MIXNET = sig
  type element
  type scalar
  type 'a proof

  val gen_shuffle :
    element ->
    element ciphertext array ->
    element ciphertext array * scalar array * int array

  val gen_shuffle_proof :
    element ->
    element ciphertext array ->
    element ciphertext array ->
    scalar array ->
    int array ->
    element proof

  val check_shuffle_proof :
    element ->
    element ciphertext array ->
    element ciphertext array ->
    element proof ->
    bool
end
