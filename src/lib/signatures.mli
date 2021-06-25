(**************************************************************************)
(*                                BELENIOS                                *)
(*                                                                        *)
(*  Copyright © 2012-2021 Inria                                           *)
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

open Belenios_platform
open Platform
open Serializable_builtin_t
open Serializable_t

(** Helpers for interacting with atd stuff *)

type 'a reader = Yojson.Safe.lexer_state -> Lexing.lexbuf -> 'a
type 'a writer = Bi_outbuf.t -> 'a -> unit

module type GROUP = Signatures_core.GROUP

(** A public key with its group *)
module type WRAPPED_PUBKEY = sig
  module G : GROUP
  val y : G.t
end

module type MONAD = Signatures_core.MONAD
module type RANDOM = Signatures_core.RANDOM

(** Election data needed for cryptographic operations. *)
type 'a election = {
  e_params : 'a params;
  (** Parameters of the election. *)

  e_fingerprint : string;
  (** Fingerprint of the election. *)
}

(** Election data bundled with a group. *)
module type ELECTION_DATA = sig
  module G : GROUP
  val election : G.t election
end

type combination_error =
  | MissingPartialDecryption
  | NotEnoughPartialDecryptions
  | UnusedPartialDecryption

(** Cryptographic primitives for an election with homomorphic tally. *)
module type ELECTION = sig

  type 'a m
  (** The type of monadic values. *)

  (** {2 Election parameters} *)

  (** Ballots are encrypted using public-key cryptography secured by
      the discrete logarithm problem. Here, we suppose private keys
      are integers modulo a large prime number. Public keys are
      members of a suitably chosen group. *)

  type elt

  type private_key = Z.t
  type public_key = elt

  (** {2 Ballots} *)

  type plaintext = Serializable_t.plaintext
  (** The plaintext equivalent of [ciphertext], i.e. the contents of a
      ballot. When [x] is such a value, [x.(i).(j)] is the weight (0
      or 1) given to answer [j] in question [i]. *)

  type ballot = elt Serializable_t.ballot
  (** A ballot ready to be transmitted, containing the encrypted
      answers and cryptographic proofs that they satisfy the election
      constraints. *)

  type weighted_ballot = Weight.t * ballot

  val create_ballot : ?sk:private_key -> plaintext -> ballot m
  (** [create_ballot r answers] creates a ballot, or raises
      [Invalid_argument] if [answers] doesn't satisfy the election
      constraints. The private key, if given, will be used to sign
      the ballot. *)

  val check_ballot : ballot -> bool
  (** [check_ballot b] checks all the cryptographic proofs in [b]. All
      ballots produced by [create_ballot] should pass this check. *)

  (** {2 Tally} *)

  val process_ballots : weighted_ballot array -> elt encrypted_tally

  val extract_nh_ciphertexts : elt encrypted_tally -> elt nh_ciphertexts
  val merge_nh_ciphertexts : elt nh_ciphertexts -> elt encrypted_tally -> elt encrypted_tally

  val shuffle_ciphertexts : elt nh_ciphertexts -> elt shuffle m
  val check_shuffle : elt nh_ciphertexts -> elt shuffle -> bool

  (** {2 Partial decryptions} *)

  type factor = elt partial_decryption
  (** A decryption share. It is computed by a trustee from his or her
      private key share and the encrypted tally, and contains a
      cryptographic proof that he or she didn't cheat. *)

  val compute_factor : elt Serializable_t.ciphertext shape -> private_key -> factor m

  val check_factor : elt Serializable_t.ciphertext shape -> public_key -> factor -> bool
  (** [check_factor c pk f] checks that [f], supposedly submitted by a
      trustee whose public_key is [pk], is valid with respect to the
      encrypted tally [c]. *)

  (** {2 Result} *)

  type result = elt Serializable_t.election_result
  (** The election result. It contains the needed data to validate the
      result from the encrypted tally. *)

  val compute_result :
    ?shuffles:elt shuffle list -> ?shufflers:shuffler list ->
    Weight.t -> elt Serializable_t.ciphertext shape -> factor list -> elt trustees ->
    (result, combination_error) Stdlib.result
  (** Combine the encrypted tally and the factors from all trustees to
      produce the election result. The first argument is the number of
      tallied ballots. May raise [Invalid_argument]. *)

  val check_result : elt trustees -> result -> bool
end

module type PKI = sig
  type 'a m
  type private_key
  type public_key
  val genkey : unit -> string m
  val derive_sk : string -> private_key
  val derive_dk : string -> private_key
  val sign : private_key -> string -> signed_msg m
  val verify : public_key -> signed_msg -> bool
  val encrypt : public_key -> string -> string m
  val decrypt : private_key -> string -> string
  val make_cert : sk:private_key -> dk:private_key -> cert m
  val verify_cert : cert -> bool
end

module type CHANNELS = sig
  type 'a m
  type private_key
  type public_key
  val send : private_key -> public_key -> string -> string m
  val recv : private_key -> public_key -> string -> string
end

module type PEDERSEN = sig
  type 'a m
  type elt

  val step1 : unit -> (string * cert) m
  val step1_check : cert -> bool
  val step2 : certs -> unit
  val step3 : certs -> string -> int -> polynomial m
  val step3_check : certs -> int -> polynomial -> bool
  val step4 : certs -> polynomial array -> vinput array
  val step5 : certs -> string -> vinput -> elt voutput m
  val step5_check : certs -> int -> polynomial array -> elt voutput -> bool
  val step6 : certs -> polynomial array -> elt voutput array -> elt threshold_parameters
end

module type MIXNET = sig
  type 'a m
  type elt

  type 'a proof

  val gen_shuffle : elt -> elt ciphertext array -> (elt ciphertext array * number array * int array) m
  val gen_shuffle_proof : elt -> elt ciphertext array -> elt ciphertext array -> number array -> int array -> elt proof m
  val check_shuffle_proof : elt -> elt ciphertext array -> elt ciphertext array -> elt proof -> bool
end
