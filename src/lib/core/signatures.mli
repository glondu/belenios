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
type 'a writer = Buffer.t -> 'a -> unit

module type GROUP = Signatures_core.GROUP
module type MONAD = Signatures_core.MONAD
module type RANDOM = Signatures_core.RANDOM

module type ELECTION_BASE = sig
  module G : GROUP
  val election : params
  val fingerprint : string
  val public_key : G.t

  type ballot
  val string_of_ballot : ballot -> string
  val ballot_of_string : string -> ballot
  val get_credential : ballot -> G.t option
end

module type ELECTION_RESULT = sig
  type result = private raw_result
  val cast_result : raw_result -> result
  val write_result : result writer
  val read_result : result reader
end

module type MAKE_RESULT = functor (X : ELECTION_BASE) -> ELECTION_RESULT

module type ELECTION_DATA = sig
  include ELECTION_BASE
  include ELECTION_RESULT
end

type combination_error =
  | MissingPartialDecryption
  | NotEnoughPartialDecryptions
  | UnusedPartialDecryption

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
  ]

type ballot_id = string

type credential_record = {
    cr_ballot : ballot_id option;
    cr_weight : weight;
}

module type BBOX_OPS = sig
  type 'a m
  type user

  (** Returns [None] if given credential does not exist. *)
  val get_credential_record : string -> credential_record option m

  (** Returns the credential used in previous ballot of given user, if
     any. *)
  val get_user_record : user -> string option m
end

module type BBOX = sig
  type 'a m
  type ballot
  type user

  (** Tries to cast a raw ballot. If possible, returns [Ok
     (credential, parsed_ballot, id_of_ballot_to_be_replaced)]. *)
  val cast :
    ?user:user -> ?weight:weight -> string ->
    (string * ballot * ballot_id option, cast_error) Stdlib.result m
end

(** Cryptographic primitives for an election with homomorphic tally. *)
module type ELECTION_OPS = sig

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

  type ballot
  (** A ballot ready to be transmitted, containing the encrypted
      answers and cryptographic proofs that they satisfy the election
      constraints. *)

  type weighted_ballot = Weight.t * ballot

  val create_ballot : sk:private_key -> plaintext -> ballot m
  (** [create_ballot r answers] creates a ballot, or raises
      [Invalid_argument] if [answers] doesn't satisfy the election
      constraints. *)

  val check_ballot : ballot -> bool
  (** [check_ballot b] checks all the cryptographic proofs in [b]. All
      ballots produced by [create_ballot] should pass this check. *)

  module CastBallot (B : BBOX_OPS with type 'a m := 'a m) : BBOX
         with type 'a m := 'a m
          and type ballot := ballot
          and type user := B.user

  (** {2 Tally} *)

  val process_ballots : weighted_ballot list -> elt encrypted_tally

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

  type result_type
  type result = (result_type, elt encrypted_tally, elt partial_decryption, elt shuffle) Serializable_t.election_result
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

module type ELECTION = sig
  include ELECTION_DATA
  type 'a m
  module E : ELECTION_OPS with type elt = G.t and type 'a m = 'a m and type ballot = ballot and type result_type = result
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
  val encrypt : public_key -> string -> public_key encrypted_msg m
  val decrypt : private_key -> public_key encrypted_msg -> string
  val make_cert : sk:private_key -> dk:private_key -> cert m
  val verify_cert : cert -> bool
end

module type CHANNELS = sig
  type 'a m
  type private_key
  type public_key
  val send : private_key -> public_key -> string -> public_key encrypted_msg m
  val recv : private_key -> public_key -> public_key encrypted_msg -> string
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
