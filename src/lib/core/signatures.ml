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

open Common_types
open Serializable
open Common
include Signatures_core

module type QUESTION = sig
  type question
  type answer [@@deriving yojson]
  type element
  type result

  val create_answer :
    question -> public_key:element -> prefix:string -> int Shape.t -> answer

  val verify_answer :
    question -> public_key:element -> prefix:string -> answer -> bool

  val extract_ciphertexts : question -> answer -> element ciphertext Shape.t

  val process_ciphertexts :
    question ->
    (Weight.t * element ciphertext Shape.t) list ->
    element ciphertext Shape.t

  val compute_result :
    total_weight:Weight.t -> question -> element Shape.t -> result

  val check_result :
    total_weight:Weight.t -> question -> element Shape.t -> result -> bool
end

module type ELECTION_BASE = sig
  type question [@@deriving yojson]

  val erase_question : question -> question

  module G : GROUP

  val template : question template
  val has_nh_questions : bool
  val version : int
  val uuid : uuid
  val fingerprint : hash
  val public_key : G.t
end

module type ELECTION_BALLOT = sig
  type element
  type ballot [@@deriving yojson]

  val get_credential : ballot -> element option
end

module type ELECTION_RESULT = sig
  type result [@@deriving yojson]

  val of_generic_result : Yojson.Safe.t array -> result
  val to_generic_result : result -> Yojson.Safe.t array
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

  type plaintext = Serializable.plaintext
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

  val compute_factor : element ciphertext shape -> private_key -> factor

  val check_factor : element ciphertext shape -> public_key -> factor -> bool
  (** [check_factor c pk f] checks that [f], supposedly submitted by a trustee
      whose public_key is [pk], is valid with respect to the encrypted tally
      [c]. *)

  (** {2 Result} *)

  type result_type

  type result = result_type Serializable.election_result
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

type 'a exchangeable = {
  dst : string;
  to_yojson : 'a -> Yojson.Safe.t;
  of_yojson : Yojson.Safe.t -> 'a;
}

module type PKI = sig
  module Group : GROUP

  type private_key = Group.Zq.t
  type public_key = Group.t

  val genkey : unit -> string
  val derive_sk : string -> private_key
  val derive_dk : string -> private_key

  val sign :
    'a exchangeable ->
    private_key ->
    'a ->
    (public_key, private_key, 'a) signed_msg

  val verify :
    'a exchangeable ->
    public_key ->
    (public_key, private_key, 'a) signed_msg ->
    bool

  val encrypt :
    'a exchangeable ->
    public_key ->
    'a ->
    (public_key, private_key, 'a) encrypted_msg Lwt.t

  val decrypt :
    'a exchangeable ->
    private_key ->
    (public_key, private_key, 'a) encrypted_msg ->
    'a option Lwt.t
end

module type CHANNELS = sig
  module P : PKI

  type private_key = P.private_key
  type public_key = P.public_key
  type 'a msg = (public_key, private_key, 'a) sent_msg

  val send : 'a exchangeable -> private_key -> public_key -> 'a -> 'a msg Lwt.t
  val recv : 'a exchangeable -> private_key -> public_key -> 'a msg -> 'a Lwt.t
end

module type PEDERSEN = sig
  module Channels : CHANNELS

  type scalar = Channels.P.Group.Zq.t
  type element = Channels.P.Group.t
  type nonrec cert = (element, scalar) cert
  type nonrec certs = (element, scalar) certs
  type nonrec polynomial = (element, scalar) polynomial

  val xch_decryption_key : scalar partial_decryption_key exchangeable
  val step1 : full_context -> string * cert
  val step1_check : full_context -> cert -> bool
  val step2 : certs -> int
  val step3 : certs -> string -> polynomial Lwt.t
  val step3_check : certs -> int -> polynomial -> bool
  val step4 : certs -> polynomial array -> (element, scalar) vinput array

  val step5 :
    certs ->
    string ->
    (element, scalar) vinput ->
    (element, scalar) voutput Lwt.t

  val step5_check :
    certs -> int -> polynomial array -> (element, scalar) voutput -> bool

  val step6 :
    certs ->
    polynomial array ->
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

module type GROUP_SIG = sig
  val of_string : string -> (module GROUP)
end

module type QUESTION_SIG = sig
  type t [@@deriving yojson]

  val is_nh_question : t -> bool
  val get_complexity : t -> complexity

  module Make (G : GROUP) :
    QUESTION
      with type element := G.t
       and type question := t
       and type answer := Yojson.Safe.t
       and type result := Yojson.Safe.t
end

module type MIXNET_SIG = sig
  type question

  module Make (W : ELECTION_DATA with type question := question) :
    MIXNET
      with type element := W.G.t
       and type scalar := W.G.Zq.t
       and type 'a proof := ('a, W.G.Zq.t) Serializable.shuffle_proof
end

module type ELECTION_SIG = sig
  type question

  val get_complexity : question array -> complexity
  val template_of_yojson : Yojson.Safe.t -> question Serializable.template

  val make_raw_election :
    question Serializable.template ->
    uuid:Common.Uuid.t ->
    group:string ->
    public_key:string ->
    Yojson.Safe.t

  module Make (_ : RAW_ELECTION) () : ELECTION with type question := question
end

module type TRUSTEES_SIG = sig
  (** Simple distributed generation of an election public key. *)
  module MakeSimple (G : GROUP) : sig
    (** This module implements a simple distributed key generation. Each share
        is a number modulo q, and the secret key is their sum. All shares are
        needed to decrypt, but the decryptions can be done in a distributed
        fashion. *)

    val generate : unit -> G.Zq.t
    (** [generate ()] generates a new private key. *)

    val prove : name:string -> G.Zq.t -> (G.t, G.Zq.t) trustee_public_key
    (** [prove x] returns the public key associated to [x] and a zero- knowledge
        proof of its knowledge. *)
  end

  exception PedersenFailure of string

  module MakePedersen (C : CHANNELS) : PEDERSEN with module Channels = C

  module MakeCombinator (G : GROUP) : sig
    val check : (G.t, G.Zq.t) trustees -> bool
    (** Check consistency of a set of trustees. *)

    val combine_keys : (G.t, G.Zq.t) trustees -> G.t
    (** Compute the public key associated to a set of trustees. *)

    val combine_factors :
      (G.t, G.Zq.t) trustees ->
      (G.t -> (G.t, G.Zq.t) partial_decryption -> bool) ->
      (G.t, G.Zq.t) partial_decryption owned list ->
      (G.t shape, combination_error) result
    (** Compute synthetic decryption factors. *)
  end
end
