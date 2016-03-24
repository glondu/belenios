(**************************************************************************)
(*                                BELENIOS                                *)
(*                                                                        *)
(*  Copyright © 2012-2016 Inria                                           *)
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

open Platform
open Serializable_t

(** Helpers for interacting with atd stuff *)

type 'a reader = Yojson.Safe.lexer_state -> Lexing.lexbuf -> 'a
type 'a writer = Bi_outbuf.t -> 'a -> unit

(** A group suitable for discrete logarithm-based cryptography. *)
module type GROUP = sig
  (** The following interface is redundant: it is assumed, but not
      checked, that usual mathematical relations hold. *)

  type t
  (** The type of elements. Note that it may be larger than the group
      itself, hence the [check] function below. *)

  val check : t -> bool
  (** Check group membership. *)

  val one : t
  (** The neutral element of the group. *)

  val g : t
  (** A generator of the group. *)

  val q : Z.t
  (** The order of [g]. *)

  val ( *~ ) : t -> t -> t
  (** Multiplication. *)

  val ( **~ ) : t -> Z.t -> t
  (** Exponentiation. *)

  val ( =~ ) : t -> t -> bool
  (** Equality test. *)

  val invert : t -> t
  (** Inversion. *)

  val to_string : t -> string
  (** Conversion to string. *)

  val of_string : string -> t
  (** Conversion from string. *)

  val read : t reader
  (** Reading from a stream. *)

  val write : t writer
  (** Writing to a stream. *)

  val hash : string -> t array -> Z.t
  (** Hash an array of elements into an integer mod [q]. The string
      argument is a string that is prepended before computing the hash. *)

  val compare : t -> t -> int
  (** A total ordering over the elements of the group. *)

  type group
  (** Serializable description of the group. *)

  val group : group
  val write_group : group writer

end

(** Monad signature. *)
module type MONAD = sig
  type 'a t
  val return : 'a -> 'a t
  val bind : 'a t -> ('a -> 'b t) -> 'b t
  val fail : exn -> 'a t
end

(** Random number generation. *)
module type RANDOM = sig
  include MONAD

  val random : Z.t -> Z.t t
  (** [random q] returns a random number modulo [q]. *)
end

(** Read operations of a monadic map. *)
module type MONADIC_MAP_RO = sig
  type 'a m
  (** The type of monadic values. *)

  type elt
  (** The type of map values. *)

  type key
  (** The type of map keys. *)

  val fold : (key -> elt -> 'a -> 'a m) -> 'a -> 'a m
  (** [fold f a] computes [(f kN vN ... (f k2 v2 (f k1 v1 a))...)],
      where [k1/v1 ... kN/vN] are all key/value pairs. *)

  val cardinal : int m
  (** Return the number of bindings. *)
end

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

  type t = elt election
  type private_key = Z.t
  type public_key = elt

  (** {2 Ciphertexts} *)

  type ciphertext = elt Serializable_t.ciphertext array array
  (** A ciphertext that can be homomorphically combined. *)

  val neutral_ciphertext : t -> ciphertext
  (** The neutral element for [combine_ciphertext] below. *)

  val combine_ciphertexts : ciphertext -> ciphertext -> ciphertext
  (** Combine two ciphertexts. The encrypted tally of an election is
      the combination of all ciphertexts of valid cast ballots. *)

  (** {2 Ballots} *)

  type plaintext = Serializable_t.plaintext
  (** The plaintext equivalent of [ciphertext], i.e. the contents of a
      ballot. When [x] is such a value, [x.(i).(j)] is the weight (0
      or 1) given to answer [j] in question [i]. *)

  type ballot = elt Serializable_t.ballot
  (** A ballot ready to be transmitted, containing the encrypted
      answers and cryptographic proofs that they satisfy the election
      constraints. *)

  type randomness
  (** Randomness needed to create a ballot. *)

  val make_randomness : t -> randomness m
  (** Creates randomness for [create_ballot] below. The result can be
      kept for Benaloh-style auditing. *)

  val create_ballot : t -> ?sk:private_key ->
    randomness -> plaintext -> ballot m
  (** [create_ballot r answers] creates a ballot, or raises
      [Invalid_argument] if [answers] doesn't satisfy the election
      constraints. The private key, if given, will be used to sign
      the ballot. *)

  val check_ballot : t -> ballot -> bool
  (** [check_ballot b] checks all the cryptographic proofs in [b]. All
      ballots produced by [create_ballot] should pass this check. *)

  val extract_ciphertext : ballot -> ciphertext
  (** Extract the ciphertext from a ballot. *)

  (** {2 Partial decryptions} *)

  type factor = elt partial_decryption
  (** A decryption share. It is computed by a trustee from his or her
      private key share and the encrypted tally, and contains a
      cryptographic proof that he or she didn't cheat. *)

  val compute_factor : ciphertext -> private_key -> factor m

  val check_factor : ciphertext -> public_key -> factor -> bool
  (** [check_factor c pk f] checks that [f], supposedly submitted by a
      trustee whose public_key is [pk], is valid with respect to the
      encrypted tally [c]. *)

  (** {2 Result} *)

  type result = elt Serializable_t.result
  (** The election result. It contains the needed data to validate the
      result from the encrypted tally. *)

  val combine_factors : int -> ciphertext -> factor array -> result
  (** Combine the encrypted tally and the factors from all trustees to
      produce the election result. The first argument is the number of
      tallied ballots. May raise [Invalid_argument]. *)

  val check_result : public_key array -> result -> bool

  val extract_tally : result -> plaintext
  (** Extract the plaintext result of the election. *)
end
