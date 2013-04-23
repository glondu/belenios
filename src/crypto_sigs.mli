(** Signatures of cryptographic primitives *)

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

  val hash : t array -> Z.t
  (** Hash an array of elements into an integer mod [q]. *)
end

(** Parameters for an election. *)
module type ELECTION_PARAMS = sig
  module G : GROUP
  (** The group used for cryptography. *)

  val params : G.t Serializable_t.election
  (** Other parameters. *)

  val fingerprint : string
  (** The election fingerprint. *)
end

(** Cryptographic primives for an election with homomorphic tally. *)
module type HOMOMORPHIC = sig

  (** {2 Election parameters} *)

  (** Ballots are encrypted using public-key cryptography secured by
      the discrete logarithm problem. Here, we suppose private keys
      are integers modulo a large prime number. Public keys are
      members of a suitably chosen group. *)

  type private_key = Z.t
  type public_key

  val election_params : public_key Serializable_t.election

  (** {2 Ciphertexts} *)

  type ciphertext = public_key Serializable_t.ciphertext array array
  (** A ciphertext that can be homomorphically combined. *)

  val combine_ciphertexts : ciphertext -> ciphertext -> ciphertext
  (** Combine two ciphertexts. The encrypted tally of an election is
      the combination of all ciphertexts of valid cast ballots. *)

  (** {2 Ballots} *)

  type plaintext = int array array
  (** The plaintext equivalent of [ciphertext], i.e. the contents of a
      ballot. When [x] is such a value, [x.(i).(j)] is the weight (0
      or 1) given to answer [j] in question [i]. *)

  type ballot = public_key Serializable_t.ballot
  (** A ballot ready to be transmitted, containing the encrypted
      answers and cryptographic proofs that they satisfy the election
      constraints. *)

  type randomness = Z.t array array
  (** Randomness needed to create a ballot. *)

  val create_ballot : randomness -> plaintext -> ballot
  (** [create_ballot answers] creates a ballot, or raises
      [Invalid_argument] if [answers] doesn't satisfy the election
      constraints. *)

  val check_ballot : ballot -> bool
  (** [check_ballot b] checks all the cryptographic proofs in [b]. All
      ballots produced by [create_ballot] should pass this check. *)

  val extract_ciphertext : ballot -> ciphertext
  (** Extract the ciphertext from a ballot. *)

  (** {2 Partial decryptions} *)

  type factor = public_key Serializable_t.partial_decryption
  (** A decryption share. It is computed by a trustee from his or her
      private key share and the encrypted tally, and contains a
      cryptographic proof that he or she didn't cheat. *)

  val compute_factor : randomness -> ciphertext -> private_key -> factor

  val check_factor : ciphertext -> public_key -> factor -> bool
  (** [check_factor c pk f] checks that [f], supposedly submitted by a
      trustee whose public_key is [pk], is valid with respect to the
      encrypted tally [c]. *)

  (** {2 Result} *)

  type result = public_key Serializable_t.result
  (** The election result. It contains the needed data to validate the
      result from the encrypted tally. *)

  val combine_factors : int -> ciphertext -> factor array -> result
  (** Combine the encrypted tally and the factors from all trustees to
      produce the election result. This first argument is the number
      of tallied ballots. *)

  val check_result : result -> bool

  val extract_tally : result -> plaintext
  (** Extract the plaintext result of the election. *)
end
