(** Signatures *)

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

  val compare : t -> t -> int
  (** A total ordering over the elements of the group. *)
end

(** Monad signature. *)
module type MONAD = sig
  type 'a t
  val return : 'a -> 'a t
  val bind : 'a t -> ('a -> 'b t) -> 'b t
end

(** Random number generation. *)
module type RANDOM = sig
  include MONAD

  val random : Z.t -> Z.t t
  (** [random q] returns a random number modulo [q]. *)
end

(** Ballot box. *)
module type BALLOT_BOX = sig
  include MONAD

  (** {2 Election-specific operations} *)

  type ballot
  (** The type of ballots. The monad is supposed to keep track of all
      cast ballots (e.g. in a database). *)

  val cast : ballot -> unit t
  (** Cast a ballot. *)

  val fold : (ballot -> 'a -> 'a t) -> 'a -> 'a t
  (** [fold f a] computes [(f bN ... (f b2 (f b1 a))...)], where [b1
      ... bN] are all cast ballots. *)
end

(** Parameters for an election. *)
module type ELECTION_PARAMS = sig
  module G : GROUP
  (** The group used for cryptography. *)

  val public_keys : G.t array
  (** Trustee public keys. *)

  val params : G.t Serializable_t.election
  (** Other parameters. *)

  val fingerprint : string
  (** The election fingerprint. *)
end

(** Cryptographic primives for an election with homomorphic tally. *)
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

  val election_params : elt Serializable_t.election

  (** {2 Ciphertexts} *)

  type ciphertext = elt Serializable_t.ciphertext array array
  (** A ciphertext that can be homomorphically combined. *)

  val neutral_ciphertext : ciphertext
  (** The neutral element for [combine_ciphertext] below. *)

  val combine_ciphertexts : ciphertext -> ciphertext -> ciphertext
  (** Combine two ciphertexts. The encrypted tally of an election is
      the combination of all ciphertexts of valid cast ballots. *)

  (** {2 Ballots} *)

  type plaintext = int array array
  (** The plaintext equivalent of [ciphertext], i.e. the contents of a
      ballot. When [x] is such a value, [x.(i).(j)] is the weight (0
      or 1) given to answer [j] in question [i]. *)

  type ballot = elt Serializable_t.ballot
  (** A ballot ready to be transmitted, containing the encrypted
      answers and cryptographic proofs that they satisfy the election
      constraints. *)

  type randomness = Z.t array array
  (** Randomness needed to create a ballot. *)

  val make_randomness : unit -> randomness m
  (** Creates randomness for [create_ballot] below. The result can be
      kept for Benaloh-style auditing. *)

  val create_ballot : randomness -> plaintext -> ballot m
  (** [create_ballot r answers] creates a ballot, or raises
      [Invalid_argument] if [answers] doesn't satisfy the election
      constraints. *)

  val check_ballot : ballot -> bool
  (** [check_ballot b] checks all the cryptographic proofs in [b]. All
      ballots produced by [create_ballot] should pass this check. *)

  val extract_ciphertext : ballot -> ciphertext
  (** Extract the ciphertext from a ballot. *)

  (** {2 Partial decryptions} *)

  type factor = elt Serializable_t.partial_decryption
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

  val check_result : result -> bool

  val extract_tally : result -> plaintext
  (** Extract the plaintext result of the election. *)
end
