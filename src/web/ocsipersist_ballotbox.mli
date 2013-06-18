(** Ocsipersist-based ballot box *)

module MakeLwtRandom (G : Signatures.GROUP) : sig

  (** {2 Monadic definitions} *)

  include Signatures.MONAD with type 'a t = 'a Lwt.t

  (** {2 Random number generation} *)

  val random : Z.t -> Z.t t
  (** [random q] returns a random number modulo [q]. It uses a secure
      random number generator initialized by a 128-bit seed. *)
end
(** Lwt-compatible random number generation. *)

exception Serialization of exn
exception ProofCheck

module type LWT_ELECTION = Signatures.ELECTION
  with type elt = Z.t
  and type 'a m = 'a Lwt.t

module MakeBallotBox (E : LWT_ELECTION) : sig

  (** {2 Ballot box management} *)

  include Signatures.BALLOT_BOX
  with type 'a m := 'a Lwt.t
  and type ballot = string
  and type record = string * Serializable_builtin_t.datetime
end
(** This ballot box stores ballots and records in Ocsipersist tables. *)
