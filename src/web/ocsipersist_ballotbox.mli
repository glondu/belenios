(** Ocsipersist-based ballot box *)

module MakeBallotBox (G : Signatures.GROUP)
  (U : sig val uuid : Uuidm.t end) : sig

  (** {2 Monadic definitions} *)

  include Signatures.MONAD with type 'a t = 'a Lwt.t

  (** {2 Random number generation} *)

  val random : Z.t -> Z.t t
  (** [random q] returns a random number modulo [q]. It uses a secure
      random number generator initialized by a 128-bit seed. *)

  (** {2 Ballot box management} *)

  include Signatures.BALLOT_BOX
  with type 'a m := 'a t
  and type ballot = string
  and type record = string * Serializable_builtin_t.datetime
end
(** This ballot box stores ballots and records in Ocsipersist tables. *)
