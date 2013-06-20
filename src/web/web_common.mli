open Serializable_t

type user_type = Dummy | CAS

val string_of_user_type : user_type -> string

type user = {
  user_name : string;
  user_type : user_type;
}

type acl =
  | Any
  | Restricted of (user -> bool Lwt.t)

type election_data = {
  fn_election : string;
  fingerprint : string;
  election : ff_pubkey election;
  fn_public_keys : string;
  author : user;
  featured_p : bool;
  can_read : acl;
  can_vote : acl;
  can_admin : acl;
}

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

module type WEB_BBOX = sig
  include Signatures.BALLOT_BOX
  with type 'a m := 'a Lwt.t
  and type ballot = string
  and type record = string * Serializable_builtin_t.datetime
end

module MakeBallotBox (E : LWT_ELECTION) : WEB_BBOX

module type WEB_ELECTION = sig
  module G : Signatures.GROUP
  module E : LWT_ELECTION
  module B : WEB_BBOX
  val data : election_data
end
