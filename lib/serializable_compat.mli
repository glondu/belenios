(** Compatibility layer with the Helios reference implementation *)

open Serializable_compat_t

val of_election : 'a election -> 'a Serializable_t.election
val of_ballot : 'a ballot -> 'a Serializable_t.ballot

module type COMPAT = sig
  type t
  val to_ballot : t Serializable_t.ballot -> t ballot
end

module MakeCompat (P : Crypto_sigs.ELECTION_PARAMS) :
  COMPAT with type t = P.G.t
