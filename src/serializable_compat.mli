(** Compatibility layer with the Helios reference implementation *)

open Serializable_compat_t

val of_election : 'a election -> 'a Serializable_t.election
val of_ballot : 'a ballot -> 'a Serializable_t.ballot
