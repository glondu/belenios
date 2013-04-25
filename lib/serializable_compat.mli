(** Compatibility layer with the Helios reference implementation *)

open Serializable_compat_t

val election : 'a election -> 'a Serializable_t.election
val ballot : 'a ballot -> 'a Serializable_t.ballot
val partial_decryption :
  'a partial_decryption -> 'a Serializable_t.partial_decryption
val result : 'a result -> 'a Serializable_t.result

module type COMPAT = sig
  type t
  val ballot : t Serializable_t.ballot -> t ballot
  val partial_decryption : t Serializable_t.ciphertext array array ->
    t Serializable_t.partial_decryption -> t partial_decryption
end

module MakeCompat (P : Signatures.ELECTION_PARAMS) :
  COMPAT with type t = P.G.t
