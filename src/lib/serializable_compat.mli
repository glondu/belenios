(** Compatibility layer with the Helios reference implementation *)

open Serializable_compat_t

val election : 'a election -> 'a Serializable_t.election
val ballot : 'a ballot -> 'a Serializable_t.ballot
val partial_decryption :
  'a partial_decryption -> 'a Serializable_t.partial_decryption

module MakeCompat (P : Signatures.ELECTION_PARAMS) : sig
  val ballot : P.G.t Serializable_t.ballot -> P.G.t ballot
  val partial_decryption : P.G.t Serializable_t.ciphertext array array ->
    P.G.t Serializable_t.partial_decryption -> P.G.t partial_decryption
end
