(** Compatibility layer with the Helios reference implementation *)

open Serializable_compat_t

val params : 'a params -> 'a Serializable_t.params
val ballot : 'a ballot -> 'a Serializable_t.ballot
val partial_decryption :
  'a partial_decryption -> 'a Serializable_t.partial_decryption

module MakeCompat (G : Signatures.GROUP) : sig
  type election = G.t Signatures.election
  val ballot : election -> G.t Serializable_t.ballot -> G.t ballot
  val partial_decryption : election ->
    G.t Serializable_t.ciphertext array array ->
    G.t Serializable_t.partial_decryption -> G.t partial_decryption
end
