module type PARAMS = sig
  val group : string
  val version : int
end

module type S = sig
  type keypair = { id : string; priv : string; pub : string }

  val trustee_keygen : unit -> keypair
end

module Make (P : PARAMS) (M : Belenios_core.Signatures.RANDOM) () : S
