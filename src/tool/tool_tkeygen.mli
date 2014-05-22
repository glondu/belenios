module type PARAMS = sig
  val group : string
end

module type S = sig
  type keypair = { id : string; priv : string; pub : string }
  val trustee_keygen : unit -> keypair
end

val make : (module PARAMS) -> (module S)
