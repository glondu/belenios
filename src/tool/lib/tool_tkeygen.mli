module type PARAMS = sig
  val group : string
  val version : int
end

module type S = sig
  type 'a m
  type keypair = { id : string; priv : string; pub : string }
  val trustee_keygen : unit -> keypair m
end

module Make (P : PARAMS) (M : Belenios_core.Signatures.RANDOM) () : S with type 'a m := 'a M.t
