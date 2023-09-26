open Belenios_core.Common

module type PARAMS = sig
  val version : int
  val uuid : string
  val group : string
end

module type S = sig
  val derive : string -> string
  val generate : Voter.t list -> Belenios_core.Credential.batch
end

module Make (P : PARAMS) (M : Belenios_core.Signatures.RANDOM) () : S
