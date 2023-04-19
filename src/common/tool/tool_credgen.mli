open Belenios_core.Serializable_t
open Belenios_core.Common

module type PARAMS = sig
  val version : int
  val uuid : string
  val group : string
end

type credentials = {
  priv : private_credentials;
  public : string list;
  public_with_ids : string list;
}

module type S = sig
  val derive : string -> string
  val generate : Voter.t list -> credentials
end

module Make (P : PARAMS) (M : Belenios_core.Signatures.RANDOM) () : S

val generate_ids : int -> Voter.t list
