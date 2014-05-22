module type PARAMS = sig
  val uuid : string
  val group : string
  val template : string
  val get_public_keys : unit -> string array option
end

module type S = sig
  val mkelection : unit -> string
end

val make : (module PARAMS) -> (module S)
