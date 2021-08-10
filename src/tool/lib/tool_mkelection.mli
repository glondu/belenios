module type PARAMS = sig
  val version : int
  val uuid : string
  val group : string
  val template : string
  val get_trustees : unit -> string
end

module type S = sig
  val mkelection : unit -> string
end

val make : (module PARAMS) -> (module S)
