module type PARAMS = sig
  val uuid : string
  val group : string
end

module type S = sig
  val derive : string -> string
  val generate : string list -> string list * string list
end

val make : (module PARAMS) -> (module S)

val generate_ids : int -> string list
