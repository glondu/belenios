open Tool_common

module type PARAMS = sig
  val uuid : string
  val group : string
end

module type S = sig
  val derive : string -> string
  val generate : unit -> string * string * string
end

val make : (module PARAMS) -> (module S)

val cmds : (unit Cmdliner.Term.t * Cmdliner.Term.info) list
