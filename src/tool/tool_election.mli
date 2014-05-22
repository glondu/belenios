module type PARAMS = sig
  val election : string
  val get_public_keys : unit -> string array option
  val get_public_creds : unit -> string Stream.t option
  val get_ballots : unit -> string Stream.t option
  val get_result : unit -> string option
  val print_msg : string -> unit
end

module type S = sig
  val vote : string option -> int array array -> string
  val decrypt : string -> string
  val finalize : string array -> string
  val verify : unit -> unit
end

val make : (module PARAMS) -> (module S)
