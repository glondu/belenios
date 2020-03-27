module type PARAMS = sig
  val election : string
  val get_trustees : unit -> string option
  val get_public_creds : unit -> string Stream.t option
  val get_ballots : unit -> string Stream.t option
  val get_shuffles : unit -> string Stream.t option
  val get_result : unit -> string option
  val print_msg : string -> unit
end

module type S = sig
  val vote : string option -> int array array -> string
  val decrypt : string -> string
  val tdecrypt : string -> string -> string
  val validate : string list -> string
  val verify : unit -> unit
  val shuffle_ciphertexts : unit -> string
  val checksums : unit -> string
  val compute_voters : string list -> string list
end

val make : (module PARAMS) -> (module S)
