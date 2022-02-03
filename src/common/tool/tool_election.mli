module type PARAMS = sig
  val raw_election : string
  val get_trustees : unit -> string option
  val get_public_creds : unit -> string list option
  val get_ballots : unit -> string list option
  val get_shuffles : unit -> string list option
  val get_result : unit -> string option
  val print_msg : string -> unit
end

module type S = sig
  type 'a m
  val vote : string option -> int array array -> string m
  val decrypt : string -> string m
  val tdecrypt : string -> string -> string m
  val validate : string list -> string m
  val verify : unit -> unit m
  val shuffle_ciphertexts : unit -> string m
  val checksums : unit -> string
  val compute_voters : string list -> string list
end

module Make (P : PARAMS) (R : Belenios_core.Signatures.RANDOM) () : S with type 'a m := 'a R.t
