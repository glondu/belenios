module type PARAMS = sig
  val file : string
end

module type S = sig
  type 'a m

  val vote : string option -> int array array -> string m
  val decrypt : int -> string -> (string * string) m
  val tdecrypt : int -> string -> string -> (string * string) m
  val compute_result : unit -> string m
  val verify : unit -> unit m
  val shuffle_ciphertexts : int -> (string * string) m
  val checksums : unit -> string
  val compute_voters : (string * string) list -> string list
  val compute_ballot_summary : unit -> string
  val compute_encrypted_tally : unit -> string * string
end

module Make (P : PARAMS) () : S with type 'a m := 'a
