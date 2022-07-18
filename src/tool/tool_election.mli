module type PARAMS = sig
  val dir : string
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

module Make (P : PARAMS) () : S with type 'a m := 'a
