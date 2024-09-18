open Belenios

module type PARAMS = sig
  val file : string
end

module type S = sig
  val vote : string option -> int Shape.t array -> string Lwt.t
  val decrypt : int -> string -> (string * string) Lwt.t
  val tdecrypt : int -> string -> string -> (string * string) Lwt.t
  val compute_result : unit -> string Lwt.t
  val verify_ballot : string -> unit Lwt.t
  val verify : ?skip_ballot_check:bool -> unit -> unit Lwt.t
  val shuffle_ciphertexts : int -> (string * string) Lwt.t
  val checksums : unit -> string Lwt.t
  val compute_voters : (string * string) list -> string list Lwt.t
  val compute_ballot_summary : unit -> string Lwt.t
  val compute_encrypted_tally : unit -> (string * string) Lwt.t
end

val make : string -> (module S) Lwt.t
