val ( |> ) : 'a -> ('a -> 'b) -> 'b
val ( =% ) : Z.t -> Z.t -> bool

module List : sig
  include module type of List
  val iteri : (int -> 'a -> 'b list) -> 'a list -> 'b list
end

module Array : sig
  include module type of Array
  val forall2 : ('a -> 'b -> bool) -> 'a array -> 'b array -> bool
  val foralli : (int -> 'a -> bool) -> 'a array -> bool
end

module String : sig
  include module type of String
  val map : (char -> char) -> string -> string
  val startswith : string -> string -> bool
end

val hashB : string -> string

val load_from_file : (Yojson.lexer_state -> Lexing.lexbuf -> 'a) -> string -> 'a
val non_empty_lines_of_file : string -> string list Lwt.t
