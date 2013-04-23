val ( |> ) : 'a -> ('a -> 'b) -> 'b
val ( =% ) : Z.t -> Z.t -> bool

module List : sig
  include module type of List
  val iteri : (int -> 'a -> 'b list) -> 'a list -> 'b list
end

module Array : sig
  include module type of Array
  val forall : ('a -> bool) -> 'a array -> bool
  val forall2 : ('a -> 'b -> bool) -> 'a array -> 'b array -> bool
  val foralli : (int -> 'a -> bool) -> 'a array -> bool
  val fforall3 : ('a -> 'b -> 'c -> bool) ->
    'a array array -> 'b array array -> 'c array array -> bool
  val map2 : ('a -> 'b -> 'c) -> 'a array -> 'b array -> 'c array
  val map2i : (int -> 'a -> 'b -> 'c) -> 'a array -> 'b array -> 'c array
  val map3 : ('a -> 'b -> 'c -> 'd) ->
    'a array -> 'b array -> 'c array -> 'd array
  val mmap : ('a -> 'b) -> 'a array array -> 'b array array
  val mmap2 : ('a -> 'b -> 'c) ->
    'a array array -> 'b array array -> 'c array array
  val mmap3 : ('a -> 'b -> 'c -> 'd) ->
    'a array array -> 'b array array -> 'c array array -> 'd array array
  val ssplit : ('a * 'b) array array -> 'a array array * 'b array array
end

module String : sig
  include module type of String
  val map : (char -> char) -> string -> string
  val startswith : string -> string -> bool
end

val hashB : string -> string

val load_from_file : (Yojson.lexer_state -> Lexing.lexbuf -> 'a) ->
  string -> 'a
val non_empty_lines_of_file : string -> string list Lwt.t

val random : Z.t -> Z.t
