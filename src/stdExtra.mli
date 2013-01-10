val ( |> ) : 'a -> ('a -> 'b) -> 'b
val ( =% ) : Z.t -> Z.t -> bool

val array_forall2 : ('a -> 'b -> bool) -> 'a array -> 'b array -> bool
val array_foralli : (int -> 'a -> bool) -> 'a array -> bool

val hashB : string -> string

val load_from_file : (Yojson.lexer_state -> Lexing.lexbuf -> 'a) -> string -> 'a
val non_empty_lines_of_file : string -> string list Lwt.t
