val ( |> ) : 'a -> ('a -> 'b) -> 'b
val ( =% ) : Z.t -> Z.t -> bool

val array_forall2 : ('a -> 'b -> bool) -> 'a array -> 'b array -> bool
val array_foralli : (int -> 'a -> bool) -> 'a array -> bool
