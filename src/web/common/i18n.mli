module type GETTEXT = sig
  val lang : string
  val s_ : string -> string
  val f_ : ('a, 'b, 'c, 'c, 'c, 'd) format6 -> ('a, 'b, 'c, 'c, 'c, 'd) format6
end

module type S = sig
  val get : component:string -> lang:string -> (module GETTEXT) Lwt.t
end
