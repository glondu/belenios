val init : string -> string -> unit Lwt.t

module Gettext : sig
  val s_ : string -> string
  val f_ :  ('a, 'b, 'c, 'c, 'c, 'd) format6 -> ('a, 'b, 'c, 'c, 'c, 'd) format6
end
