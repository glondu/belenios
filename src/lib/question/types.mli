open Belenios_core

type raw_question = ..
type 'a generic_question = { type_ : string; value : 'a; extra : json option }
type question = raw_question generic_question

module type QUESTION = sig
  type t
  type raw_question += Q of t

  val extract : raw_question -> t option
  val type_ : string
  val make : value:t -> extra:json option -> question
  val wrap : value:json -> extra:json option -> question
  val unwrap : question -> json option
  val erase : t -> t

  val check :
    (module GROUP) Lazy.t -> t generic_question -> (unit, question_error) result
end

module type PACK = sig
  module Ops : QUESTION

  val it : Ops.t
end
