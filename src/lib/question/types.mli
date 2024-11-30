open Belenios_core.Signatures

type raw_question = ..

type 'a generic_question = {
  type_ : string;
  value : 'a;
  extra : Yojson.Safe.t option;
}

type question = raw_question generic_question

module type QUESTION = sig
  type t
  type raw_question += Q of t

  val extract : raw_question -> t option
  val type_ : string
  val make : value:t -> extra:Yojson.Safe.t option -> question
  val wrap : value:Yojson.Safe.t -> extra:Yojson.Safe.t option -> question
  val unwrap : question -> Yojson.Safe.t option
  val erase : t -> t

  val check :
    (module GROUP) Lazy.t ->
    t generic_question ->
    (unit, vector_encoding_error) result
end

module type PACK = sig
  module Ops : QUESTION

  val it : Ops.t
end
