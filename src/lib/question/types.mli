type raw_question = ..

type question = {
  type_ : string;
  value : raw_question;
  extra : Yojson.Safe.t option;
}

module type QUESTION = sig
  type t
  type raw_question += Q of t

  val extract : raw_question -> t option
  val type_ : string
  val make : value:t -> extra:Yojson.Safe.t option -> question
  val wrap : value:Yojson.Safe.t -> extra:Yojson.Safe.t option -> question
  val unwrap : question -> Yojson.Safe.t option
end

module type PACK = sig
  module Ops : QUESTION

  val it : Ops.t
end
