open Belenios_core
open Belenios_core.Question
open Common_types

type question = Q : ('a question_impl, 'a) generic_question -> question
[@@deriving yojson]

include
  QUESTION_SIG
    with type question := question
     and type answer = json
     and type result = json

val extract : question -> Question.t
val intract : Question.t -> question
