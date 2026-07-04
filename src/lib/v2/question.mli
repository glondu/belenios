open Belenios_core

include
  QUESTION_SIG
    with type question = Belenios_core.Question.t
     and type answer = json
     and type result = json
