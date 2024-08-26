open Belenios_core.Signatures

module type QUESTION_KIND = sig
  type question
  type result

  val type_ : string
  val of_concrete : Belenios_question.t -> question option
  val read_result : result reader
  val write_result : result writer

  module Make (_ : RANDOM) (G : GROUP) :
    Belenios_core.Question_sigs.QUESTION
      with type element := G.t
       and type question := question
       and type result := result
end
