open Belenios_core
open Signatures

module Make (M : RANDOM) (G : GROUP) :
  Question_sigs.QUESTION
    with type elt := G.t
     and type question := Question_h_t.question
     and type answer := (G.t, G.Zq.t) Question_h_t.answer
     and type result := Question_h_t.result
