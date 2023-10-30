open Belenios_core
open Signatures
open Belenios_question

module Make (M : RANDOM) (G : GROUP) :
  Question_sigs.QUESTION
    with type elt := G.t
     and type question := NonHomomorphic.question
     and type answer := (G.t, G.Zq.t) NonHomomorphic.answer
     and type result := NonHomomorphic.result
