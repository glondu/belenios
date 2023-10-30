open Belenios_core
open Signatures
open Belenios_question

module Make (M : RANDOM) (G : GROUP) :
  Question_sigs.QUESTION
    with type elt := G.t
     and type question := Homomorphic.question
     and type answer := (G.t, G.Zq.t) Homomorphic.answer
     and type result := Homomorphic.result
