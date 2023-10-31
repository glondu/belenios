open Belenios_core
open Signatures
open Belenios_question.Homomorphic.Syntax

module Make (M : RANDOM) (G : GROUP) :
  Question_sigs.QUESTION
    with type elt := G.t
     and type question := question
     and type answer := (G.t, G.Zq.t) answer
     and type result := result
