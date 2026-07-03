open Belenios_core
include QUESTION_SIG

val to_concrete : t -> Question.t
val of_concrete : Question.t -> t
