open Belenios_core
include QUESTION_SIG

val to_concrete : t -> Question_core.t
val of_concrete : Question_core.t -> t
