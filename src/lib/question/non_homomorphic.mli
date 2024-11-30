module Syntax = Question_nh_j
include Types.QUESTION with type t = Syntax.question

type counting_method =
  [ `None
  | `MajorityJudgment of Question_nh_t.mj_extra
  | `Schulze of Question_nh_t.schulze_extra
  | `STV of Question_nh_t.stv_extra ]

val get_counting_method : Yojson.Safe.t option -> counting_method
