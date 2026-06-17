module Syntax = Question_nh
include Types.QUESTION with type t = Syntax.question

type counting_method =
  [ `None
  | `MajorityJudgment of Question_nh.mj_extra
  | `Schulze of Question_nh.schulze_extra
  | `STV of Question_nh.stv_extra ]

val get_counting_method : Yojson.Safe.t option -> counting_method
