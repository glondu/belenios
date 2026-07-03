(**************************************************************************)
(*                                BELENIOS                                *)
(*                                                                        *)
(*  Copyright © 2012-2023 Inria                                           *)
(*                                                                        *)
(*  This program is free software: you can redistribute it and/or modify  *)
(*  it under the terms of the GNU Affero General Public License as        *)
(*  published by the Free Software Foundation, either version 3 of the    *)
(*  License, or (at your option) any later version, with the additional   *)
(*  exemption that compiling, linking, and/or using OpenSSL is allowed.   *)
(*                                                                        *)
(*  This program is distributed in the hope that it will be useful, but   *)
(*  WITHOUT ANY WARRANTY; without even the implied warranty of            *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU     *)
(*  Affero General Public License for more details.                       *)
(*                                                                        *)
(*  You should have received a copy of the GNU Affero General Public      *)
(*  License along with this program.  If not, see                         *)
(*  <http://www.gnu.org/licenses/>.                                       *)
(**************************************************************************)

module Syntax = Question_h

type t = Syntax.question [@@deriving yojson]
type Types.raw_question += Q of t

let type_ = "Homomorphic"
let make ~value ~extra = Types.{ type_; value = Q value; extra }

let erase (q : t) : t =
  {
    answers = Array.map (fun _ -> "") q.answers;
    blank = q.blank;
    min = q.min;
    max = q.max;
    question = "";
  }

let check _ (q : t Types.generic_question) =
  let q = q.value in
  if q.min <= q.max then Ok () else Error `Min_max
