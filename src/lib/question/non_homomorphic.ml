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

module Syntax = Question_nh_j

type t = Syntax.question
type Types.raw_question += Q of t

let extract = function Q x -> Some x | _ -> None
let type_ = "NonHomomorphic"
let make ~value ~extra = Types.{ type_; value = Q value; extra }

let wrap ~value ~extra =
  let value = Q (value |> Yojson.Safe.to_string |> Syntax.question_of_string) in
  Types.{ type_; value; extra }

let unwrap (q : Types.question) =
  match q.value with
  | Q x ->
      let value = x |> Syntax.string_of_question |> Yojson.Safe.from_string in
      let o = match q.extra with None -> [] | Some x -> [ ("extra", x) ] in
      Some
        (`Assoc (("type", `String "NonHomomorphic") :: ("value", value) :: o))
  | _ -> None
