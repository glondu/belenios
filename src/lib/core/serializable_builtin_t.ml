(**************************************************************************)
(*                                BELENIOS                                *)
(*                                                                        *)
(*  Copyright © 2012-2021 Inria                                           *)
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

open Common

type 'a shape = 'a Shape.t =
  | SAtomic of 'a
  | SArray of 'a shape array

type question_result =
  | RHomomorphic of Weight.t array
  | RNonHomomorphic of int array array

let json_of_question_result = function
  | RHomomorphic xs ->
     xs
     |> Array.map Weight.unwrap
     |> (fun x -> `List (Array.to_list x))
  | RNonHomomorphic xs ->
     xs
     |> Array.map
          (fun ys ->
            ys
            |> Array.map (fun i -> `Int i)
            |> (fun y -> `List (Array.to_list y))
          )
     |> (fun x -> `List (Array.to_list x))
