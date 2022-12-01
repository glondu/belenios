(**************************************************************************)
(*                                BELENIOS                                *)
(*                                                                        *)
(*  Copyright © 2012-2022 Inria                                           *)
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

open Belenios_server.Web_serializable_j

type t =
  [ `Id of int list
  | `User of user
  ]

let wrap_int_list xs =
  List.map (function `Int i -> i | _ -> invalid_arg "User_or_id.wrap") xs

let wrap = function
  | `Int i -> `Id [i]
  | `List xs -> `Id (wrap_int_list xs)
  | `Assoc _ as x -> `User (user_of_string (Yojson.Safe.to_string x))
  | _ -> invalid_arg "User_or_id.wrap"

let unwrap = function
  | `Id i -> `List (List.map (fun i -> `Int i) i)
  | `User u -> Yojson.Safe.from_string (string_of_user u)
