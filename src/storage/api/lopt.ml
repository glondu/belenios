(**************************************************************************)
(*                                BELENIOS                                *)
(*                                                                        *)
(*  Copyright © 2024-2025 Inria                                           *)
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

type 'a t =
  | Value of { value : 'a option; string : string option Lazy.t }
  | String of { string : string option; value : 'a option Lazy.t }

let get_string = function
  | String { string; _ } -> string
  | Value { string; _ } -> Lazy.force string

let get_value = function
  | String { value; _ } -> Lazy.force value
  | Value { value; _ } -> value

let none = Value { value = None; string = Lazy.from_val None }
let none_lwt = Lwt.return none

let some_string of_string x =
  let value = lazy (try Some (of_string x) with _ -> None) in
  String { string = Some x; value }

let some_value to_string x =
  Value { value = Some x; string = lazy (Some (to_string x)) }
