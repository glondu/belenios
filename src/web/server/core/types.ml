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

module Datetime = struct
  open CalendarLib

  let datetime_format = "%Y-%m-%d %H:%M:%S"

  type t = Calendar.Precise.t

  let now () = Calendar.Precise.now ()

  let unwrap n =
    let n = Calendar.Precise.to_gmt n in
    Printer.Precise_Calendar.sprint datetime_format n

  let wrap s =
    match String.index_opt s '.' with
    | None ->
        let l = Printer.Precise_Calendar.from_fstring datetime_format s in
        Calendar.Precise.from_gmt l
    | Some i ->
        let l =
          Printer.Precise_Calendar.from_fstring datetime_format
            (String.sub s 0 i)
        in
        let l = Calendar.Precise.from_gmt l in
        let r = float_of_string ("0" ^ String.sub s i (String.length s - i)) in
        let r = int_of_float (Float.round r) in
        Calendar.Precise.add l (Calendar.Precise.Period.second r)

  let compare = Calendar.Precise.compare
  let format ?(fmt = datetime_format) a = Printer.Precise_Calendar.sprint fmt a
  let to_unixfloat a = Calendar.Precise.to_unixfloat a |> Float.round
  let from_unixfloat t = Calendar.Precise.from_unixfloat t
end

module Period = struct
  open CalendarLib

  type t = Calendar.Precise.Period.t

  let day = Calendar.Precise.Period.day
  let second = Calendar.Precise.Period.second
  let add = Calendar.Precise.add
  let sub = Calendar.Precise.sub
  let ymds = Calendar.Precise.Period.ymds
end

module Lopt = struct
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
end
