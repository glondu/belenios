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

open CalendarLib
let datetime_format = "%Y-%m-%d %H:%M:%S"

type datetime = Calendar.Precise.t

let now () = Calendar.Precise.now ()

let raw_string_of_datetime n =
  let n = Calendar.Precise.to_gmt n in
  Printer.Precise_Calendar.sprint datetime_format n

let raw_datetime_of_string s =
  match String.index_opt s '.' with
  | None ->
     let l = Printer.Precise_Calendar.from_fstring datetime_format s in
     Calendar.Precise.from_gmt l
  | Some i ->
     let l = Printer.Precise_Calendar.from_fstring datetime_format (String.sub s 0 i) in
     let l = Calendar.Precise.from_gmt l in
     let r = float_of_string ("0" ^ String.sub s i (String.length s - i)) in
     let r = int_of_float (Float.round r) in
     Calendar.Precise.add l (Calendar.Precise.Period.second r)

let datetime_compare = Calendar.Precise.compare

let format_datetime ?(fmt = datetime_format) a =
  Printer.Precise_Calendar.sprint fmt a

let unixfloat_of_datetime a =
  Calendar.Precise.to_unixfloat a |> Float.round

let datetime_of_unixfloat t =
  Calendar.Precise.from_unixfloat t

type period = Calendar.Precise.Period.t
let day = Calendar.Precise.Period.day
let second = Calendar.Precise.Period.second
let datetime_add = Calendar.Precise.add
let datetime_sub = Calendar.Precise.sub
let ymds = Calendar.Precise.Period.ymds

type 'a user_or_id =
  [ `Id of int
  | `User of 'a
  ]
