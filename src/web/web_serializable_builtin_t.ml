(**************************************************************************)
(*                                BELENIOS                                *)
(*                                                                        *)
(*  Copyright © 2012-2016 Inria                                           *)
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

type datetime = Fcalendar.Precise.t * string option

let now () = CalendarLib.Fcalendar.Precise.now (), None

let raw_string_of_datetime (n, s) =
  match s with
  | Some s -> s
  | None ->
    let n = Fcalendar.Precise.to_gmt n in
    let a = Printer.Precise_Fcalendar.sprint datetime_format n in
    let ts = Printf.sprintf "%.6f" (Fcalendar.Precise.to_unixfloat n) in
    let i = String.index ts '.' in
    let b = String.sub ts i (String.length ts - i) in
    a ^ b

let raw_datetime_of_string s =
  let i = String.index s '.' in
  let l = Printer.Precise_Fcalendar.from_fstring datetime_format (String.sub s 0 i) in
  let l = Fcalendar.Precise.from_gmt l in
  let r = float_of_string ("0" ^ String.sub s i (String.length s-i)) in
  (Fcalendar.Precise.add l (Fcalendar.Precise.Period.second r), Some s)

let datetime_compare (a, _) (b, _) =
  CalendarLib.Fcalendar.Precise.compare a b

let format_datetime fmt (a, _) =
  CalendarLib.Printer.Precise_Fcalendar.sprint fmt a
