(**************************************************************************)
(*                                BELENIOS                                *)
(*                                                                        *)
(*  Copyright © 2024-2024 Inria                                           *)
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

let ( let@ ) f x = f x
let ( let& ) = Option.bind

let make_voters n =
  let length = String.length @@ string_of_int n in
  let rec loop accu n =
    if n > 0 then
      loop (Printf.sprintf "user%0*d@example.org" length n :: accu) (n - 1)
    else accu
  in
  loop [] n

let decode_data_uri x =
  let off = String.index x ',' + 1 in
  match Base64.decode ~off x with Ok x -> x | Error (`Msg x) -> failwith x
