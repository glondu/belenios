(**************************************************************************)
(*                                BELENIOS                                *)
(*                                                                        *)
(*  Copyright © 2012-2014 Inria                                           *)
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

let sha256_hex x = assert false
let sha256_b64 x = assert false

let b64_encode_compact x = assert false

let derive_cred uuid x = assert false

type rng = unit -> unit

let secure_rng () = assert false
let pseudo_rng x () = assert false
let random_string rng i = assert false

module Z = struct
  type t = unit -> unit
  let zero () = assert false
  let one () = assert false
  let of_int x = assert false
  let of_string x = assert false
  let of_string_base b x = assert false
  let ( + ) x y = assert false
  let ( - ) x y = assert false
  let ( * ) x y = assert false
  let ( mod ) x y = assert false
  let erem x y = assert false
  let to_int x = assert false
  let to_string x = assert false
  let compare x y = assert false
  let ( =% ) x y = assert false
  let geq x y = assert false
  let lt x y = assert false
  let powm x y m = assert false
  let invert x m = assert false
  let probab_prime x n = assert false
  let size x = assert false
  let of_bits x = assert false
end

type datetime
let now () = assert false
let string_of_datetime x = assert false
let datetime_of_string x = assert false
let datetime_compare x y = assert false
let format_datetime fmt x = assert false
