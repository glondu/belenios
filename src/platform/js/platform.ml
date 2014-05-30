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
  open Js.Unsafe
  type t = any

  let lib = variable "BigInteger"
  let zero = get lib "ZERO"
  let one = get lib "ONE"

  let of_string_base b x = new_obj lib
    [|
      x |> Js.string |> inject;
      b |> float_of_int |> Js.number_of_float |> inject;
    |]

  let of_string x = of_string_base 10 x
  let of_int x = x |> string_of_int |> of_string
  let ( + ) x y = meth_call x "add" [| y |]
  let ( - ) x y = meth_call x "subtract" [| y |]
  let ( * ) x y = meth_call x "multiply" [| y |]
  let ( mod ) x y = meth_call x "mod" [| y |]

  let to_int x = meth_call x "intValue" [| |]
  let to_string x = meth_call x "toString" [| |] |> Js.to_string
  let compare x y = meth_call x "compareTo" [| y |]
  let ( =% ) x y = compare x y = 0
  let geq x y = compare x y >= 0
  let lt x y = compare x y < 0
  let powm x y m = meth_call x "modPow" [| y; m |]
  let invert x m = meth_call x "modInverse" [| m |]
  let bit_length x = meth_call x "bitLength" [| |]

  let erem x y =
    let r = x mod y in
    if lt r zero then r + y else r

  let probab_prime x n =
    meth_call x "isProbablePrime" [| |] |>
    Js.float_of_number |> int_of_float

  let z256 = of_int 256

  let of_bits x =
    let n = String.length x in
    let rec loop res i =
      if i >= 0
      then loop (res * z256 + of_int (int_of_char x.[i])) (pred i)
      else res
    in loop zero (pred n)
end

type datetime
let now () = assert false
let string_of_datetime x = assert false
let datetime_of_string x = assert false
let datetime_compare x y = assert false
let format_datetime fmt x = assert false
