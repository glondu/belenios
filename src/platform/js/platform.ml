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

let sjcl = Js.Unsafe.variable "sjcl"

let sha256 x =
  Js.Unsafe.meth_call sjcl "hash.sha256.hash"
    [| Js.string x |> Js.Unsafe.inject |]

let sha256_hex x =
  Js.Unsafe.meth_call sjcl "codec.hex.fromBits"
    [| sha256 x |] |> Js.to_string

let sha256_b64 x =
  let raw =
    Js.Unsafe.meth_call sjcl "codec.base64.fromBits"
      [| sha256 x |] |> Js.to_string
  in
  match (try Some (String.index raw '=') with Not_found -> None) with
  | Some i -> String.sub raw 0 i
  | None -> raw

let pbkdf2_hex ~iterations ~salt x =
  let salt = Js.Unsafe.meth_call sjcl "codec.hex.toBits"
    [| Js.string salt |> Js.Unsafe.inject |]
  in
  let derived = Js.Unsafe.meth_call sjcl "misc.pbkdf2"
    [|
      Js.string x |> Js.Unsafe.inject;
      salt;
      Js.Unsafe.inject iterations;
      Js.Unsafe.inject 256;
    |]
  in
  Js.Unsafe.meth_call sjcl "codec.hex.fromBits"
    [| derived |] |> Js.to_string

type rng = unit -> unit

let sjcl_random = Js.Unsafe.get sjcl "random"

(* PRNG is initialized in random.js *)
let secure_rng () = ()
let pseudo_rng _ () = ()

let string_of_hex hex n =
  String.init n (fun i ->
    let c = int_of_string ("0x" ^ String.sub hex (2*i) 2) in
    char_of_int c
  )

let random_string rng n =
  let () = rng () in
  let words = Js.Unsafe.meth_call sjcl_random "randomWords"
    [| n/4+1 |> float_of_int |> Js.number_of_float |> Js.Unsafe.inject |]
  in
  let hex_words = Js.Unsafe.meth_call sjcl "codec.hex.fromBits"
    [| words |] |> Js.to_string
  in
  string_of_hex hex_words n

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
    meth_call x "isProbablePrime" [| n |> float_of_int |> Js.number_of_float |> inject |] |>
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
