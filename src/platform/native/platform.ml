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

let sha256_hex x = Cryptokit.(x |>
  hash_string (Hash.sha256 ()) |>
  transform_string (Hexa.encode ())
)

let sha256_b64 x = Cryptokit.(x |>
  hash_string (Hash.sha256 ()) |>
  transform_string (Base64.encode_compact ())
)

let int_msb i =
  let result = Bytes.create 4 in
  Bytes.set result 0 (char_of_int (i lsr 24));
  Bytes.set result 1 (char_of_int ((i lsr 16) land 0xff));
  Bytes.set result 2 (char_of_int ((i lsr 8) land 0xff));
  Bytes.set result 3 (char_of_int (i land 0xff));
  Bytes.to_string result

let xor a b =
  let n = String.length a in
  assert (n = String.length b);
  String.init n (fun i ->
    char_of_int (int_of_char a.[i] lxor int_of_char b.[i])
  )

let pbkdf2 ~prf ~salt ~iterations ~size password =
  let c = iterations - 1 in
  let hLen = (prf password)#hash_size in
  let result = Bytes.create (hLen * size) in
  let one_iteration i =
    let u = Cryptokit.hash_string (prf password) (salt ^ int_msb i) in
    let rec loop c u accu =
      if c > 0 then
        let u' = Cryptokit.hash_string (prf password) u in
        loop (c-1) u' (xor accu u')
      else accu
    in loop c u u
  in
  for i = 1 to size do
    let offset = (i-1) * hLen in
    String.blit (one_iteration i) 0 result offset hLen;
  done;
  Bytes.to_string result

let pbkdf2_hex ~iterations ~salt x =
  let open Cryptokit in
  let salt = transform_string (Hexa.decode ()) salt in
  pbkdf2 ~prf:MAC.hmac_sha256 ~iterations ~size:1 ~salt x |>
  transform_string (Hexa.encode ())

type rng = Cryptokit.Random.rng
let secure_rng = Cryptokit.Random.secure_rng
let pseudo_rng = Cryptokit.Random.pseudo_rng
let random_string = Cryptokit.Random.string

module Z = struct
  include Z
  let ( =% ) = equal
  let bit_length x = Pervasives.(String.length (to_bits x) * 8)
end
