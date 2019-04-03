(**************************************************************************)
(*                                BELENIOS                                *)
(*                                                                        *)
(*  Copyright © 2012-2018 Inria                                           *)
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

open Js_of_ocaml

module Sjcl = struct
  open Js

  type bits

  class type codec =
    object
      method fromBits : bits -> js_string t meth
      method toBits : js_string t -> bits meth
    end

  class type codecs =
    object
      method hex : codec t readonly_prop
      method utf8String : codec t readonly_prop
      method base64 : codec t readonly_prop
    end

  class type hash =
    object
      method hash : js_string t -> bits meth
    end

  class type hashes =
    object
      method sha256 : hash t readonly_prop
    end

  class type cipher =
    object
      method encrypt : bits -> bits meth
    end

  class type ciphers =
    object
      method aes : (bits -> cipher t) constr readonly_prop
    end

  class type mode =
    object
      method encrypt : cipher t -> bits -> bits -> bits meth
      method decrypt : cipher t -> bits -> bits -> bits meth
    end

  class type modes =
    object
      method ccm : mode t readonly_prop
    end

  class type random =
    object
      method randomWords : int -> bits meth
    end

  class type misc =
    object
      method pbkdf2 : js_string t -> bits -> int -> int -> bits meth
    end

  class type sjcl =
    object
      method codec : codecs t readonly_prop
      method hash : hashes t readonly_prop
      method cipher : ciphers t readonly_prop
      method mode : modes t readonly_prop
      method random : random t readonly_prop
      method misc : misc t readonly_prop
    end

  let sjcl : sjcl t = Unsafe.global##.sjcl

  let hex = sjcl##.codec##.hex
  let utf8String = sjcl##.codec##.utf8String
  let base64 = sjcl##.codec##.base64
  let sha256 = sjcl##.hash##.sha256
  let aes = sjcl##.cipher##.aes
  let ccm = sjcl##.mode##.ccm
end

let hex_fromBits x =
  Sjcl.hex##fromBits x |> Js.to_string

let hex_toBits x =
  Sjcl.hex##toBits (Js.string x)

let utf8String_fromBits x =
  Sjcl.utf8String##fromBits x |> Js.to_string

let utf8String_toBits x =
  Sjcl.utf8String##toBits (Js.string x)

let sha256 x =
  Sjcl.sha256##hash (Js.string x)

let sha256_hex x =
  hex_fromBits (sha256 x)

let sha256_b64 x =
  let raw = Sjcl.base64##fromBits (sha256 x) |> Js.to_string in
  match String.index_opt raw '=' with
  | Some i -> String.sub raw 0 i
  | None -> raw

let pbkdf2_generic toBits ~iterations ~salt x =
  let salt = toBits salt in
  let derived = Sjcl.sjcl##.misc##pbkdf2 (Js.string x) salt iterations 256 in
  hex_fromBits derived

let pbkdf2_hex = pbkdf2_generic hex_toBits
let pbkdf2_utf8 = pbkdf2_generic utf8String_toBits

let aes_hex ~key ~data =
  let key = hex_toBits key in
  let data = hex_toBits data in
  let cipher = new%js Sjcl.aes key in
  let output = cipher##encrypt data in
  hex_fromBits output

let encrypt ~key ~iv ~plaintext =
  let key = hex_toBits key in
  let iv = hex_toBits iv in
  let plaintext = utf8String_toBits plaintext in
  let prf = new%js Sjcl.aes key in
  let ciphertext = Sjcl.ccm##encrypt prf plaintext iv in
  hex_fromBits ciphertext

let decrypt ~key ~iv ~ciphertext =
  let key = hex_toBits key in
  let iv = hex_toBits iv in
  let ciphertext = hex_toBits ciphertext in
  let prf = new%js Sjcl.aes key in
  let plaintext = Sjcl.ccm##decrypt prf ciphertext iv in
  utf8String_fromBits plaintext

type rng = unit -> unit

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
  let words = Sjcl.sjcl##.random##randomWords (n/4+1) in
  let hex_words = hex_fromBits words in
  string_of_hex hex_words n

module Jsbn = struct
  open Js

  class type bigint =
    object
      method add : bigint t -> bigint t meth
      method subtract : bigint t -> bigint t meth
      method multiply : bigint t -> bigint t meth
      method divide : bigint t -> bigint t meth
      method _mod : bigint t -> bigint t meth
      method intValue : int meth
      method toString : js_string t meth
      method compareTo : bigint t -> int meth
      method modPow : bigint t -> bigint t -> bigint t meth
      method modInverse : bigint t -> bigint t meth
      method bitLength : int meth
      method isProbablePrime : int -> int meth
      method shiftLeft : int -> bigint t meth
      method shiftRight : int -> bigint t meth
      method _and : bigint t -> bigint t meth
    end

  class type lib =
    object
      method _ZERO : bigint t readonly_prop
      method _ONE : bigint t readonly_prop
    end

  let lib : lib t = Unsafe.global##._BigInteger
  let of_string_base : (js_string t -> int -> bigint t) constr = Unsafe.global##._BigInteger
end

module Z = struct
  type t = Jsbn.bigint Js.t

  let zero = Jsbn.lib##._ZERO
  let one = Jsbn.lib##._ONE

  let of_string_base b x = new%js Jsbn.of_string_base (Js.string x) b
  let of_string x = of_string_base 10 x
  let of_int x = x |> string_of_int |> of_string
  let ( + ) x y = x##add y
  let ( - ) x y = x##subtract y
  let ( * ) x y = x##multiply y
  let ( / ) x y = x##divide y
  let ( mod ) x y = x##_mod y

  let to_int x = x##intValue
  let to_string x = x##toString |> Js.to_string
  let compare x y = x##compareTo y
  let ( =% ) x y = compare x y = 0
  let geq x y = compare x y >= 0
  let lt x y = compare x y < 0
  let powm x y m = x##modPow y m
  let invert x m = x##modInverse m
  let bit_length x = x##bitLength

  let erem x y =
    let r = x mod y in
    if lt r zero then r + y else r

  let probab_prime x n = x##isProbablePrime n

  let z256 = of_int 256

  let of_bits x =
    let n = String.length x in
    let rec loop res i =
      if i >= 0
      then loop (res * z256 + of_int (int_of_char x.[i])) (pred i)
      else res
    in loop zero (pred n)

  let shift_left x n = x##shiftLeft n
  let shift_right x n = x##shiftRight n
  let logand x y = x##_and y
end
