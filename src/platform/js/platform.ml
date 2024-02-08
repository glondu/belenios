(**************************************************************************)
(*                                BELENIOS                                *)
(*                                                                        *)
(*  Copyright © 2012-2023 Inria                                           *)
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

let belenios = Js.Unsafe.pure_js_expr "belenios"

module Debug = struct
  let debug x = Firebug.console##log (Js.string x)
end

module Sjcl = struct
  open Js

  type bits

  class type codec = object
    method fromBits : bits -> js_string t meth
    method toBits : js_string t -> bits meth
  end

  class type codecs = object
    method hex : codec t readonly_prop
    method utf8String : codec t readonly_prop
    method base64 : codec t readonly_prop
  end

  class type hash = object
    method hash : js_string t -> bits meth
  end

  class type hashes = object
    method sha256 : hash t readonly_prop
  end

  class type cipher = object
    method encrypt : bits -> bits meth
  end

  class type ciphers = object
    method aes : (bits -> cipher t) constr readonly_prop
  end

  class type mode = object
    method encrypt : cipher t -> bits -> bits -> bits meth
    method decrypt : cipher t -> bits -> bits -> bits meth
  end

  class type modes = object
    method ccm : mode t readonly_prop
  end

  class type random = object
    method randomWords : int -> bits meth
  end

  class type misc = object
    method pbkdf2 : js_string t -> bits -> int -> int -> bits meth
  end

  class type sjcl = object
    method codec : codecs t readonly_prop
    method hash : hashes t readonly_prop
    method cipher : ciphers t readonly_prop
    method mode : modes t readonly_prop
    method random : random t readonly_prop
    method misc : misc t readonly_prop
  end

  let sjcl : sjcl t = belenios##.sjcl
  let hex = sjcl##.codec##.hex
  let utf8String = sjcl##.codec##.utf8String
  let sha256 = sjcl##.hash##.sha256
  let aes = sjcl##.cipher##.aes
  let ccm = sjcl##.mode##.ccm
end

module Crypto_primitives = struct
  let hex_fromBits x = Sjcl.hex##fromBits x |> Js.to_string
  let hex_toBits x = Sjcl.hex##toBits (Js.string x)
  let utf8String_fromBits x = Sjcl.utf8String##fromBits x |> Js.to_string
  let utf8String_toBits x = Sjcl.utf8String##toBits (Js.string x)
  let sha256 x = Sjcl.sha256##hash (Js.string x)
  let sha256_hex x = hex_fromBits (sha256 x)

  let pbkdf2_generic toBits ~iterations ~salt ~size x =
    let salt = toBits salt in
    let derived =
      Sjcl.sjcl##.misc##pbkdf2 (Js.string x) salt iterations (256 * size)
    in
    hex_fromBits derived

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

  type rng = unit

  let secure_rng = ()
  let pseudo_rng _ = ()

  let string_of_hex hex n =
    String.init n (fun i ->
        let c = int_of_string ("0x" ^ String.sub hex (2 * i) 2) in
        char_of_int c)

  let random_string () n =
    let words = Sjcl.sjcl##.random##randomWords ((n / 4) + 1) in
    let hex_words = hex_fromBits words in
    string_of_hex hex_words n
end

module BigIntCompat = struct
  open Js

  type bigint

  class type lib = object
    method _ZERO : bigint readonly_prop
    method _ONE : bigint readonly_prop
    method ofInt : int -> bigint meth
    method ofString : js_string t -> bigint meth
    method ofHex : js_string t -> bigint meth
    method add : bigint -> bigint -> bigint meth
    method subtract : bigint -> bigint -> bigint meth
    method multiply : bigint -> bigint -> bigint meth
    method divide : bigint -> bigint -> bigint meth
    method _mod : bigint -> bigint -> bigint meth
    method toInt : bigint -> int meth
    method toString : bigint -> js_string t meth
    method toHex : bigint -> js_string t meth
    method compare : bigint -> bigint -> int meth
    method modPow : bigint -> bigint -> bigint -> bigint meth
    method modInverse : bigint -> bigint -> bigint meth
    method bitLength : bigint -> int meth
    method isProbablePrime : bigint -> int -> int meth
    method shiftLeft : bigint -> int -> bigint meth
    method shiftRight : bigint -> int -> bigint meth
    method _and : bigint -> bigint -> bigint meth
    method _or : bigint -> bigint -> bigint meth
    method xor : bigint -> bigint -> bigint meth
  end

  let lib : lib t = belenios##._BigIntCompat
end

module Z = struct
  open BigIntCompat

  type t = bigint

  let zero = lib##._ZERO
  let one = lib##._ONE
  let of_hex x = lib##ofHex (Js.string x)
  let of_string x = lib##ofString (Js.string x)
  let of_int x = lib##ofInt x
  let ( + ) x y = lib##add x y
  let ( - ) x y = lib##subtract x y
  let ( * ) x y = lib##multiply x y
  let ( / ) x y = lib##divide x y
  let ( mod ) x y = lib##_mod x y
  let to_int x = lib##toInt x
  let to_string x = lib##toString x |> Js.to_string
  let to_hex x = lib##toHex x |> Js.to_string
  let compare x y = lib##compare x y
  let ( =% ) x y = compare x y = 0
  let powm x y m = lib##modPow x y m
  let invert x m = lib##modInverse x m
  let bit_length x = lib##bitLength x

  let erem x y =
    let r = x mod y in
    if compare r zero < 0 then r + y else r

  let probab_prime x n = lib##isProbablePrime x n
  let z256 = of_int 256

  let of_bits x =
    let n = String.length x in
    let rec loop res i =
      if i >= 0 then loop ((res * z256) + of_int (int_of_char x.[i])) (pred i)
      else res
    in
    loop zero (pred n)

  let shift_left x n = lib##shiftLeft x n
  let shift_right x n = lib##shiftRight x n
  let logand x y = lib##_and x y
  let logor x y = lib##_or x y
  let logxor x y = lib##xor x y
  let hash_modulus = of_int 1073741789 (* previous_prime(2^30) *)
  let hash_to_int x = to_int (erem x hash_modulus)
end

class type libsodium = object
  method bytes : unit -> int Js.meth
  method scalarbytes : unit -> int Js.meth
  method is_valid_point_ : int -> int Js.meth
  method scalarmult : int -> int -> int -> int Js.meth
  method add : int -> int -> int -> int Js.meth
  method base : int Js.readonly_prop
  method buffer : Typed_array.uint8Array Js.t Js.readonly_prop
end

let build_libsodium_stubs (libsodium : libsodium Js.t) =
  let module X : Signatures.LIBSODIUM_STUBS = struct
    type scalar = bytes
    type point = bytes

    let bytes () = libsodium##bytes ()
    let scalarbytes () = libsodium##scalarbytes ()
    let base = libsodium##.base
    let buffer = libsodium##.buffer
    let nbytes = bytes ()
    let nscalarbytes = scalarbytes ()

    let copy_to_wasm address x length =
      for i = 0 to length - 1 do
        Typed_array.set buffer (address + i) (int_of_char (Bytes.get x i))
      done

    let copy_from_wasm x address length =
      for i = 0 to length - 1 do
        Bytes.set x i
          (char_of_int (Typed_array.unsafe_get buffer (address + i)))
      done

    let is_valid_point p =
      copy_to_wasm base p nbytes;
      libsodium##is_valid_point_ base

    let reg1 = base + nbytes
    let reg2 = reg1 + nscalarbytes

    let scalarmult q n p =
      copy_to_wasm reg1 n nscalarbytes;
      copy_to_wasm reg2 p nbytes;
      let r = libsodium##scalarmult base reg1 reg2 in
      copy_from_wasm q base nbytes;
      r

    let reg3 = reg1 + nbytes

    let add r p q =
      copy_to_wasm reg1 p nbytes;
      copy_to_wasm reg3 q nbytes;
      let result = libsodium##add base reg1 reg3 in
      copy_from_wasm r base nbytes;
      result
  end in
  (module X : Signatures.LIBSODIUM_STUBS)

let libsodium_ref = ref None

module Libsodium_stubs = struct
  let make () =
    match !libsodium_ref with
    | None ->
        Js.Optdef.iter belenios##.libsodium (fun x ->
            libsodium_ref := Some (build_libsodium_stubs x));
        !libsodium_ref
    | x -> x
end
