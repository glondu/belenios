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

open Belenios_platform
open Common_types
open Signatures_core

let dst_prefix = "BELENIOS-V2"
let default_algorithm = "AES-GCM"
let ( >> ) f g x = g (f x)
let ( ^^^ ) a b = a ^ " — " ^ b
let ( let@ ) = ( @@ )
let ( let& ) = Option.bind
let ( // ) = Filename.concat
let datetime_now = datetime_now
let add_days t days = Int64.(add t (mul 86400L (of_int days)))

let finally x cont =
  cont ();
  x

let cast (type t u) (e : (t, u) Type.eq) (x : t) : u = match e with Equal -> x

type 'a smart_ref = { get : unit -> 'a; set : 'a -> unit }

let smart_ref x =
  let r = ref x in
  { get = (fun () -> !r); set = (fun x -> r := x) }

let sha256_hex = Hash.hash_string >> Hash.to_hex
let sha256_b64 = Hash.hash_string >> Hash.to_b64

let encode_data_uri ?charset ~mime_type x =
  let charset =
    match charset with None -> "" | Some x -> Printf.sprintf "charset=%s;" x
  in
  match Base64.encode x with
  | Ok x -> Printf.sprintf "data:%s;%sbase64,%s" mime_type charset x
  | Error (`Msg msg) -> failwith msg

module String = struct
  include String

  let drop_prefix ~prefix x =
    let prefixn = length prefix and n = length x in
    if n >= prefixn && sub x 0 prefixn = prefix then
      Some (sub x prefixn (n - prefixn))
    else None
end

module List = struct
  include List

  let rec join sep = function
    | [] -> []
    | [ x ] -> [ x ]
    | x :: xs -> x :: sep :: join sep xs

  let findi f xs =
    let rec loop i = function
      | [] -> None
      | x :: xs -> (
          match f i x with Some y -> Some y | None -> loop (i + 1) xs)
    in
    loop 0 xs
end

module Option = struct
  include Option

  let wrap f x = try Some (f x) with _ -> None
  let unwrap default x f = match x with None -> default | Some x -> f x
end

let random_modulo q =
  let bits = Z.bit_length q in
  let size = ((bits - 1) / 8) + 1 in
  let mask = Z.(shift_left one bits - one) in
  let rec loop rng =
    let r = Crypto_primitives.random_string rng size in
    let r = Z.(logand (of_bits r) mask) in
    if Z.compare r q < 0 then r else loop rng
  in
  loop

module ZMap = Map.Make (Z)

module MakeField (X : sig
  val q : Z.t
end) =
struct
  type t = Z.t

  let q = X.q
  let of_int x = Z.(erem (of_int x) q)
  let to_int x = Z.to_int x
  let zero = of_int 0
  let one = of_int 1
  let to_Z = Fun.id
  let reduce x = Z.erem x q

  module Expand_message = Crypto_std.Expand_message (Crypto_std.SHA256)

  module Hash_to_field = Crypto_std.Hash_to_field (struct
    let k = 128
    let p = q
    let m = 1
    let expand_message = Expand_message.expand_message_xmd
  end)

  let hash ~dst n msg =
    let dst = dst ^ "-SHA256" in
    let r = Hash_to_field.hash_to_field ~dst msg n in
    Array.map (fun x -> x.(0)) r

  let coerce x =
    if Z.compare zero x <= 0 && Z.compare x q < 0 then x
    else invalid_arg "MakeField().coerce: number not in range"

  let of_string x =
    let x = Z.of_string x in
    if Z.compare zero x <= 0 && Z.compare x q < 0 then x
    else invalid_arg "MakeField().of_string: number not in range"

  let to_string x = Z.to_string x
  let compare = Z.compare
  let double x = Z.(erem (shift_left x 1) q)
  let invert x = Z.invert x q
  let ( + ) x y = Z.(erem (x + y) q)
  let ( - ) x y = Z.(erem (x - y) q)
  let ( * ) x y = Z.(erem (x * y) q)
  let ( =% ) x y = compare x y = 0
  let random = random_modulo q
end

let ( !$ ) of_string =
  Ppx_yojson_conv_lib.Yojson_conv.string_of_yojson >> of_string

let ( !& ) to_string =
  to_string >> Ppx_yojson_conv_lib.Yojson_conv.yojson_of_string

let ( !* ) of_yojson = Json.of_string >> of_yojson
let ( !+ ) to_yojson = to_yojson >> Json.to_string

let b64_order =
  "+/0123456789aAbBcCdDeEfFgGhHiIjJkKlLmMnNoOpPqQrRsStTuUvVwWxXyYzZ"

let compare_b64 a b =
  let na = String.length a and nb = String.length b in
  let value_of c =
    match String.index_opt b64_order c with Some i -> i | None -> -1
  in
  let rec loop i =
    match (i < na, i < nb) with
    | true, true ->
        let diff = value_of a.[i] - value_of b.[i] in
        if diff = 0 then loop (i + 1) else diff
    | true, false -> 1
    | false, true -> -1
    | false, false -> 0
  in
  loop 0

module SSet = Set.Make (String)
module SMap = Map.Make (String)
module IMap = Map.Make (Int)

let check_modulo p x = Z.(compare x zero >= 0 && compare x p < 0)
let b10_digits = "0123456789"

let generate ~length ~digits =
  let radix = String.length digits |> Z.of_int in
  let modulus =
    let rec loop length accu =
      if length > 0 then loop (length - 1) Z.(accu * radix) else accu
    in
    loop length Z.one
  in
  let to_string x =
    let result = Bytes.create length in
    let rec loop i x =
      if i >= 0 then (
        Bytes.set result i digits.[Z.(to_int (x mod radix))];
        loop (i - 1) Z.(x / radix))
      else Bytes.to_string result
    in
    loop (length - 1) x
  in
  fun () -> random_modulo modulus (Crypto_primitives.get_rng ()) |> to_string

let generate_token length = generate ~length ~digits:b58_digits ()
let generate_numeric length = generate ~length ~digits:b10_digits ()

let sqrt s =
  (* https://en.wikipedia.org/wiki/Integer_square_root *)
  let rec loop x0 =
    let x1 = Z.(shift_right (x0 + (s / x0)) 1) in
    if Z.compare x1 x0 < 0 then loop x1 else x0
  in
  let x0 = Z.shift_right s 1 in
  if Z.compare x0 Z.zero > 0 then loop x0 else s

module BabyStepGiantStep (G : GROUP) = struct
  (* https://en.wikipedia.org/wiki/Baby-step_giant-step *)
  let log ~generator:alpha ~max:n =
    let m = Z.(to_int (sqrt n + one)) in
    let table = Hashtbl.create m in
    let add_to_table x i =
      let h = G.hash_to_int x in
      let ii =
        match Hashtbl.find_opt table h with None -> [] | Some ii -> ii
      in
      Hashtbl.add table h (i :: ii)
    in
    let rec populate_table j cur =
      if j < m then (
        add_to_table cur j;
        populate_table (j + 1) G.(cur *~ alpha))
      else cur
    in
    let inv_alpha_m = G.(invert (populate_table 0 one)) in
    fun beta ->
      let rec lookup i gamma =
        if i < m then
          let r =
            let& jj = Hashtbl.find_opt table (G.hash_to_int gamma) in
            let rec find = function
              | [] -> None
              | j :: jj ->
                  let r = G.Zq.((of_int i * of_int m) + of_int j) in
                  if G.(alpha **~ r =~ beta) then Some r else find jj
            in
            find jj
          in
          match r with
          | Some r -> Some r
          | None -> lookup (i + 1) G.(gamma *~ inv_alpha_m)
        else None
      in
      lookup 0 beta
end

let split_on_br s =
  let n = String.length s in
  let rec loop i j accu =
    if j <= n - 4 then
      if String.sub s j 4 = "<br>" then
        loop (j + 4) (j + 4) (String.sub s i (j - i) :: accu)
      else loop i (j + 1) accu
    else List.rev (String.sub s i (n - i) :: accu)
  in
  loop 0 0 []

let rev_split_lines str =
  let n = String.length str in
  let rec loop accu i j =
    if j < n then
      match str.[j] with
      | '\n' | '\r' ->
          let line = String.sub str i (j - i) in
          let accu = if line = "" then accu else line :: accu in
          loop accu (j + 1) (j + 1)
      | _ -> loop accu i (j + 1)
    else
      let line = String.sub str i (j - i) in
      let accu = if line = "" then accu else line :: accu in
      accu
  in
  loop [] 0 0

let split_lines str = List.rev @@ rev_split_lines str

let join_lines xs =
  let b = Buffer.create 1024 in
  List.iter
    (fun x ->
      Buffer.add_string b x;
      Buffer.add_char b '\n')
    xs;
  Buffer.contents b

let re_exec_opt ~rex x = try Some (Re.Pcre.exec ~rex x) with Not_found -> None
let username_rex = "^[A-Z0-9._%+-]+$"

let is_username =
  let rex = Re.Pcre.regexp ~flags:[ `CASELESS ] username_rex in
  fun x -> match re_exec_opt ~rex x with Some _ -> true | None -> false

let email_rex = "^[A-Z0-9._%+-]+@([A-Z0-9.-]+\\.[A-Z]{2,})$"

let is_email =
  let rex = Re.Pcre.regexp ~flags:[ `CASELESS ] email_rex in
  fun ?blacklist x ->
    match re_exec_opt ~rex x with
    | None -> false
    | Some g -> (
        match blacklist with
        | None -> true
        | Some blacklist ->
            let domain = Re.Pcre.get_substring g 1 |> String.lowercase_ascii in
            not (SSet.mem domain blacklist))

let map_and_concat_with_commas f xs =
  let n = Array.length xs in
  let res = Buffer.create (n * 1024) in
  for i = 0 to n - 1 do
    Buffer.add_string res (f xs.(i));
    Buffer.add_char res ','
  done;
  let size = Buffer.length res - 1 in
  if size > 0 then Buffer.sub res 0 size else ""

let remove_special_characters x =
  let n = String.length x in
  let b = Buffer.create n in
  let convert = function
    | ('0' .. '9' | 'A' .. 'Z' | 'a' .. 'z' | '-') as c -> c
    | _ -> '_'
  in
  let rec loop last i =
    if i < n then (
      let c = convert x.[i] in
      if c <> '_' || last <> '_' then Buffer.add_char b c;
      loop c (i + 1))
    else Buffer.contents b
  in
  loop '\000' 0

let uniq_first (type a) ?(compare = Stdlib.compare) xs =
  let module S = Set.Make (struct
    type t = a

    let compare = compare
  end) in
  let rec loop seen accu = function
    | [] -> List.rev accu
    | x :: xs ->
        if S.mem x seen then loop seen accu xs
        else loop (S.add x seen) (x :: accu) xs
  in
  loop S.empty [] xs

module Voter = struct
  type t = voter [@@deriving yojson]

  let get ({ address; login; _ } : t) =
    match (login, address) with
    | None, None -> invalid_arg "Voter.get"
    | Some x, _ -> x
    | _, Some x -> x

  let get_weight ({ weight; _ } : t) = Option.value ~default:Weight.one weight

  let get_recipient ({ address; login; _ } : t) : recipient =
    match (login, address) with
    | None, None -> invalid_arg "Voter.get_recipient"
    | Some name, None -> { name; address = name }
    | None, Some address -> { name = address; address }
    | Some name, Some address -> { name; address }

  let validate ({ address; login; _ } : t) =
    match (address, login) with
    | None, None -> false
    | Some x, None -> is_email x
    | None, Some y -> is_username y
    | Some x, Some y -> is_email x && is_username y

  let int_length n = string_of_int n |> String.length

  let rec find_first n first =
    if int_length first = int_length (first + n) then first
    else find_first n (10 * first)

  let generate n =
    (* choose the first id so that they all have the same length *)
    let first = find_first n 1 in
    let last = first + n - 1 in
    let rec loop last accu =
      if last < first then accu
      else
        let login = Some (string_of_int last) in
        let x : t = { address = None; login; weight = None } in
        loop (last - 1) (x :: accu)
    in
    loop last []

  let has_explicit_weights voters =
    List.exists (fun ({ weight; _ } : t) -> weight <> None) voters
end
