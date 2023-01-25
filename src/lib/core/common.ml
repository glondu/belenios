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

open Belenios_platform
open Platform
open Signatures_core

let ( let@ ) f x = f x
let ( let& ) = Option.bind
let ( // ) = Filename.concat

module Uuid = Common_types.Uuid
module Hash = Common_types.Hash
module Weight = Common_types.Weight
module Question_signature = Common_types.Question_signature
module Election_result = Common_types.Election_result
module Question_result = Common_types.Question_result
module Array = Common_types.Array
module Shape = Common_types.Shape

let sha256_b64 x = Hash.hash_string x |> Hash.to_b64

module String = struct
  include String

  let drop_prefix ~prefix x =
    let prefixn = length prefix and n = length x in
    if n >= prefixn && sub x 0 prefixn = prefix then
      Some (sub x prefixn (n - prefixn))
    else
      None
end

module List = struct
  include List

  let rec join sep = function
    | [] -> []
    | [x] -> [x]
    | x :: xs -> x :: sep :: join sep xs
end

module Option = struct
  include Option

  let wrap f x =
    try Some (f x) with _ -> None

  let unwrap default x f =
    match x with
    | None -> default
    | Some x -> f x
end

let sread of_string state buf =
  match Yojson.Safe.read_json state buf with
  | `String x -> of_string x
  | _ -> failwith "read_string"

let swrite to_string buf x =
  Yojson.Safe.write_json buf (`String (to_string x))

let save_to filename writer x =
  let oc = open_out filename in
  let ob = Bi_outbuf.create_channel_writer oc in
  writer ob x;
  Bi_outbuf.add_char ob '\n';
  Bi_outbuf.flush_channel_writer ob;
  close_out oc;;

let b64_order = "+/0123456789aAbBcCdDeEfFgGhHiIjJkKlLmMnNoOpPqQrRsStTuUvVwWxXyYzZ"

let compare_b64 a b =
  let na = String.length a and nb = String.length b in
  let value_of c =
    match String.index_opt b64_order c with
    | Some i -> i
    | None -> -1
  in
  let rec loop i =
    match (i < na), (i < nb) with
    | true, true ->
       let diff = value_of a.[i] - value_of b.[i] in
       if diff = 0 then loop (i+1) else diff
    | true, false -> 1
    | false, true -> -1
    | false, false -> 0
  in loop 0

module SSet = Set.Make(String)
module SMap = Map.Make(String)

module IMap = Map.Make(Int)

(** Direct random monad *)

let bytes_to_sample q =
  (* we take 128 additional bits of random before the mod q, so that
     the statistical distance with a uniform distribution in [0,q[ is
     negligible *)
  Z.bit_length q / 8 + 17

let check_modulo p x = Z.(compare x zero >= 0 && compare x p < 0)

let b58_digits = "123456789ABCDEFGHJKLMNPQRSTUVWXYZabcdefghijkmnopqrstuvwxyz"
let z58 = Z.of_int (String.length b58_digits)
let z10 = Z.of_int 10

module MakeGenerateToken (R : Signatures_core.RANDOM) = struct
  let random_char () =
    R.bind (R.random z58) (fun n -> R.return b58_digits.[Z.to_int n])

  let generate_token ?(length=14) () =
    let res = Bytes.create length in
    let rec loop i =
      if i < length then (
        R.bind (random_char ())
          (fun c ->
            Bytes.set res i c;
            loop (i + 1)
          )
      ) else R.return (Bytes.to_string res)
    in
    loop 0

  let generate_numeric ?(length=6) () =
    let modulus =
      let rec loop length accu =
        if length > 0 then
          loop (length - 1) Z.(accu * z10)
        else accu
      in
      loop length Z.one
    in
    R.bind (R.random modulus) (fun n ->
        R.return (Printf.sprintf "%0*d" length (Z.to_int n))
      )
end

let sqrt s =
  (* https://en.wikipedia.org/wiki/Integer_square_root *)
  let rec loop x0 =
    let x1 = Z.(shift_right (x0 + s / x0) 1) in
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
        match Hashtbl.find_opt table h with
        | None -> []
        | Some ii -> ii
      in
      Hashtbl.add table h (i :: ii)
    in
    let rec populate_table j cur =
      if j < m then (
        add_to_table cur j;
        populate_table (j + 1) G.(cur *~ alpha)
      ) else cur
    in
    let inv_alpha_m = G.(invert (populate_table 0 one)) in
    fun beta ->
    let rec lookup i gamma =
      if i < m then (
        let r =
          let& jj = Hashtbl.find_opt table (G.hash_to_int gamma) in
          let rec find = function
            | [] -> None
            | j :: jj ->
               let r = Z.((of_int i * of_int m + of_int j) mod G.q) in
               if G.(alpha **~ r =~ beta) then
                 Some r
               else find jj
          in
          find jj
        in
        match r with
        | Some r -> Some r
        | None -> lookup (i + 1) G.(gamma *~ inv_alpha_m)
      ) else None
    in
    lookup 0 beta
end

let split_on_br s =
  let n = String.length s in
  let rec loop i j accu =
    if j <= n - 4 then
      if String.sub s j 4 = "<br>" then
        loop (j + 4) (j + 4) (String.sub s i (j - i) :: accu)
      else
        loop i (j + 1) accu
    else
      List.rev (String.sub s i (n - i) :: accu)
  in
  loop 0 0 []

let split_lines str =
  let n = String.length str in
  let find i c =
    match String.index_from_opt str i c with
    | None -> n
    | Some j -> j
  in
  let rec loop accu i =
    if i < n then (
      let j = min (find i '\n') (find i '\r') in
      let line = String.sub str i (j-i) in
      let accu = if line = "" then accu else line :: accu in
      loop accu (j + 1)
    ) else List.rev accu
  in loop [] 0

let strip_cred x =
  match String.split_on_char ',' x with
  | [_] | [_; _] -> x
  | [x; ""; _] -> x
  | [x; y; _] -> Printf.sprintf "%s,%s" x y
  | _ -> Printf.ksprintf invalid_arg "invalid line in public credentials: %s" x

let extract_weight str =
  try
    let i = String.rindex str ',' in
    let w = Weight.of_string (String.sub str (i + 1) (String.length str - i - 1)) in
    String.sub str 0 i, w
  with _ -> str, Weight.one

let split_identity_opt x =
  match String.split_on_char ',' x with
  | [address] -> address, None, None
  | [address; login] -> address, (if login = "" then None else Some login), None
  | [address; login; weight] ->
     address,
     (if login = "" then None else Some login),
     Some (Weight.of_string weight)
  | _ -> failwith "Common.split_identity_opt"

module Voter = struct
  type t = [`Plain | `Json] * Serializable_core_t.voter

  let wrap = function
    | `String x ->
       let address, login, weight = split_identity_opt x in
       ((`Plain, {address; login; weight}) : t)
    | x ->
       let s = Yojson.Safe.to_string x in
       `Json, Serializable_core_j.voter_of_string s

  let of_string x =
    match Serializable_core_j.voter_of_string x with
    | exception _ -> wrap (`String x)
    | o -> `Json, o

  let to_string ((typ, o) : t) =
    match typ with
    | `Json -> Serializable_core_j.string_of_voter o
    | `Plain ->
       match o with
       | {address; login = None; weight = None} -> address
       | {address; login = None; weight = Some weight} -> address ^ ",," ^ Weight.to_string weight
       | {address; login = Some login; weight = None} -> address ^ "," ^ login
       | {address; login = Some login; weight = Some weight} -> address ^ "," ^ login ^ "," ^ Weight.to_string weight

  let unwrap ((typ, o) as x : t) =
    match typ with
    | `Json ->
       let s = Serializable_core_j.string_of_voter o in
       Yojson.Safe.from_string s
    | `Plain -> `String (to_string x)

  let list_to_string voters =
    if List.exists (fun (x, _) -> x = `Json) voters then
      Serializable_core_j.string_of_voter_list (List.map snd voters)
    else (
      let b = Buffer.create 1024 in
      List.iter
        (fun x ->
          Buffer.add_string b (to_string x);
          Buffer.add_char b '\n';
        ) voters;
      Buffer.contents b
    )

  let list_of_string x =
    match Serializable_core_j.voter_list_of_string x with
    | voters -> List.map (fun x -> `Json, x) voters
    | exception _ -> split_lines x |> List.map of_string

  let get ((_, {address; login; weight}) : t) =
    address,
    Option.value login ~default:address,
    Option.value weight ~default:Weight.one
end

let has_explicit_weights voters =
  List.exists (fun ((_, {weight; _}) : Voter.t) -> weight <> None) voters

let supported_crypto_versions = [1]
