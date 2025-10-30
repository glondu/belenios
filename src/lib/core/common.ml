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
open Platform
open Signatures_core

let ( >> ) f g x = g (f x)
let ( ^^^ ) a b = a ^ " — " ^ b
let ( let@ ) = ( @@ )
let ( let& ) = Option.bind
let ( // ) = Filename.concat
let ( ++ ) = Serializable_core_j.identity_of_string
let ( -- ) w x = Serializable_core_j.string_of_identity w x

type (_, _) eq = Refl : ('a, 'a) eq

let cast (type t u) (e : (t, u) eq) (x : t) : u = match e with Refl -> x

module Dummy_random : RANDOM = struct
  let get_rng () = failwith "dummy random not implemented"
end

module Uuid = Common_types.Uuid
module Hash = Common_types.Hash
module Weight = Common_types.Weight
module Array = Common_types.Array
module Shape = Common_types.Shape

let sha256_hex = Digestif.SHA256.(digest_string >> to_hex)
let sha256_b64 = Hash.hash_string >> Hash.to_b64
let b58_digits = Common_types.b58_digits

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
  let reduce_hex x = Z.(erem (of_hex x) q)

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

let sread of_string state buf =
  match Yojson.Safe.read_json state buf with
  | `String x -> of_string x
  | _ -> failwith "read_string"

let swrite to_string buf x = Yojson.Safe.write_json buf (`String (to_string x))

let save_to filename writer x =
  let oc = open_out filename in
  let ob = Bi_outbuf.create_channel_writer oc in
  writer ob x;
  Bi_outbuf.add_char ob '\n';
  Bi_outbuf.flush_channel_writer ob;
  close_out oc

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

module MakeGenerateToken (R : Signatures_core.RANDOM) = struct
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
    fun () -> random_modulo modulus (R.get_rng ()) |> to_string

  let generate_token ?(length = 14) = generate ~length ~digits:b58_digits
  let generate_numeric ?(length = 6) = generate ~length ~digits:b10_digits
end

let rec generate_b58_digit rng =
  let x = Crypto_primitives.random_string rng 1 in
  let x = Char.code x.[0] land 63 in
  if x < 58 then b58_digits.[x] else generate_b58_digit rng

let generate_b58_token ~rng ~length =
  String.init length (fun _ -> generate_b58_digit rng)

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

let parse_public_credential of_string x =
  let open Serializable_core_t in
  match String.split_on_char ',' x with
  | [ c ] -> { credential = of_string c; weight = None; username = None }
  | [ c; w ] ->
      {
        credential = of_string c;
        weight = Some (Weight.of_string w);
        username = None;
      }
  | [ c; ""; u ] ->
      { credential = of_string c; weight = None; username = Some u }
  | [ c; w; u ] ->
      {
        credential = of_string c;
        weight = Some (Weight.of_string w);
        username = Some u;
      }
  | _ -> Printf.ksprintf invalid_arg "invalid line in public credentials: %s" x

let format_public_credential to_string x =
  let open Serializable_core_t in
  match x.weight with
  | None -> to_string x.credential
  | Some w ->
      Printf.sprintf "%s,%s" (to_string x.credential) (Weight.to_string w)

let strip_public_credential =
  parse_public_credential Fun.id >> format_public_credential Fun.id

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

exception Invalid_identity of string

let option_of_string = function "" -> None | x -> Some x
let string_of_option = function None -> "" | Some x -> x

let split_identity_opt x =
  match String.split_on_char ',' x with
  | [ address ] -> (option_of_string address, None, None)
  | [ address; login ] ->
      (option_of_string address, option_of_string login, None)
  | [ address; login; weight ] ->
      ( option_of_string address,
        option_of_string login,
        Some (Weight.of_string weight) )
  | _ -> raise @@ Invalid_identity x

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

module Voter = struct
  type t = [ `Plain | `Json ] * Serializable_core_t.voter

  let wrap = function
    | `String x ->
        let address, login, weight = split_identity_opt x in
        ((`Plain, { address; login; weight }) : t)
    | x ->
        let s = Yojson.Safe.to_string x in
        (`Json, Serializable_core_j.voter_of_string s)

  let of_string x =
    match Serializable_core_j.voter_of_string x with
    | exception _ -> wrap (`String (String.trim x))
    | o -> (`Json, o)

  let to_string ((typ, o) : t) =
    match typ with
    | `Json -> Serializable_core_j.string_of_voter o
    | `Plain -> (
        match o with
        | { address; login = None; weight = None } -> string_of_option address
        | { address; login; weight = None } ->
            Printf.sprintf "%s,%s" (string_of_option address)
              (string_of_option login)
        | { address; login; weight = Some weight } ->
            Printf.sprintf "%s,%s,%s" (string_of_option address)
              (string_of_option login) (Weight.to_string weight))

  let unwrap ((typ, o) as x : t) =
    match typ with
    | `Json ->
        let s = Serializable_core_j.string_of_voter o in
        Yojson.Safe.from_string s
    | `Plain -> `String (to_string x)

  let list_to_string voters =
    if List.exists (fun (x, _) -> x = `Json) voters then
      Serializable_core_j.string_of_voter_list (List.map snd voters)
    else
      let b = Buffer.create 1024 in
      List.iter
        (fun x ->
          Buffer.add_string b (to_string x);
          Buffer.add_char b '\n')
        voters;
      Buffer.contents b

  let list_of_string x =
    match Serializable_core_j.voter_list_of_string x with
    | voters -> List.map (fun x -> (`Json, x)) voters
    | exception _ -> rev_split_lines x |> List.rev_map of_string

  let get ((_, { address; login; _ }) : t) =
    match (login, address) with
    | None, None -> invalid_arg "Voter.get"
    | Some x, _ -> x
    | _, Some x -> x

  let get_weight ((_, { weight; _ }) : t) =
    Option.value ~default:Weight.one weight

  let get_recipient ((_, { address; login; _ }) : t) :
      Serializable_core_t.recipient =
    match (login, address) with
    | None, None -> invalid_arg "Voter.get_recipient"
    | Some name, None -> { name; address = name }
    | None, Some address -> { name = address; address }
    | Some name, Some address -> { name; address }

  let validate ((_, { address; login; _ }) : t) =
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
        let x : t = (`Plain, { address = None; login; weight = None }) in
        loop (last - 1) (x :: accu)
    in
    loop last []

  let has_explicit_weights voters =
    List.exists (fun ((_, { weight; _ }) : t) -> weight <> None) voters
end
