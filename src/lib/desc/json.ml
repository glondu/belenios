(**************************************************************************)
(*                                BELENIOS                                *)
(*                                                                        *)
(*  Copyright © 2026-2026 Inria                                           *)
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

open Core

(* Encoding *)

let int64_to_json x =
  if
    Int64.compare x (Int64.of_int min_int) >= 0
    && Int64.compare x (Int64.of_int max_int) <= 0
  then `Int (Int64.to_int x)
  else `Intlit (Int64.to_string x)

let rec to_json : type a. a t -> a -> json =
 fun d x ->
  match d with
  | Unit -> `Null
  | Bool -> `Bool x
  | Int -> `Int x
  | Int64 -> int64_to_json x
  | String -> `String x
  | Json -> x
  | List d -> `List (List.map (to_json d) x)
  | Option d -> ( match x with None -> `Null | Some y -> to_json d y)
  | Tuple { elts; _ } -> `List (elts_to_json elts x)
  | Assoc { value; sorted } ->
      let x =
        if sorted then List.sort (fun (a, _) (b, _) -> String.compare a b) x
        else x
      in
      `Assoc (List.map (fun (k, v) -> (k, to_json value v)) x)
  | Map { base; prj; _ } -> to_json base (prj x)
  | Record { fields; _ } -> `Assoc (fields_to_json fields x)
  | Variant { classify; _ } -> (
      let (Value (c, y)) = classify x in
      match c.arg with
      | No_arg -> `String c.tag
      | Arg d -> `Assoc [ (c.tag, to_json d y) ])
  | Untagged { classify; _ } -> (
      let (Value (c, y)) = classify x in
      match c.arg with
      | Arg d -> to_json d y
      | No_arg -> assert false (* rejected by Desc.Core.untagged *))
  | Custom { to_json; _ } -> to_json x
  | Rec { body; _ } -> to_json (Lazy.force body) x

and elts_to_json : type r f. (r, f) elts -> r -> json list =
 fun es r ->
  match es with
  | Nil -> []
  | Cons ({ ty; get }, rest) -> to_json ty (get r) :: elts_to_json rest r

and fields_to_json : type r f. (r, f) fields -> r -> (string * json) list =
 fun fs r ->
  match fs with
  | Fnil -> []
  | Fcons ({ name; kind; get }, rest) -> (
      let rest = fields_to_json rest r in
      match kind with
      | Required ty -> (name, to_json ty (get r)) :: rest
      | Optional ty -> (
          match get r with
          | None -> rest
          | Some v -> (name, to_json ty v) :: rest)
      | Default { ty; default; equal } ->
          let v = get r in
          if equal v default then rest else (name, to_json ty v) :: rest)

(* Decoding *)

exception Error of string list * string

let error_to_string (path, msg) =
  match path with
  | [] -> msg
  | _ -> Printf.sprintf "%s: %s" (String.concat "." path) msg

(* [path] is reversed while decoding. *)
let fail path fmt =
  Printf.ksprintf (fun s -> raise (Error (List.rev path, s))) fmt

(* The wildcard is for the non-standard constructors of Yojson 2. *)
let[@warning "-11"] kind_of_json : json -> kind option = function
  | `Null -> Some Knull
  | `Bool _ -> Some Kbool
  | `Int _ | `Intlit _ -> Some Kint
  | `Float _ -> Some Kfloat
  | `String _ -> Some Kstring
  | `List _ -> Some Klist
  | `Assoc _ -> Some Kobject
  | _ -> None

let kind_name = function
  | None -> "an unsupported value"
  | Some Knull -> "null"
  | Some Kbool -> "boolean"
  | Some Kint -> "integer"
  | Some Kfloat -> "number"
  | Some Kstring -> "string"
  | Some Klist -> "array"
  | Some Kobject -> "object"

let check_no_duplicates path kvs =
  let tbl = Hashtbl.create 16 in
  List.iter
    (fun (k, _) ->
      if Hashtbl.mem tbl k then fail path "duplicate key %S" k
      else Hashtbl.add tbl k ())
    kvs

let protect path f x =
  try f x
  with Failure msg | Invalid_argument msg -> fail path "invalid value: %s" msg

let rec of_json_aux : type a. string list -> a t -> json -> a =
 fun path d j ->
  let expected what =
    fail path "expected %s, got %s" what (kind_name (kind_of_json j))
  in
  match (d, j) with
  | Unit, `Null -> ()
  | Unit, _ -> expected "null"
  | Bool, `Bool b -> b
  | Bool, _ -> expected "a boolean"
  | Int, `Int i -> i
  | Int, `Intlit s -> (
      match int_of_string_opt s with
      | Some x -> x
      | None -> fail path "integer out of range")
  | Int, _ -> expected "an integer"
  | Int64, `Int i -> Int64.of_int i
  | Int64, `Intlit s -> (
      match Int64.of_string_opt s with
      | Some x -> x
      | None -> fail path "integer out of range")
  | Int64, _ -> expected "an integer"
  | String, `String s -> s
  | String, _ -> expected "a string"
  | Json, j -> j
  | List d, `List l ->
      List.mapi (fun i j -> of_json_aux (string_of_int i :: path) d j) l
  | List _, _ -> expected "an array"
  | Option _, `Null -> None
  | Option d, j -> Some (of_json_aux path d j)
  | Tuple { elts; make }, `List l -> elts_of_json path 0 elts make l
  | Tuple _, _ -> expected "an array"
  | Assoc { value; _ }, `Assoc kvs ->
      check_no_duplicates path kvs;
      List.map (fun (k, j) -> (k, of_json_aux (k :: path) value j)) kvs
  | Assoc _, _ -> expected "an object"
  | Map { base; inj; _ }, j -> protect path inj (of_json_aux path base j)
  | Record { fields; make }, `Assoc kvs ->
      check_no_duplicates path kvs;
      fields_of_json path fields make kvs
  | Record _, _ -> expected "an object"
  | Variant { cases; _ }, (`String tag | `Assoc [ (tag, _) ]) -> (
      match List.find_opt (fun (Any c) -> c.tag = tag) cases with
      | None -> fail path "unknown constructor %S" tag
      | Some (Any c) -> (
          match (c.arg, j) with
          | No_arg, `String _ -> c.inj ()
          | Arg d, `Assoc [ (_, a) ] -> c.inj (of_json_aux (tag :: path) d a)
          | No_arg, _ -> fail path "constructor %S takes no argument" tag
          | Arg _, _ -> fail path "constructor %S takes an argument" tag))
  | Variant _, _ -> expected "a string or an object with one field"
  | Untagged { cases; _ }, j -> (
      let k = kind_of_json j in
      let accepts (Any c) =
        match c.arg with
        | No_arg -> false
        | Arg d -> (
            match (kinds d, k) with
            | None, _ -> true
            | Some ks, Some k -> List.mem k ks
            | Some _, None -> false)
      in
      match List.find_opt accepts cases with
      | Some (Any ({ arg = Arg d; _ } as c)) -> c.inj (of_json_aux path d j)
      | _ -> fail path "unexpected %s" (kind_name k))
  | Custom { of_json; _ }, j -> protect path of_json j
  | Rec { body; _ }, j -> of_json_aux path (Lazy.force body) j

and elts_of_json : type r f.
    string list -> int -> (r, f) elts -> f -> json list -> r =
 fun path i es make l ->
  match (es, l) with
  | Nil, [] -> make
  | Cons ({ ty; _ }, rest), j :: l ->
      elts_of_json path (i + 1) rest
        (make (of_json_aux (string_of_int i :: path) ty j))
        l
  | Nil, _ :: _ -> fail path "too many elements (expected %d)" i
  | Cons _, [] -> fail path "not enough elements (got %d)" i

and fields_of_json : type r f.
    string list -> (r, f) fields -> f -> (string * json) list -> r =
 fun path fs make kvs ->
  match fs with
  | Fnil -> make
  | Fcons ({ name; kind; _ }, rest) ->
      let j = List.assoc_opt name kvs in
      let sub = name :: path in
      let v : _ =
        match (kind, j) with
        | Required ty, Some j -> make (of_json_aux sub ty j)
        | Required _, None -> fail path "missing field %S" name
        | Optional ty, Some j -> make (Some (of_json_aux sub ty j))
        | Optional _, None -> make None
        | Default { ty; _ }, Some j -> make (of_json_aux sub ty j)
        | Default { default; _ }, None -> make default
      in
      fields_of_json path rest v kvs

let of_json d j = of_json_aux [] d j
let to_string d x = Yojson.Safe.to_string (to_json d x)

let of_string d s =
  match Yojson.Safe.from_string s with
  | j -> of_json d j
  | exception Yojson.Json_error msg -> raise (Error ([], msg))

let () =
  Printexc.register_printer (function
    | Error (p, m) -> Some ("Desc.Json.Error: " ^ error_to_string (p, m))
    | _ -> None)

let of_encoder f =
  Core.custom "encoder" f (fun _ -> invalid_arg "Desc.Json.of_encoder")

let of_decoder g =
  Core.custom "decoder" (fun _ -> invalid_arg "Desc.Json.of_decoder") g
