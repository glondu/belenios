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

open Desc

(* Tests of Desc and ppx_desc. *)

let check_string name expected got =
  if expected <> got then (
    Printf.printf "FAIL %s\n  expected: %s\n  got:      %s\n" name expected got;
    exit 1)

(* Encode [x], compare with [expected], decode back and compare. *)
let roundtrip ?(equal = ( = )) name d x expected =
  let s = Json.to_string d x in
  check_string name expected s;
  if not (equal (Json.of_string d s) x) then (
    Printf.printf "FAIL %s: decode (encode x) <> x\n" name;
    exit 1)

(* Decode [input] and check the canonical re-encoding. *)
let reencode name d input expected =
  check_string name expected (Json.to_string d (Json.of_string d input))

let decode_error name d input expected =
  match Json.of_string d input with
  | _ -> check_string name expected "(no error)"
  | exception Json.Error (p, m) ->
      check_string name expected (Json.error_to_string (p, m))

let invalid name f =
  match f () with
  | _ -> check_string name "Invalid" "(accepted)"
  | exception Core.Invalid _ -> ()

(* Variants: a constant, one argument, several arguments, a tuple argument,
   a record argument, a constructor name shadowing Stdlib. *)

type r = { x : int } [@@deriving desc]

type v = A | B of int | C of int * string | D of (int * string) | E of r | Ok
[@@deriving desc]

type pv = [ `A | `B of int * string ] [@@deriving desc]

(* Records: optional fields, defaults, option as a value, int64, renamed
   keys. *)

type event_type = [ `Setup | `Ballot ] [@@deriving desc]

type event = {
  typ : event_type; [@key "type"]
  parent : string option;
  weight : int64;
  deny_revote : bool; [@default false]
}
[@@deriving desc]

type shuffler = { fingerprint : string option option } [@@deriving desc]

(* Parameterized and recursive types. *)

type ('a, 'b) pair = { left : 'a; right : 'b list } [@@deriving desc]
type 'a tree = Leaf | Node of 'a tree * 'a * 'a tree [@@deriving desc]
type int_tree = int tree [@@deriving desc]

(* Signatures. *)

module M : sig
  type 'a t = { v : 'a; w : int option } [@@deriving desc]
end = struct
  type 'a t = { v : 'a; w : int option } [@@deriving desc]
end

(* Shape: untagged, recursive, hand-written. *)

type 'a shape = Atomic of 'a | Array of 'a shape array

let shape_d a =
  let open Desc.Core in
  fix "shape" (fun self ->
      let array_c = case "Array" (array self) (fun x -> Array x)
      and atomic = case "Atomic" a (fun x -> Atomic x) in
      untagged [ Any array_c; Any atomic ] (function
        | Array x -> Value (array_c, x)
        | Atomic x -> Value (atomic, x)))

type ballot = { answers : int shape list } [@@deriving desc]

(* Maps: smap through sorted_assoc. *)

module SMap = Map.Make (String)

let smap_d v = Core.map (Core.sorted_assoc v) SMap.of_list SMap.to_list

let () =
  roundtrip "constant" v_d A {|"A"|};
  roundtrip "one argument" v_d (B 1) {|{"B":1}|};
  roundtrip "several arguments" v_d (C (1, "s")) {|{"C":[1,"s"]}|};
  roundtrip "tuple argument" v_d (D (1, "s")) {|{"D":[1,"s"]}|};
  roundtrip "record argument" v_d (E { x = 1 }) {|{"E":{"x":1}}|};
  roundtrip "shadowing constructor" v_d Ok {|"Ok"|};
  roundtrip "polymorphic constant" pv_d `A {|"A"|};
  roundtrip "polymorphic tuple" pv_d (`B (1, "s")) {|{"B":[1,"s"]}|};
  decode_error "unknown constructor" v_d {|"Z"|} {|unknown constructor "Z"|};
  decode_error "missing argument" v_d {|"B"|}
    {|constructor "B" takes an argument|};
  decode_error "two tags" v_d {|{"A":null,"B":1}|}
    "expected a string or an object with one field, got object";
  let e = { typ = `Ballot; parent = None; weight = 1L; deny_revote = false } in
  roundtrip "omitted optional and default" event_d e
    {|{"type":"Ballot","weight":1}|};
  roundtrip "present optional and default" event_d
    {
      typ = `Setup;
      parent = Some "ab";
      weight = 100000000000L;
      deny_revote = true;
    }
    {|{"type":"Setup","parent":"ab","weight":100000000000,"deny_revote":true}|};
  reencode "key order, unknown fields" event_d
    {|{"weight":1,"zz":[1],"type":"Ballot"}|} {|{"type":"Ballot","weight":1}|};
  reencode "explicit default" event_d
    {|{"type":"Ballot","weight":1,"deny_revote":false}|}
    {|{"type":"Ballot","weight":1}|};
  decode_error "duplicate key" event_d
    {|{"type":"Ballot","weight":1,"weight":2}|} {|duplicate key "weight"|};
  decode_error "missing field" event_d {|{"type":"Ballot"}|}
    {|missing field "weight"|};
  decode_error "null optional" event_d
    {|{"type":"Ballot","weight":1,"parent":null}|}
    "parent: expected a string, got null";
  roundtrip "option option: absent" shuffler_d { fingerprint = None } {|{}|};
  roundtrip "option option: null" shuffler_d
    { fingerprint = Some None }
    {|{"fingerprint":null}|};
  roundtrip "option option: value" shuffler_d
    { fingerprint = Some (Some "ab") }
    {|{"fingerprint":"ab"}|};
  roundtrip "parameters"
    (pair_d Core.int Core.string)
    { left = 1; right = [ "a" ] }
    {|{"left":1,"right":["a"]}|};
  roundtrip "recursion" int_tree_d
    (Node (Leaf, 1, Node (Leaf, 2, Leaf)))
    {|{"Node":["Leaf",1,{"Node":["Leaf",2,"Leaf"]}]}|};
  roundtrip "signature" (M.t_d Core.string) { M.v = "a"; w = None }
    {|{"v":"a"}|};
  let m = SMap.of_list [ ("b", 2); ("a", 1); ("ä", 3) ] in
  roundtrip ~equal:(SMap.equal Int.equal) "sorted keys" (smap_d Core.int) m
    {|{"a":1,"b":2,"ä":3}|};
  reencode "unsorted input" (smap_d Core.int) {|{"b":2,"a":1}|}
    {|{"a":1,"b":2}|};
  roundtrip "assoc keeps order" (Core.assoc Core.int)
    [ ("b", 2); ("a", 1) ]
    {|{"b":2,"a":1}|};
  let s = Array [| Atomic 1; Array [| Atomic 2; Atomic 3 |]; Array [||] |] in
  roundtrip "nested" (shape_d Core.int) s {|[1,[2,3],[]]|};
  roundtrip "atomic" (shape_d Core.int) (Atomic 4) {|4|};
  roundtrip "atomic record" (shape_d r_d)
    (Array [| Atomic { x = 1 } |])
    {|[{"x":1}]|};
  decode_error "path" ballot_d {|{"answers":[[1,[2,"a"]]]}|}
    "answers.0.1.1: unexpected string";
  decode_error "int out of range" Core.int "123456789012345678901234567890"
    "integer out of range";
  invalid "option option" (fun () -> Core.(option (option int)));
  invalid "option unit" (fun () -> Core.(option unit));
  invalid "option json" (fun () -> Core.(option json));
  invalid "duplicate field" (fun () ->
      Core.(
        record
          (Fcons (field "x" int fst, Fcons (field "x" int snd, Fnil)))
          (fun a b -> (a, b))));
  invalid "duplicate tag" (fun () ->
      Core.(
        variant [ Any (const "A" 1); Any (const "A" 2) ] (fun _ -> assert false)));
  invalid "untagged overlap" (fun () ->
      Core.(
        untagged
          [ Any (case "I" int Fun.id); Any (case "J" int64 Int64.to_int) ]
          (fun _ -> assert false)));
  invalid "untagged after unknown" (fun () ->
      Core.(
        untagged
          [ Any (case "J" json (fun _ -> 0)); Any (case "I" int Fun.id) ]
          (fun _ -> assert false)));
  invalid "untagged constant" (fun () ->
      Core.(untagged [ Any (const "A" 0) ] (fun _ -> assert false)));
  (match shape_d Core.int with
  | Rec { body; _ } -> assert (Lazy.is_val body)
  | _ -> assert false);
  (match Core.kinds (shape_d Core.int) with
  | Some ks -> assert (List.sort compare ks = [ Core.Kint; Klist ])
  | None -> assert false);
  ()
