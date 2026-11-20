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

type json = Yojson.Safe.t

type _ t =
  | Unit : unit t
  | Bool : bool t
  | Int : int t
  | Int64 : int64 t
  | String : string t
  | Json : json t
  | List : 'a t -> 'a list t
  | Option : 'a t -> 'a option t
  | Tuple : { elts : ('r, 'f) elts; make : 'f } -> 'r t
  | Assoc : { value : 'a t; sorted : bool } -> (string * 'a) list t
  | Map : { base : 'a t; inj : 'a -> 'b; prj : 'b -> 'a } -> 'b t
  | Record : { fields : ('r, 'f) fields; make : 'f } -> 'r t
  | Variant : {
      cases : 'v any_case list;
      classify : 'v -> 'v case_value;
    }
      -> 'v t
  | Untagged : {
      cases : 'v any_case list;
      classify : 'v -> 'v case_value;
    }
      -> 'v t
  | Custom : {
      name : string;
      to_json : 'a -> json;
      of_json : json -> 'a;
    }
      -> 'a t
  | Rec : { name : string; body : 'a t Lazy.t } -> 'a t

and ('r, 'a) elt = { ty : 'a t; get : 'r -> 'a }

and ('r, 'f) elts =
  | Nil : ('r, 'r) elts
  | Cons : ('r, 'a) elt * ('r, 'f) elts -> ('r, 'a -> 'f) elts

and ('r, 'a) field = { name : string; kind : 'a field_kind; get : 'r -> 'a }

and _ field_kind =
  | Required : 'a t -> 'a field_kind
  | Optional : 'a t -> 'a option field_kind
  | Default : {
      ty : 'a t;
      default : 'a;
      equal : 'a -> 'a -> bool;
    }
      -> 'a field_kind

and ('r, 'f) fields =
  | Fnil : ('r, 'r) fields
  | Fcons : ('r, 'a) field * ('r, 'f) fields -> ('r, 'a -> 'f) fields

and ('v, 'a) case = { tag : string; arg : 'a case_arg; inj : 'a -> 'v }
and _ case_arg = No_arg : unit case_arg | Arg : 'a t -> 'a case_arg
and 'v any_case = Any : ('v, 'a) case -> 'v any_case
and 'v case_value = Value : ('v, 'a) case * 'a -> 'v case_value

exception Invalid of string

let invalid fmt = Printf.ksprintf (fun s -> raise (Invalid s)) fmt

(* JSON kinds *)

type kind = Knull | Kbool | Kint | Kfloat | Kstring | Klist | Kobject

let union a b = List.sort_uniq compare (a @ b)

(* [seen] holds the [Rec] nodes being visited, to stop on cycles (least
   fixpoint: a cycle adds no kind). *)
let rec kinds_aux : type a. Obj.t list -> a t -> kind list option =
 fun seen d ->
  match d with
  | Unit -> Some [ Knull ]
  | Bool -> Some [ Kbool ]
  | Int | Int64 -> Some [ Kint ]
  | String -> Some [ Kstring ]
  | Json | Custom _ -> None
  | List _ | Tuple _ -> Some [ Klist ]
  | Assoc _ | Record _ -> Some [ Kobject ]
  | Option d -> Option.map (union [ Knull ]) (kinds_aux seen d)
  | Map { base; _ } -> kinds_aux seen base
  | Variant { cases; _ } ->
      let consts, args =
        List.partition
          (fun (Any c) -> match c.arg with No_arg -> true | Arg _ -> false)
          cases
      in
      Some
        ((if consts = [] then [] else [ Kstring ])
        @ if args = [] then [] else [ Kobject ])
  | Untagged { cases; _ } ->
      List.fold_left
        (fun acc (Any c) ->
          match (acc, c.arg) with
          | None, _ -> None
          | Some _, No_arg -> acc
          | Some acc, Arg d -> Option.map (union acc) (kinds_aux seen d))
        (Some []) cases
  | Rec { body; _ } ->
      if not (Lazy.is_val body) then None
      else if List.memq (Obj.repr d) seen then Some []
      else kinds_aux (Obj.repr d :: seen) (Lazy.force body)

let kinds d = kinds_aux [] d

(* Base types *)

let unit = Unit
let bool = Bool
let int = Int
let int64 = Int64
let string = String
let json = Json

(* Containers *)

let list d = List d
let array d = Map { base = List d; inj = Array.of_list; prj = Array.to_list }

let option d =
  match kinds d with
  | Some ks when not (List.mem Knull ks) -> Option d
  | _ -> invalid "option: null may already stand for a value of the argument"

let assoc value = Assoc { value; sorted = false }
let sorted_assoc value = Assoc { value; sorted = true }

(* Tuples *)

let tuple elts make = Tuple { elts; make }
let elt ty get = { ty; get }

let tup2 a b =
  tuple (Cons (elt a fst, Cons (elt b snd, Nil))) (fun a b -> (a, b))

let tup3 a b c =
  tuple
    (Cons
       ( elt a (fun (x, _, _) -> x),
         Cons
           (elt b (fun (_, x, _) -> x), Cons (elt c (fun (_, _, x) -> x), Nil))
       ))
    (fun a b c -> (a, b, c))

let tup4 a b c d =
  tuple
    (Cons
       ( elt a (fun (x, _, _, _) -> x),
         Cons
           ( elt b (fun (_, x, _, _) -> x),
             Cons
               ( elt c (fun (_, _, x, _) -> x),
                 Cons (elt d (fun (_, _, _, x) -> x), Nil) ) ) ))
    (fun a b c d -> (a, b, c, d))

let tup5 a b c d e =
  tuple
    (Cons
       ( elt a (fun (x, _, _, _, _) -> x),
         Cons
           ( elt b (fun (_, x, _, _, _) -> x),
             Cons
               ( elt c (fun (_, _, x, _, _) -> x),
                 Cons
                   ( elt d (fun (_, _, _, x, _) -> x),
                     Cons (elt e (fun (_, _, _, _, x) -> x), Nil) ) ) ) ))
    (fun a b c d e -> (a, b, c, d, e))

(* Records *)

let field name ty get = { name; kind = Required ty; get }
let optional name ty get = { name; kind = Optional ty; get }

let default ?(equal = ( = )) name ty default get =
  { name; kind = Default { ty; default; equal }; get }

let rec field_names : type r f. (r, f) fields -> string list = function
  | Fnil -> []
  | Fcons (f, rest) -> f.name :: field_names rest

let check_unique what names =
  let tbl = Hashtbl.create 16 in
  List.iter
    (fun k ->
      if Hashtbl.mem tbl k then invalid "duplicate %s %S" what k
      else Hashtbl.add tbl k ())
    names

let record fields make =
  check_unique "field" (field_names fields);
  Record { fields; make }

(* Variants *)

let const tag v = { tag; arg = No_arg; inj = (fun () -> v) }
let case tag d inj = { tag; arg = Arg d; inj }

let variant cases classify =
  check_unique "tag" (List.map (fun (Any c) -> c.tag) cases);
  Variant { cases; classify }

let untagged cases classify =
  let _ : kind list option =
    List.fold_left
      (fun earlier (Any c) ->
        match (earlier, c.arg) with
        | _, No_arg -> invalid "untagged: case %S has no argument" c.tag
        | None, _ ->
            invalid "untagged: case %S follows a case of unknown kinds" c.tag
        | Some earlier, Arg d -> (
            match kinds d with
            | None -> None
            | Some ks ->
                if List.exists (fun k -> List.mem k earlier) ks then
                  invalid "untagged: case %S overlaps an earlier case" c.tag;
                Some (union earlier ks)))
      (Some []) cases
  in
  Untagged { cases; classify }

(* Other *)

let map base inj prj = Map { base; inj; prj }
let custom name to_json of_json = Custom { name; to_json; of_json }

let fix name f =
  let rec self = Rec { name; body } and body = lazy (f self) in
  ignore (Lazy.force body);
  self
