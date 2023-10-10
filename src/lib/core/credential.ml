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

open Belenios_platform.Platform
open Signatures
open Serializable_t
open Common

let token_length = 14
let salt_length = 22 (* > 128 bits of entropy *)
let n58 = Z.of_int 58
let n53 = Z.of_int 53

let format x =
  assert (String.length x = token_length);
  Printf.sprintf "%s-%s-%s-%s" (String.sub x 0 3) (String.sub x 3 4)
    (String.sub x 7 3) (String.sub x 10 4)

let check_old_raw x =
  String.length x = token_length + 1
  &&
  let rec loop i accu =
    if i < token_length then
      let& digit = String.index_opt b58_digits x.[i] in
      loop (i + 1) Z.((n58 * accu) + of_int digit)
    else Some accu
  in
  match (loop 0 Z.zero, String.index_opt b58_digits x.[token_length]) with
  | Some n, Some checksum -> Z.((n + of_int checksum) mod n53 =% zero)
  | _, _ -> false

let check_old xs =
  List.for_all (fun x -> String.length x = 3) xs
  && check_old_raw (String.concat "" xs)

let check x n =
  String.length x = n
  && String.for_all (fun digit -> String.contains b58_digits digit) x

let parse_raw x =
  match String.split_on_char '-' x with
  | [ a ] when check_old_raw a ->
      (* very old style credential with no "-", e.g. 123456789abcdeN *)
      `Valid_old
  | [ _; _; _; _; _ ] as xs when check_old xs ->
      (* old style credential with "-", e.g. 123-456-789-abc-deN *)
      `Valid_old
  | [ _; _; _ ] as xs when List.for_all (fun x -> String.length x = 5) xs ->
      (* maybe a password, e.g. XXXXX-XXXXX-XXXXX *)
      `MaybePassword
  | [ a; b; c; d; e ] when List.for_all2 check [ a; b; c; d ] [ 3; 4; 3; 4 ]
    -> (
      (* new style credential with index, e.g. XXX-XXXX-XXX-XXXX-NNNN *)
      match int_of_string_opt e with
      | Some e -> `Valid (String.concat "" [ a; b; c; d ], e)
      | None -> `Invalid)
  | _ -> `Invalid

type 'a t = { raw : string; private_key : 'a; salt : string }

type batch = {
  private_creds : private_credentials;
  public_creds : public_credentials;
  public_with_ids_and_salts : string list;
}

module type ELECTION = sig
  type 'a t
  type public_key

  val return : 'a -> 'a t
  val bind : 'a t -> ('a -> 'b t) -> 'b t
  val pause : unit -> unit t
  val uuid : Uuid.t
  val get_salt : int -> public_key salt option t
end

module type S = sig
  type 'a m
  type private_key
  type public_key

  val generate : Voter.t list -> batch m

  val derive :
    string ->
    (private_key, [ `Wrong | `Invalid | `MaybePassword | `MissingSalt ]) result
    m

  val parse_public_credential : string -> (Weight.t * public_key) option
end

module Make (R : RANDOM) (G : GROUP) (E : ELECTION with type public_key := G.t) =
struct
  module GT = MakeGenerateToken (R)
  module GMap = Map.Make (G)

  let ( let* ) = E.bind

  let derive_raw ~salt x =
    (* TODO: do not depend on uuid *)
    let salt = Uuid.unwrap E.uuid ^ salt in
    let derived = pbkdf2_utf8 ~iterations:100000 ~salt ~size:2 x in
    G.Zq.reduce_hex derived

  let generate_token = GT.generate_token ~length:token_length
  let generate_salt = GT.generate_token ~length:salt_length

  let generate_one () =
    (* we generate only new-style credentials *)
    let raw = generate_token () in
    let salt = generate_salt () in
    let private_key = derive_raw ~salt raw in
    { raw = format raw; salt; private_key }

  let derive x =
    match parse_raw x with
    | `Valid_old ->
        let salt = Uuid.unwrap E.uuid in
        let derived = pbkdf2_utf8 ~iterations:1000 ~salt ~size:1 x in
        let r = G.Zq.reduce_hex derived in
        E.return (Ok r)
    | `Valid (raw, index) -> (
        let* salt = E.get_salt index in
        match salt with
        | None -> E.return (Error `MissingSalt)
        | Some { salt; public_credential } ->
            let x = derive_raw ~salt raw in
            if G.(compare (g **~ x) public_credential = 0) then E.return (Ok x)
            else E.return (Error `Wrong))
    | `Invalid -> E.return (Error `Invalid)
    | `MaybePassword -> E.return (Error `MaybePassword)

  let rec monadic_fold_left f accu = function
    | [] -> E.return accu
    | x :: xs ->
        let* () = E.pause () in
        monadic_fold_left f (f accu x) xs

  let generate voters =
    let implicit_weights = not (Voter.has_explicit_weights voters) in
    let* privs, pubs =
      monadic_fold_left
        (fun (privs, pubs) v ->
          let _, username, weight = Voter.get v in
          let { raw; salt; private_key } = generate_one () in
          ( SMap.add username (ref raw) privs,
            GMap.add G.(g **~ private_key) (weight, username, salt) pubs ))
        (SMap.empty, GMap.empty) voters
    in
    let serialize_with_id_and_salt (e, (w, id, salt)) =
      G.to_string e
      ^ (if implicit_weights then ","
         else Printf.sprintf ",%s" (Weight.to_string w))
      ^ Printf.sprintf ",%s" id ^ Printf.sprintf ",%s" salt
    in
    let serialize_public (e, (w, _, _)) =
      G.to_string e
      ^
      if implicit_weights then "" else Printf.sprintf ",%s" (Weight.to_string w)
    in
    let bindings = GMap.bindings pubs in
    let index_size = String.length (string_of_int (GMap.cardinal pubs - 1)) in
    let () =
      List.iteri
        (fun i (_, (_, id, _)) ->
          let r = SMap.find id privs in
          r := Printf.sprintf "%s-%0*d" !r index_size i)
        bindings
    in
    let private_creds =
      SMap.fold (fun id cred creds -> (id, !cred) :: creds) privs [] |> List.rev
    in
    {
      private_creds;
      public_creds = List.map serialize_public bindings;
      public_with_ids_and_salts = List.map serialize_with_id_and_salt bindings;
    }
    |> E.return

  let parse_public_credential s =
    match parse_public_credential G.of_string s with
    | exception Invalid_argument _ -> None
    | p ->
        if G.check p.credential then
          let weight = Option.value ~default:Weight.one p.weight in
          Some (weight, p.credential)
        else None
end
