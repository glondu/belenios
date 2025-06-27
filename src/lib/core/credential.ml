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
open Crypto_primitives
open Signatures
open Serializable_t
open Common

let salt_length = 22 (* > 128 bits of entropy *)

let format_full x =
  assert (String.length x = salt_length);
  Printf.sprintf "%s-%s-%s-%s" (String.sub x 0 5) (String.sub x 5 6)
    (String.sub x 11 5) (String.sub x 16 6)

let check x n =
  String.length x = n
  && String.for_all (fun digit -> String.contains b58_digits digit) x

let parse_raw x =
  match String.split_on_char '-' x with
  | [ _; _; _ ] as xs when List.for_all (fun x -> String.length x = 5) xs ->
      (* maybe a password, e.g. XXXXX-XXXXX-XXXXX *)
      `MaybePassword
  | [ a; b; c; d ] when List.for_all2 check [ a; b; c; d ] [ 5; 6; 5; 6 ] ->
      (* full style credential, e.g. XXXXX-XXXXXX-XXXXX-XXXXXX *)
      `Valid_full
  | _ -> `Invalid

type 'a t = { private_credential : string; private_key : 'a }

type batch = {
  private_creds : private_credentials;
  public_creds : public_credentials;
  public_with_ids : string list;
}

module type ELECTION = sig
  type 'a t
  type public_key

  val return : 'a -> 'a t
  val bind : 'a t -> ('a -> 'b t) -> 'b t
  val pause : unit -> unit t
  val uuid : Uuid.t
end

module type S = sig
  type 'a m
  type private_key
  type public_key

  val generate : Voter.t list -> batch m
  val generate_sub : int -> sub_batch m * (unit -> int)
  val merge_sub : Voter.t list -> sub_batch -> batch

  val derive :
    string -> (private_key, [ `Wrong | `Invalid | `MaybePassword ]) result m

  val parse_public_credential : string -> (Weight.t * public_key) option
end

module Make (G : GROUP) (E : ELECTION with type public_key := G.t) = struct
  module GMap = Map.Make (G)

  let ( let* ) = E.bind

  let derive_full seed =
    let output_length =
      128
      (* 512 bits *)
    in
    (* TODO: get rid of uuid in the following line (when the formal proof is done) *)
    let prefix = Printf.sprintf "derive_credential|%s" (Uuid.unwrap E.uuid) in
    let b = Buffer.create output_length in
    let rec loop i =
      if Buffer.length b >= output_length then
        Buffer.contents b |> G.Zq.reduce_hex
      else (
        Printf.ksprintf
          (sha256_hex >> Buffer.add_string b)
          "%s|%d|%s" prefix i seed;
        loop (i + 1))
    in
    loop 0

  let generate_one rng =
    (* we generate only full style credentials *)
    let private_credential =
      generate_b58_token ~rng ~length:salt_length |> format_full
    in
    let private_key = derive_full private_credential in
    { private_credential; private_key }

  let derive x =
    match parse_raw x with
    | `Valid_full -> E.return (Ok (derive_full x))
    | `Invalid -> E.return (Error `Invalid)
    | `MaybePassword -> E.return (Error `MaybePassword)

  let rec monadic_fold_left f accu = function
    | [] -> E.return accu
    | x :: xs ->
        let* () = E.pause () in
        monadic_fold_left f (f accu x) xs

  let generate voters =
    let rng = pseudo_rng (random_string secure_rng 32) in
    let implicit_weights = not (Voter.has_explicit_weights voters) in
    let* privs, pubs =
      monadic_fold_left
        (fun (privs, pubs) v ->
          let username = Voter.get v in
          let weight = Voter.get_weight v in
          let { private_credential; private_key } = generate_one rng in
          ( SMap.add username private_credential privs,
            GMap.add G.(g **~ private_key) (weight, username) pubs ))
        (SMap.empty, GMap.empty) voters
    in
    let serialize_with_id (e, (w, id)) =
      G.to_string e
      ^ (if implicit_weights then ","
         else Printf.sprintf ",%s" (Weight.to_string w))
      ^ Printf.sprintf ",%s" id
    in
    let serialize_public (e, (w, _)) =
      G.to_string e
      ^
      if implicit_weights then "" else Printf.sprintf ",%s" (Weight.to_string w)
    in
    let bindings = GMap.bindings pubs in
    {
      private_creds = SMap.bindings privs;
      public_creds = List.map serialize_public bindings;
      public_with_ids = List.map serialize_with_id bindings;
    }
    |> E.return

  let generate_sub n =
    let rng = pseudo_rng (random_string secure_rng 32) in
    let n = ref n in
    let rec loop accu =
      if !n > 0 then (
        let* () = E.pause () in
        let { private_credential; private_key } = generate_one rng in
        let sub_public = G.(g **~ private_key |> to_string) in
        let x = { sub_base = private_credential; sub_public } in
        decr n;
        loop (x :: accu))
      else E.return accu
    in
    (loop [], fun () -> !n)

  let merge_sub voters subs =
    let implicit_weights = not (Voter.has_explicit_weights voters) in
    let privs, pubs =
      let rec loop (privs, pubs) voters subs =
        match (voters, subs) with
        | v :: vs, s :: ss ->
            let username = Voter.get v in
            let weight = Voter.get_weight v in
            let privs = SMap.add username s.sub_base privs in
            let pubs =
              GMap.add G.(of_string s.sub_public) (weight, username) pubs
            in
            loop (privs, pubs) vs ss
        | [], _ -> (privs, pubs)
        | _ :: _, [] -> failwith "merge_sub"
      in
      loop (SMap.empty, GMap.empty) voters subs
    in
    let serialize_with_id (e, (w, id)) =
      G.to_string e
      ^ (if implicit_weights then ","
         else Printf.sprintf ",%s" (Weight.to_string w))
      ^ Printf.sprintf ",%s" id
    in
    let serialize_public (e, (w, _)) =
      G.to_string e
      ^
      if implicit_weights then "" else Printf.sprintf ",%s" (Weight.to_string w)
    in
    let bindings = GMap.bindings pubs in
    {
      private_creds = SMap.bindings privs;
      public_creds = List.map serialize_public bindings;
      public_with_ids = List.map serialize_with_id bindings;
    }

  let parse_public_credential s =
    match parse_public_credential G.of_string s with
    | exception Invalid_argument _ -> None
    | p ->
        if G.check p.credential then
          let weight = Option.value ~default:Weight.one p.weight in
          Some (weight, p.credential)
        else None
end
