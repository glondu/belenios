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

open Signatures_core
open Common_types
open Election_types
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

type 'a batch = {
  private_creds : private_credentials;
  public_creds : 'a public_credentials;
  public_with_ids : 'a public_credentials_with_id;
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

  val generate : voters -> public_key batch m

  val derive :
    string -> (private_key, [ `Wrong | `Invalid | `MaybePassword ]) result m
end

module Make (G : GROUP) (E : ELECTION with type public_key := G.t) = struct
  module GMap = Map.Make (G)

  let ( let* ) = E.bind

  let derive_full seed =
    let dst = dst_prefix ^ "-derive_credential" in
    (* TODO: get rid of uuid in the following line (when the formal proof is done) *)
    (G.Zq.hash ~dst 1 (Printf.sprintf "%s|%s" (Uuid.to_string E.uuid) seed)).(0)

  let generate_one () =
    (* we generate only full style credentials *)
    let private_credential = generate_token salt_length |> format_full in
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
    let* privs, pubs =
      HMap.bindings voters
      |> monadic_fold_left
           (fun (privs, pubs) (_, v) ->
             let username = v.login in
             let weight = v.weight in
             let { private_credential; private_key } = generate_one () in
             let credential = G.(g **~ private_key) in
             ( HMap.add (Hash.hash_string username) private_credential privs,
               HMap.add
                 (credential |> G.to_string |> Hash.hash_string)
                 { credential; weight; id = Some username }
                 pubs ))
           (HMap.empty, HMap.empty)
    in
    {
      private_creds = privs;
      public_creds = HMap.map (fun x -> { x with id = None }) pubs;
      public_with_ids = pubs;
    }
    |> E.return
end
