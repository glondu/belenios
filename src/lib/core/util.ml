(**************************************************************************)
(*                                BELENIOS                                *)
(*                                                                        *)
(*  Copyright © 2012-2022 Inria                                           *)
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

open Signatures
open Serializable_t

let count_trustees trustees =
  List.fold_left
    (fun accu x ->
      match x with
      | `Single _ -> accu + 1
      | `Pedersen p -> accu + Array.length p.t_verification_keys)
    0 trustees

let arrange_partial_decryptions trustees partial_decryptions =
  let n = count_trustees trustees in
  let tmp = Array.make n None in
  let () =
    List.iter
      (fun x -> tmp.(x.owned_owner - 1) <- Some x.owned_payload)
      partial_decryptions
  in
  let _, accu =
    List.fold_left
      (fun (i, accu) x ->
        match x with
        | `Single _ -> (i + 1, `Single tmp.(i) :: accu)
        | `Pedersen p ->
            p.t_verification_keys |> Array.mapi (fun j _ -> tmp.(i + j))
            |> fun x -> (i + Array.length x, `Pedersen x :: accu))
      (0, []) trustees
  in
  List.rev accu

exception CombinationError of combination_error

let compute_synthetic_factors_exc trustees check partial_decryptions fold =
  List.map2
    (fun x y ->
      match (x, y) with
      | `Single x, `Single y -> (
          match y with
          | None -> raise (CombinationError MissingPartialDecryption)
          | Some y when check x.trustee_public_key y -> y.decryption_factors
          | _ -> raise (CombinationError InvalidPartialDecryption))
      | `Pedersen x, `Pedersen y ->
          let length = Array.length x.t_verification_keys in
          assert (length = Array.length y);
          let check x y =
            match y with None -> true | Some y -> check x.trustee_public_key y
          in
          if Array.for_all2 check x.t_verification_keys y then
            let y = Array.mapi (fun i x -> (i + 1, x)) y in
            let rec take n i accu =
              if n > 0 then
                if i < length then
                  match y.(i) with
                  | _, None -> take n (i + 1) accu
                  | id, Some y -> take (n - 1) (i + 1) ((id, y) :: accu)
                else raise (CombinationError NotEnoughPartialDecryptions)
              else accu
            in
            let pds_with_ids = take x.t_threshold 0 [] in
            fold pds_with_ids
          else raise (CombinationError InvalidPartialDecryption)
      | _ -> invalid_arg "combine_factors")
    trustees
    (arrange_partial_decryptions trustees partial_decryptions)

let compute_synthetic_factors trustees check partial_decryptions fold =
  try Ok (compute_synthetic_factors_exc trustees check partial_decryptions fold)
  with CombinationError e -> Error e
