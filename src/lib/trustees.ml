(**************************************************************************)
(*                                BELENIOS                                *)
(*                                                                        *)
(*  Copyright © 2012-2017 Inria                                           *)
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

open Platform
open Serializable_t
open Signatures
open Common

(** Helper functions *)

let check_modulo p x = Z.(geq x zero && lt x p)

(** Distributed key generation *)

module MakeSimpleDistKeyGen (G : GROUP) (M : RANDOM) = struct
  open G
  open M

  let ( >>= ) = bind
  let ( / ) x y = x *~ invert y

  (** Fiat-Shamir non-interactive zero-knowledge proofs of
      knowledge *)

  let fs_prove gs x oracle =
    random q >>= fun w ->
    let commitments = Array.map (fun g -> g **~ w) gs in
    let challenge = oracle commitments in
    let response = Z.((w + x * challenge) mod q) in
    return {challenge; response}

  let generate_and_prove () =
    random q >>= fun x ->
    let trustee_public_key = g **~ x in
    let zkp = "pok|" ^ G.to_string trustee_public_key ^ "|" in
    fs_prove [| g |] x (G.hash zkp) >>= fun trustee_pok ->
    return (x, {trustee_pok; trustee_public_key})

  let check {trustee_pok; trustee_public_key = y} =
    G.check y &&
    let {challenge; response} = trustee_pok in
    check_modulo q challenge &&
    check_modulo q response &&
    let commitment = g **~ response / (y **~ challenge) in
    let zkp = "pok|" ^ G.to_string y ^ "|" in
    Z.(challenge =% G.hash zkp [| commitment |])

  let combine pks =
    Array.fold_left (fun y {trustee_public_key; _} ->
      y *~ trustee_public_key
    ) G.one pks

end
