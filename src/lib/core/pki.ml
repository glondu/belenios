(**************************************************************************)
(*                                BELENIOS                                *)
(*                                                                        *)
(*  Copyright © 2012-2025 Inria                                           *)
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

open Lwt.Syntax
open Belenios_platform.Platform
open Serializable_core_t
open Serializable_j
open Signatures
open Common

module Make (G : GROUP) (M : RANDOM) = struct
  module Group = G
  module Random = M

  type private_key = G.Zq.t
  type public_key = G.t

  let random () = G.Zq.random (M.get_rng ())
  let genkey () = generate_b58_token ~rng:(M.get_rng ()) ~length:22
  let derive_sk p = G.Zq.reduce_hex (sha256_hex ("sk|" ^ p))
  let derive_dk p = G.Zq.reduce_hex (sha256_hex ("dk|" ^ p))

  let sign sk s_message =
    let w = random () in
    let commitment = G.(g **~ w) in
    let prefix = "sigmsg|" ^ s_message ^ "|" in
    let challenge = G.hash prefix [| commitment |] in
    let response = G.Zq.(w - (sk * challenge)) in
    let s_signature = { challenge; response } in
    { s_message; s_signature }

  let verify vk { s_message; s_signature = { challenge; response } } =
    let commitment = G.((g **~ response) *~ (vk **~ challenge)) in
    let prefix = "sigmsg|" ^ s_message ^ "|" in
    G.Zq.(challenge =% G.hash prefix [| commitment |])

  let encrypt y plaintext =
    let y_algorithm = "AES-GCM" in
    let module E = (val Crypto_primitives.get_endecrypt y_algorithm) in
    let r = random () in
    let key = random () in
    let key = G.(g **~ key) in
    let y_alpha = G.(g **~ r) in
    let y_beta = G.((y **~ r) *~ key) in
    let key = sha256_hex ("key|" ^ G.to_string key) in
    let iv = sha256_hex ("iv|" ^ G.to_string y_alpha) in
    let* y_data = E.encrypt ~key ~iv ~plaintext in
    Lwt.return { y_algorithm; y_alpha; y_beta; y_data }

  let decrypt x { y_algorithm; y_alpha; y_beta; y_data } =
    let module E = (val Crypto_primitives.get_endecrypt y_algorithm) in
    let key =
      sha256_hex G.("key|" ^ to_string (y_beta *~ invert (y_alpha **~ x)))
    in
    let iv = sha256_hex ("iv|" ^ G.to_string y_alpha) in
    E.decrypt ~key ~iv ~ciphertext:y_data
end

module MakeChannels (P : PKI) = struct
  module Pki = P
  module G = P.Group

  type private_key = P.private_key
  type public_key = P.public_key

  let send sk c_recipient c_message =
    let msg = { c_recipient; c_message } in
    let msg = string_of_channel_msg (swrite G.to_string) msg in
    let msg = P.sign sk msg in
    P.encrypt c_recipient (string_of_signed_msg (swrite G.Zq.to_string) msg)

  let recv dk vk msg =
    let* x = P.decrypt dk msg in
    match x with
    | None -> failwith "invalid ciphertext in received message"
    | Some msg ->
        let msg = signed_msg_of_string (sread G.Zq.of_string) msg in
        if not (P.verify vk msg) then
          failwith "invalid signature on received message";
        let msg = channel_msg_of_string (sread G.of_string) msg.s_message in
        let { c_recipient; c_message } = msg in
        if not G.(c_recipient =~ g **~ dk) then
          failwith "invalid recipient on received message";
        Lwt.return c_message
end
