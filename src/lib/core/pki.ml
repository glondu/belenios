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
open Belenios_platform
open Common_types
open Crypto_types
open Signatures_core
open Signatures
open Common

module Make (G : GROUP) = struct
  module Group = G

  type private_key = G.Zq.t
  type public_key = G.t

  let random () = G.Zq.random (Crypto_primitives.get_rng ())
  let genkey () = generate_token 44

  (** Deriving signing keys *)

  let dst_sk = dst_prefix ^ "-derive_sk"
  let derive_sk p = (G.Zq.hash ~dst:dst_sk 1 p).(0)

  (** Deriving decryption keys *)

  let dst_dk = dst_prefix ^ "-derive_dk"
  let derive_dk p = (G.Zq.hash ~dst:dst_dk 1 p).(0)

  (** Generic signature *)

  let sign { dst; to_yojson; _ } sk message =
    let w = random () in
    let commitment = G.(g **~ w) in
    let vk = G.(g **~ sk) in
    let prefix =
      Printf.sprintf "%s|%s" (G.to_string vk) (!+to_yojson message)
    in
    let challenge = G.hash ~dst prefix [| commitment |] in
    let response = G.Zq.(w - (sk * challenge)) in
    let signature = { challenge; response } in
    { message; signature }

  let verify xch vk { message; signature = { challenge; response } } =
    let commitment = G.((g **~ response) *~ (vk **~ challenge)) in
    let prefix =
      Printf.sprintf "%s|%s" (G.to_string vk) (!+(xch.to_yojson) message)
    in
    G.Zq.(challenge =% G.hash ~dst:xch.dst prefix [| commitment |])

  (** Generic encryption *)

  let encrypt ~algorithm xch y plaintext =
    let module E = (val Crypto_primitives.get_endecrypt algorithm) in
    let plaintext = !+(xch.to_yojson) plaintext in
    let r = random () in
    let key = random () in
    let key = G.(g **~ key) in
    let alpha = G.(g **~ r) in
    let beta = G.((y **~ r) *~ key) in
    let key =
      Printf.ksprintf sha256_hex "%s-key|%s" xch.dst (G.to_string key)
    in
    let iv =
      Printf.ksprintf sha256_hex "%s-iv|%s" xch.dst (G.to_string alpha)
    in
    let* data = E.encrypt ~key ~iv ~plaintext in
    Lwt.return { alpha; beta; data }

  let decrypt ~algorithm xch x { alpha; beta; data } =
    let@ () =
     fun cont ->
      if G.check alpha && G.check beta then cont () else Lwt.return_none
    in
    let module E = (val Crypto_primitives.get_endecrypt algorithm) in
    let key =
      Printf.ksprintf sha256_hex "%s-key|%s" xch.dst
        G.(to_string (beta *~ invert (alpha **~ x)))
    in
    let iv =
      Printf.ksprintf sha256_hex "%s-iv|%s" xch.dst (G.to_string alpha)
    in
    let* x = E.decrypt ~key ~iv ~ciphertext:data in
    match x with
    | None -> Lwt.return_none
    | Some x -> (
        try Lwt.return_some @@ !*(xch.of_yojson) x with _ -> Lwt.return_none)
end

module MakeChannels (P : PKI) = struct
  module P = P
  module G = P.Group

  type private_key = P.private_key
  type public_key = P.public_key
  type 'a msg = (public_key, private_key, 'a) sent_msg

  let cast_xch_inner { dst; of_yojson; to_yojson } =
    {
      dst = dst ^ "-channel_msg_inner";
      to_yojson = [%yojson_of_group: _ channel_msg] to_yojson;
      of_yojson = [%group_of_yojson: _ channel_msg] of_yojson;
    }

  let cast_xch_outer { dst; of_yojson; to_yojson } =
    {
      dst = dst ^ "-channel_msg_outer";
      to_yojson = [%yojson_of_group: _ signed_msg] to_yojson;
      of_yojson = [%group_of_yojson: _ signed_msg] of_yojson;
    }

  let send ~algorithm xch sk recipient message =
    let xch = cast_xch_inner xch in
    let xch' = cast_xch_outer xch in
    let msg = P.sign xch sk { recipient; message } in
    P.encrypt ~algorithm xch' recipient msg

  let recv ~algorithm xch dk vk msg =
    let xch = cast_xch_inner xch in
    let xch' = cast_xch_outer xch in
    let* x = P.decrypt ~algorithm xch' dk msg in
    match x with
    | None -> failwith "invalid ciphertext in received message"
    | Some msg ->
        if not (P.verify xch vk msg) then
          failwith "invalid signature on received message";
        let { recipient; message } = msg.message in
        if not G.(recipient =~ g **~ dk) then
          failwith "invalid recipient on received message";
        Lwt.return message
end
