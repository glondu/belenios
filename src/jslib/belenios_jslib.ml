(**************************************************************************)
(*                                BELENIOS                                *)
(*                                                                        *)
(*  Copyright © 2012-2020 Inria                                           *)
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

open Js_of_ocaml
open Js_of_ocaml_lwt
open Belenios_platform.Platform
open Belenios
open Signatures
open Serializable_j
open Belenios_tool_js_common.Tool_js_common

let computeFingerprint = sha256_b64

let checkCredential = Credential.check

let encryptBallot params cred plaintext callback =
  let module P = (val params : ELECTION_DATA) in
  let module G = P.G in
  let module E = Election.Make (P) (LwtJsRandom) in
  let module CD = Credential.MakeDerive (G) in
  let sk = CD.derive P.election.e_params.e_uuid cred in
  let%lwt b = E.create_ballot ~sk plaintext in
  let ballot = string_of_ballot G.write b in
  let tracker = sha256_b64 ballot in
  callback ballot tracker

let belenios =
  object%js
    method computeFingerprint x =
      Js._JSON##stringify x
      |> Js.to_string
      |> computeFingerprint
      |> Js.string

    method checkCredential cred =
      if checkCredential (Js.to_string cred) then Js._true else Js._false

    method encryptBallot params cred plaintext success failure =
      let success ballot tracker =
        let () =
          Js.Unsafe.fun_call success
            [|
              Js.Unsafe.inject (Js.string ballot);
              Js.Unsafe.inject (Js.string tracker);
            |]
        in
        Lwt.return_unit
      in
      let failure error =
        let () =
          Js.Unsafe.fun_call failure
            [|
              Js.Unsafe.inject (Js.string error);
            |]
        in
        Lwt.return_unit
      in
      Lwt.async
        (fun () ->
          try%lwt
            let%lwt () = Lwt_js.yield () in
            let params =
              Js._JSON##stringify params
              |> Js.to_string
              |> Election.of_string
              |> Election.get_group
            in
            let%lwt () = Lwt_js.yield () in
            let plaintext =
              Js._JSON##stringify plaintext
              |> Js.to_string
              |> plaintext_of_string
            in
            let%lwt () = Lwt_js.yield () in
            encryptBallot params (Js.to_string cred) plaintext success
          with e ->
            failure (Printexc.to_string e)
        );
      Js.undefined
  end

let () = Js.export "belenios" belenios
