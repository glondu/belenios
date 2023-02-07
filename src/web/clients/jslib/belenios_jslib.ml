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

open Lwt.Syntax
open Js_of_ocaml
open Js_of_ocaml_lwt
open Belenios_core.Common
open Belenios_core
open Belenios
open Signatures
open Serializable_j
open Belenios_js.Common

let computeFingerprint = sha256_b64

let checkCredential = Credential.check

let encryptBallot election cred plaintext callback =
  let module P = (val election : ELECTION) in
  let module G = P.G in
  let module CD = Credential.MakeDerive (G) in
  let sk = CD.derive P.election.e_uuid cred in
  let b = P.E.create_ballot ~sk plaintext in
  let ballot = P.string_of_ballot b in
  let tracker = sha256_b64 ballot in
  callback ballot tracker

class type renderingFunctions =
  object
    method text : int -> Js.js_string Js.t -> Js.Unsafe.any Js.meth
    method br : int -> Js.Unsafe.any Js.meth
    method bold : int -> Js.Unsafe.any Js.js_array Js.t -> Js.Unsafe.any Js.meth
    method italic : int -> Js.Unsafe.any Js.js_array Js.t -> Js.Unsafe.any Js.meth
    method result : Js.Unsafe.any Js.js_array Js.t -> Js.Unsafe.any Js.meth
    method error : Js.js_string Js.t -> Js.Unsafe.any Js.meth
  end

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
          Lwt.catch (fun () ->
            let* () = Lwt_js.yield () in
            let module R =
              struct
                let raw_election = Js._JSON##stringify params |> Js.to_string
              end
            in
            let module W = Election.Make (R) (Random) () in
            let* () = Lwt_js.yield () in
            let plaintext =
              Js._JSON##stringify plaintext
              |> Js.to_string
              |> plaintext_of_string
            in
            let* () = Lwt_js.yield () in
            encryptBallot (module W) (Js.to_string cred) plaintext success
          ) (fun e -> failure (Printexc.to_string e))
        );
      Js.undefined

    method markup (p : renderingFunctions Js.t) x =
      let open Belenios_ui in
      let pp =
        {
          Markup.text = (fun key x -> p##text key (Js.string x));
          br = (fun key -> p##br key);
          italic = (fun key xs -> p##italic key (Js.array @@ Array.of_list xs));
          bold = (fun key xs -> p##bold key (Js.array @@ Array.of_list xs));
        }
      in
      try
        let lexbuf = Lexing.from_string (Js.to_string x) in
        let xs = Markup_parser.full Markup_lexer.token lexbuf in
        let xs = Markup.render pp xs in
        p##result (Js.array @@ Array.of_list xs)
      with _ -> p##error x
  end

let () = Js.export "belenios" belenios
