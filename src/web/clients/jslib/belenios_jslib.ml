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

class type renderingFunctions = object
  method text : int -> Js.js_string Js.t -> Js.Unsafe.any Js.meth
  method br : int -> Js.Unsafe.any Js.meth
  method bold : int -> Js.Unsafe.any Js.js_array Js.t -> Js.Unsafe.any Js.meth
  method italic : int -> Js.Unsafe.any Js.js_array Js.t -> Js.Unsafe.any Js.meth
  method result : Js.Unsafe.any Js.js_array Js.t -> Js.Unsafe.any Js.meth
  method error : Js.js_string Js.t -> Js.Unsafe.any Js.meth
end

module type ELECTION_WITH_SK = sig
  include ELECTION

  val sk : G.Zq.t
end

class type checkCredentialCallbacks = object
  method success : (module ELECTION_WITH_SK) -> unit Js.meth
  method failure : Js.js_string Js.t -> Js.js_string Js.t Js.opt -> unit Js.meth
end

class type encryptBallotCallbacks = object
  method success : Js.js_string Js.t -> Js.js_string Js.t -> unit Js.meth
  method failure : Js.js_string Js.t -> unit Js.meth
end

class type belenios = object
  method setApiRoot : Js.js_string Js.t -> unit Js.meth
  method computeFingerprint : Js.Unsafe.any Js.t -> Js.js_string Js.t Js.meth

  method checkCredential :
    Js.Unsafe.any ->
    Js.js_string Js.t ->
    checkCredentialCallbacks Js.t ->
    unit Js.meth

  method encryptBallot :
    (module ELECTION_WITH_SK) ->
    Js.js_string Js.t ->
    encryptBallotCallbacks Js.t ->
    unit Js.meth

  method markup :
    renderingFunctions Js.t -> Js.js_string Js.t -> Js.Unsafe.any Js.meth
end

let apiRoot = ref "../../../api"

let belenios : belenios Js.t =
  object%js
    method setApiRoot x = apiRoot := Js.to_string x

    method computeFingerprint x =
      Js._JSON##stringify x |> Js.to_string |> sha256_b64 |> Js.string

    method checkCredential params cred
        (callbacks : checkCredentialCallbacks Js.t) =
      Lwt.async (fun () ->
          Lwt.catch
            (fun () ->
              let* () = Lwt_js.yield () in
              let module R = struct
                let raw_election = Js._JSON##stringify params |> Js.to_string
              end in
              let module W = Election.Make (R) (Random) () in
              let* () = Lwt_js.yield () in
              let module Cred =
                Credential.Make
                  (W.G)
                  (struct
                    type 'a t = 'a Lwt.t

                    let return = Lwt.return
                    let bind = Lwt.bind
                    let pause = Lwt.pause
                    let uuid = W.uuid

                    let get_salt i =
                      Printf.ksprintf
                        (get (salt_of_string (sread W.G.of_string)))
                        "%s/elections/%s/salts/%d" !apiRoot (Uuid.unwrap uuid) i
                  end)
              in
              let* x = Cred.derive (Js.to_string cred) in
              let () =
                match x with
                | Ok sk ->
                    let module X : ELECTION_WITH_SK = struct
                      include W

                      let sk = sk
                    end in
                    callbacks##success (module X : ELECTION_WITH_SK)
                | Error `Invalid ->
                    callbacks##failure (Js.string "INVALID_CREDENTIAL") Js.null
                | Error `MaybePassword ->
                    callbacks##failure (Js.string "MAYBE_PASSWORD") Js.null
                | Error `Wrong ->
                    callbacks##failure (Js.string "WRONG_CREDENTIAL") Js.null
                | Error `MissingSalt ->
                    callbacks##failure (Js.string "MISSING_SALT") Js.null
              in
              Lwt.return_unit)
            (fun e ->
              callbacks##failure
                (Js.string "GENERIC_FAILURE")
                (Js.some (Printexc.to_string e |> Js.string));
              Lwt.return_unit))

    method encryptBallot election plaintext
        (callbacks : encryptBallotCallbacks Js.t) =
      Lwt.async (fun () ->
          Lwt.catch
            (fun () ->
              let* () = Lwt_js.yield () in
              let open (val election : ELECTION_WITH_SK) in
              let plaintext =
                Js._JSON##stringify plaintext
                |> Js.to_string |> plaintext_of_string
              in
              let* () = Lwt_js.yield () in
              let b = E.create_ballot ~sk plaintext in
              let ballot = write_ballot -- b in
              let tracker = sha256_b64 ballot in
              callbacks##success (Js.string ballot) (Js.string tracker);
              Lwt.return_unit)
            (fun e ->
              callbacks##failure (Printexc.to_string e |> Js.string);
              Lwt.return_unit))

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
