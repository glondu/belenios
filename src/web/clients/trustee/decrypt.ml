(**************************************************************************)
(*                                BELENIOS                                *)
(*                                                                        *)
(*  Copyright © 2024-2024 Inria                                           *)
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
open Js_of_ocaml_tyxml
open Tyxml_js.Html5
open Belenios
open Belenios_js.Common
open Belenios_js.Session
open Belenios_api.Serializable_j
open Common

let basic_check_private_key s =
  let open (val !Belenios_js.I18n.gettext) in
  let n = String.length s in
  let rec leading i =
    if i < n then
      match s.[i] with
      | '"' -> middle (i + 1)
      | _ ->
          failwith
            (s_
               "Must start with a double quote (character given was another \
                character)")
    else failwith (s_ "Must start with a double quote (empty string given)")
  and middle i =
    if i < n then
      match s.[i] with
      | '0' .. '9' -> ending (i + 1)
      | _ -> failwith (s_ "Must have at least one digit")
    else failwith (s_ "Too short")
  and ending i =
    if i < n then
      match s.[i] with
      | '0' .. '9' -> ending (i + 1)
      | '"' -> if i + 1 < n then failwith (s_ "Must end with a double quote")
      | c -> Printf.ksprintf failwith (f_ "Illegal character: %c") c
    else failwith (s_ "Must end with a double quote")
  in
  leading 0

let compute_partial_decryption trustee ~election ~encrypted_tally ~private_key =
  let open (val !Belenios_js.I18n.gettext) in
  let module W = (val Election.of_string (module Random) election) in
  let encrypted_tally =
    encrypted_tally_of_string (sread W.G.of_string) encrypted_tally
  in
  let private_key =
    match trustee.tally_trustee_private_key with
    | Some epk ->
        let module Trustees = (val Trustees.get_by_version W.version) in
        let module PKI = Trustees.MakePKI (W.G) (Random) in
        let module C = Trustees.MakeChannels (W.G) (Random) (PKI) in
        let sk = PKI.derive_sk private_key and dk = PKI.derive_dk private_key in
        let vk = W.G.(g **~ sk) in
        let epk =
          C.recv dk vk (encrypted_msg_of_string (sread W.G.of_string) epk)
        in
        (partial_decryption_key_of_string (sread W.G.Zq.of_string) epk)
          .pdk_decryption_key
    | None -> (
        basic_check_private_key private_key;
        try sread W.G.Zq.of_string ++ private_key
        with e ->
          Printf.ksprintf failwith
            (f_ "Error in format of private key: %s")
            (Printexc.to_string e))
  in
  W.E.compute_factor encrypted_tally private_key
  |> string_of_partial_decryption (swrite W.G.to_string)
       (swrite W.G.Zq.to_string)
  |> Lwt.return

let decrypt uuid ~token =
  let open (val !Belenios_js.I18n.gettext) in
  let fail () =
    Lwt.return [ div [ txt @@ s_ "Error while loading election parameters!" ] ]
  in
  let@ trustee cont =
    let* x = Api.(get (trustee_election uuid) (`Trustee token)) in
    match x with Ok (x, _) -> cont x | Error _ -> fail ()
  in
  let@ election cont =
    let* x = Api.(get (election uuid) `Nobody) in
    match x with Ok (x, _) -> cont x | Error _ -> fail ()
  in
  let@ encrypted_tally cont =
    let* x = Api.(get (election_encrypted_tally uuid) `Nobody) in
    match x with Ok (x, _) -> cont x | Error _ -> fail ()
  in
  let container = Dom_html.createDiv document in
  let encrypted_tally_hash = sha256_b64 encrypted_tally in
  let input_private_key, get_private_key = make_private_key_input () in
  let partial_decryption = ref "" in
  let submit =
    let@ () = button ~a:[ a_id "submit_data"; a_disabled () ] @@ s_ "Submit" in
    let* x =
      Api.(post (trustee_election uuid) (`Trustee token) !partial_decryption)
    in
    let msg =
      match x.code with
      | 200 -> s_ "Your partial decryption has been received and checked!"
      | 400 -> s_ "The partial decryption didn't pass validation!"
      | 409 -> s_ "You have already submitted a valid partial decryption!"
      | _ -> s_ "Partial decryption submission failed unexpectedly!"
    in
    let element = div ~a:[ a_id "success" ] [ txt msg ] in
    Dom.appendChild container (Tyxml_js.To_dom.of_div element);
    Lwt.return_unit
  in
  let compute =
    let@ () =
      button ~a:[ a_id "compute_factor" ]
      @@ s_ "Generate your contribution to decryption"
    in
    let private_key = get_private_key () in
    let* pd =
      compute_partial_decryption trustee ~election ~encrypted_tally ~private_key
    in
    partial_decryption := pd;
    let r = Tyxml_js.To_dom.of_button submit in
    r##.disabled := Js._false;
    Lwt.return_unit
  in
  let title = h3 [ txt @@ s_ "Partial decryption" ] in
  let contents =
    [
      p
        [
          txt @@ s_ "It is now time to compute your partial decryption factors.";
        ];
      p
        [
          txt @@ s_ "The fingerprint of the encrypted tally is ";
          b [ span ~a:[ a_id "hash" ] [ txt encrypted_tally_hash ] ];
          txt ".";
        ];
      hr ();
      div
        [
          b [ txt @@ s_ "Instructions:" ];
          ol
            [
              li [ input_private_key; br () ];
              li [ div [ compute ]; br () ];
              li
                [
                  div
                    ~a:[ a_id "pd_done" ]
                    [
                      div
                        ~a:[ a_id "pd_form" ]
                        [
                          div
                            [
                              submit;
                              txt @@ s_ " your contribution to decryption.";
                            ];
                        ];
                    ];
                ];
            ];
        ];
    ]
  in
  List.iter
    (fun x -> Dom.appendChild container (Tyxml_js.To_dom.of_node x))
    contents;
  Lwt.return [ title; hr (); Tyxml_js.Of_dom.of_div container ]
