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

open Lwt.Syntax
open Js_of_ocaml
open Js_of_ocaml_lwt
open Belenios
open Belenios_js.Common
open Belenios_api.Serializable_j

let election = ref None
let encrypted_tally = ref None

let wrap f x =
  (try
     Js.Opt.case (f x) (fun () -> failwith "Unexpected error") (fun () -> ())
   with
  | Failure s -> alert s
  | e -> Printf.ksprintf alert "Unexpected error: %s" (Printexc.to_string e));
  Js._false

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

let compute_partial_decryption tally_trustee _ =
  let open (val !Belenios_js.I18n.gettext) in
  let&|&& e = !election in
  let module P =
    Election.Make
      (struct
        let raw_election = e
      end)
      (Random)
      ()
  in
  let&|&& e = !encrypted_tally in
  let encrypted_tally = encrypted_tally_of_string P.(sread G.of_string) e in
  let&& e = document##getElementById (Js.string "private_key") in
  let&& e = Dom_html.CoerceTo.input e in
  let pk_str = Js.to_string e##.value in
  let private_key =
    match tally_trustee.tally_trustee_private_key with
    | Some epk ->
        let module Trustees = (val Trustees.get_by_version P.version) in
        let module PKI = Trustees.MakePKI (P.G) (Random) in
        let module C = Trustees.MakeChannels (P.G) (Random) (PKI) in
        let sk = PKI.derive_sk pk_str and dk = PKI.derive_dk pk_str in
        let vk = P.G.(g **~ sk) in
        let epk =
          C.recv dk vk (encrypted_msg_of_string P.(sread G.of_string) epk)
        in
        (partial_decryption_key_of_string (sread P.G.Zq.of_string) epk)
          .pdk_decryption_key
    | None -> (
        basic_check_private_key pk_str;
        try sread P.G.Zq.of_string ++ pk_str
        with e ->
          Printf.ksprintf failwith
            (f_ "Error in format of private key: %s")
            (Printexc.to_string e))
  in
  Lwt.async (fun () ->
      let factor = P.E.compute_factor encrypted_tally private_key in
      set_textarea "pd"
        (string_of_partial_decryption
           P.(swrite G.to_string)
           P.(swrite G.Zq.to_string)
           factor);
      Lwt.return_unit);
  return_unit

let compute_hash () =
  let&$ e = !encrypted_tally in
  let hash = sha256_b64 e in
  let$ e = document##getElementById (Js.string "hash") in
  let t = document##createTextNode (Js.string hash) in
  Dom.appendChild e t

let load_private_key_file _ =
  let&& e = document##getElementById (Js.string "private_key_file") in
  let&& e = Dom_html.CoerceTo.input e in
  let&& e = Js.Opt.option (Js.Optdef.to_option e##.files) in
  let&& file = e##item 0 in
  let reader = new%js File.fileReader in
  reader##.onload :=
    Dom.handler (fun _ ->
        let () =
          let$ e = document##getElementById (Js.string "private_key") in
          let$ e = Dom_html.CoerceTo.input e in
          let$ text = File.CoerceTo.string reader##.result in
          e##.value := text
        in
        Js._false);
  reader##readAsText file;
  return_unit

let fill_interactivity () =
  let@ uuid, token =
   fun cont ->
    let hash = Dom_html.window##.location##.hash |> Js.to_string in
    match extract_uuid_and_token hash with
    | Some (uuid, token) -> cont (uuid, token)
    | None ->
        alert "Unable to extract UUID and token from URL";
        Lwt.return_unit
  in
  let@ () = redirect_if_admin "trustees" uuid token in
  set_form_target "pd_form" "submit-partial-decryption" uuid token;
  let@ tally_trustee cont =
    let url = Printf.sprintf "../api/elections/%s/tally-trustee" uuid in
    let* x = get ~token tally_trustee_of_string url in
    match x with
    | Some x -> cont x
    | None ->
        alert "Error while retrieving tally-trustee";
        Lwt.return_unit
  in
  let () =
    let$ e = document##getElementById (Js.string "compute") in
    let$ e = Dom_html.CoerceTo.button e in
    e##.onclick :=
      Dom_html.handler (wrap (compute_partial_decryption tally_trustee))
  in
  let () =
    let$ e = document##getElementById (Js.string "private_key_file") in
    let$ e = Dom_html.CoerceTo.input e in
    e##.onchange := Dom_html.handler (wrap load_private_key_file)
  in
  let open Js_of_ocaml_lwt.XmlHttpRequest in
  let* e = get ("../elections/" ^ uuid ^ "/encrypted_tally.json") in
  encrypted_tally := Some (String.trim e.content);
  let* e = get ("../elections/" ^ uuid ^ "/election.json") in
  election := Some (String.trim e.content);
  Lwt.return (compute_hash ())

let () =
  Lwt.async (fun () ->
      let* _ = Lwt_js_events.onload () in
      let* () = Belenios_js.I18n.auto_init "admin" in
      fill_interactivity ())
