(**************************************************************************)
(*                                BELENIOS                                *)
(*                                                                        *)
(*  Copyright © 2012-2018 Inria                                           *)
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
open Platform
open Serializable_j
open Common
open Tool_js_common

let election = ref None
let encrypted_tally = ref None

let wrap f x =
  (try
     Js.Opt.case (f x)
       (fun () -> failwith "Unexpected error")
       (fun () -> ())
   with
   | Failure s -> alert s
   | e ->
      Printf.ksprintf
        alert "Unexpected error: %s" (Printexc.to_string e)
  ); Js._false

let basic_check_private_key s =
  let n = String.length s in
  let rec leading i =
    if i < n then
      match s.[i] with
      | '"' -> middle (i+1)
      | _ -> failwith "Must start with a double quote (character given was another character)"
    else failwith "Must start with a double quote (empty string given)"
  and middle i =
    if i < n then
      match s.[i] with
      | '0'..'9' -> ending (i+1)
      | _ -> failwith "Must have at least one digit"
    else failwith "Too short"
  and ending i =
    if i < n then
      match s.[i] with
      | '0'..'9' -> ending (i+1)
      | '"' -> (if i+1 < n then failwith "Must end with a double quote")
      | c -> Printf.ksprintf failwith "Illegal character: %c" c
    else failwith "Must end with a double quote"
  in leading 0

let compute_partial_decryption _ =
  Js.Opt.option !election >>= fun e ->
  let election = Election.(get_group (of_string e)) in
  let module P = (val election) in
  let module E = Election.Make (P) (DirectRandom) in
  Js.Opt.option !encrypted_tally >>= fun e ->
  let encrypted_tally = encrypted_tally_of_string P.G.read e in
  document##getElementById (Js.string "private_key") >>= fun e ->
  Dom_html.CoerceTo.input e >>= fun e ->
  let pk_str = Js.to_string e##.value in
  let private_key =
    match get_textarea_opt "encrypted_private_key" with
    | Some epk ->
       let module PKI = Trustees.MakePKI (P.G) (DirectRandom) in
       let module C = Trustees.MakeChannels (P.G) (DirectRandom) (PKI) in
       let sk = PKI.derive_sk pk_str and dk = PKI.derive_dk pk_str in
       let vk = P.G.(g **~ sk) in
       let epk = C.recv dk vk epk in
       (partial_decryption_key_of_string epk).pdk_decryption_key
    | None ->
       basic_check_private_key pk_str;
       try number_of_string pk_str with
       | e ->
         Printf.ksprintf
           failwith "Error in format of private key: %s" (Printexc.to_string e)
  in
  let factor = E.compute_factor encrypted_tally private_key in
  set_textarea "pd" (string_of_partial_decryption P.G.write factor);
  return_unit

let compute_hash () =
  let () =
    Js.Opt.option !encrypted_tally >>== fun e ->
    let hash = sha256_b64 e in
    document##getElementById (Js.string "hash") >>== fun e ->
    let t = document##createTextNode (Js.string hash) in
    Dom.appendChild e t
  in Js._false

let load_private_key_file _ =
  document##getElementById (Js.string "private_key_file") >>= fun e ->
  Dom_html.CoerceTo.input e >>= fun e ->
  Js.Opt.option (Js.Optdef.to_option (e##.files)) >>= fun e ->
  e##item (0) >>= fun file ->
  let reader = new%js File.fileReader in
  reader##.onload :=
    Dom.handler (fun _ ->
        let () =
          document##getElementById (Js.string "private_key") >>== fun e ->
          Dom_html.CoerceTo.input e >>== fun e ->
          File.CoerceTo.string (reader##.result) >>== fun text ->
          e##.value := text
        in Js._false
      );
  reader##readAsText (file);
  return_unit

let main _ =
  let () =
    document##getElementById (Js.string "compute") >>== fun e ->
    Dom_html.CoerceTo.button e >>== fun e ->
    e##.onclick := Dom_html.handler (wrap compute_partial_decryption)
  in
  let () =
    document##getElementById (Js.string "private_key_file") >>== fun e ->
    Dom_html.CoerceTo.input e >>== fun e ->
    e##.onchange := Dom_html.handler (wrap load_private_key_file)
  in
  let () =
    match List.assoc_opt "uuid" (get_params ()) with
    | None -> ()
    | Some uuid ->
       Lwt.async (fun () ->
           let open Js_of_ocaml_lwt.XmlHttpRequest in
           let%lwt e = get ("../elections/" ^ uuid ^ "/encrypted_tally.json") in
           encrypted_tally := Some (String.trim e.content);
           let%lwt e = get ("../elections/" ^ uuid ^ "/election.json") in
           election := Some e.content;
           Lwt.return (compute_hash ())
         )
  in Js._false

let () =
  Dom_html.window##.onload := Dom_html.handler main
