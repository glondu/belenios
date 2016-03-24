(**************************************************************************)
(*                                BELENIOS                                *)
(*                                                                        *)
(*  Copyright © 2012-2016 Inria                                           *)
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
open Serializable_j
open Tool_js_common

let election = ref None
let encrypted_tally = ref None

let ( >>= ) = Js.Opt.bind

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
      | _ -> failwith "Must start with a double quote"
    else failwith "Too short"
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
  let election = Group.election_params_of_string e in
  let module P = (val election) in
  let module M = Election.MakeSimpleMonad (P.G) in
  let module E = Election.MakeElection (P.G) (M) in
  Js.Opt.option !encrypted_tally >>= fun e ->
  let encrypted_tally = encrypted_tally_of_string P.G.read e in
  document##getElementById (Js.string "private_key") >>= fun e ->
  Dom_html.CoerceTo.input e >>= fun e ->
  let pk_str = Js.to_string e##value in
  basic_check_private_key pk_str;
  let private_key =
    try number_of_string pk_str
    with e ->
      Printf.ksprintf
        failwith "Error in format of private key: %s" (Printexc.to_string e)
  in
  let factor = E.compute_factor encrypted_tally private_key () in
  set_textarea "pd" (string_of_partial_decryption P.G.write factor);
  Js.some ()

let compute_hash () =
  let _ =
    Js.Opt.option !encrypted_tally >>= fun e ->
    let hash = sha256_b64 e in
    document##getElementById (Js.string "hash") >>= fun e ->
    let t = document##createTextNode (Js.string hash) in
    Dom.appendChild e t;
    Js.null
  in Js._false

let main _ =
  let _ =
    document##getElementById (Js.string "compute") >>= fun e ->
    Dom_html.CoerceTo.button e >>= fun e ->
    e##onclick <- Dom_html.handler (wrap compute_partial_decryption);
    Js.null
  in
  let _ =
    Lwt.async (fun () ->
      let open XmlHttpRequest in
      lwt e = get "../encrypted_tally.json" in
      encrypted_tally := Some e.content;
      lwt e = get "../election.json" in
      election := Some e.content;
      Lwt.return (compute_hash ()))
  in
  Js._false

let () =
  Dom_html.window##onload <- Dom_html.handler main
