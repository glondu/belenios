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
open Belenios_core
open Common
open Web_common

type captcha = {
    content_type : string;
    contents : string;
    response : string;
    c_expiration_time : Datetime.t;
  }

let captchas = ref SMap.empty

let filter_captchas_by_time table =
  let now = Datetime.now () in
  SMap.filter (fun _ {c_expiration_time; _} ->
      Datetime.compare now c_expiration_time <= 0
    ) table

let format_content_type = function
  | "png" -> "image/png"
  | x -> Printf.ksprintf failwith "Unknown captcha type: %s" x

let captcha =
  let x = "belenios-captcha" in (x, [| x |])

let create_captcha () =
  let* raw = Lwt_process.pread_lines captcha |> Lwt_stream.to_list in
  match raw with
  | content_type :: response :: contents ->
     let content_type = format_content_type content_type in
     let contents =
       let open Cryptokit in
       String.concat "\n" contents |> transform_string (Base64.decode ())
     in
     let challenge = sha256_b64 contents in
     let c_expiration_time = Period.add (Datetime.now ()) (Period.second 300) in
     let x = { content_type; contents; response; c_expiration_time } in
     captchas := SMap.add challenge x !captchas;
     Lwt.return challenge
  | _ ->
     Lwt.fail (Failure "Captcha generation failed")

let get challenge =
  captchas := filter_captchas_by_time !captchas;
  SMap.find_opt challenge !captchas

let get_captcha ~challenge =
  match get challenge with
  | None -> fail_http `Not_found
  | Some {content_type; contents; _} -> Lwt.return (contents, content_type)

let check_captcha ~challenge ~response =
  match get challenge with
  | None -> Lwt.return false
  | Some x ->
     captchas := SMap.remove challenge !captchas;
     Lwt.return (response = x.response)

module Make (Web_services : Web_services_sig.S) = struct

  let () =
    Eliom_registration.String.register
      ~service:Web_services.signup_captcha_img
      (fun challenge () -> get_captcha ~challenge)

end
