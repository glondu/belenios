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

open Platform
open Common
open Web_serializable_builtin_t
open Web_common

type captcha = {
    content_type : string;
    contents : string;
    response : string;
    c_expiration_time : datetime;
  }

let captchas = ref SMap.empty

let filter_captchas_by_time table =
  let now = now () in
  SMap.filter (fun _ {c_expiration_time; _} ->
      datetime_compare now c_expiration_time <= 0
    ) table

let format_content_type = function
  | "png" -> "image/png"
  | x -> Printf.ksprintf failwith "Unknown captcha type: %s" x

let captcha =
  let x = "./ext/captcha/captcha" in (x, [| x |])

let create_captcha () =
  let%lwt raw = Lwt_process.pread_lines captcha |> Lwt_stream.to_list in
  match raw with
  | content_type :: response :: contents ->
     let content_type = format_content_type content_type in
     let contents =
       let open Cryptokit in
       String.concat "\n" contents |> transform_string (Base64.decode ())
     in
     let challenge = sha256_b64 contents in
     let c_expiration_time = datetime_add (now ()) (second 300.) in
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
  | None -> fail_http 404
  | Some {content_type; contents; _} -> Lwt.return (contents, content_type)

let check_captcha ~challenge ~response =
  match get challenge with
  | None -> Lwt.return false
  | Some x ->
     captchas := SMap.remove challenge !captchas;
     Lwt.return (response = x.response)

type link_kind =
  | CreateAccount
  | ChangePassword of string

type link = {
    service : string;
    address : string;
    l_expiration_time : datetime;
    kind : link_kind;
}

let links = ref SMap.empty

let filter_links_by_time table =
  let now = now () in
  SMap.filter (fun _ {l_expiration_time; _} ->
      datetime_compare now l_expiration_time <= 0
    ) table

let filter_links_by_address address table =
  SMap.filter (fun _ x -> x.address = address) table

let send_confirmation_link ~service address =
  let%lwt token = generate_token ~length:20 () in
  let l_expiration_time = datetime_add (now ()) (day 1) in
  let kind = CreateAccount in
  let link = {service; address; l_expiration_time; kind} in
  let nlinks = filter_links_by_time (filter_links_by_address address !links) in
  links := SMap.add token link nlinks;
  let uri =
    Eliom_uri.make_string_uri ~absolute:true ~service:Web_services.signup_login
      token |> rewrite_prefix
  in
  let message =
    Printf.sprintf "\
Dear %s,

Your e-mail address has been used to create an account on our Belenios
server. To confirm this creation, please click on the following link:

  %s

or copy and paste it in a web browser.

Warning: this link is valid for 1 day, and previous links sent to this
address are no longer valid.

Best regards,

-- \n\
Belenios Server" address uri
  in
  let%lwt () = send_email address "Belenios account creation" message in
  Lwt.return_unit

let send_changepw_link ~service ~address ~username =
  let%lwt token = generate_token ~length:20 () in
  let l_expiration_time = datetime_add (now ()) (day 1) in
  let kind = ChangePassword username in
  let link = {service; address; l_expiration_time; kind} in
  let nlinks = filter_links_by_time (filter_links_by_address address !links) in
  links := SMap.add token link nlinks;
  let uri =
    Eliom_uri.make_string_uri ~absolute:true ~service:Web_services.signup_login
      token |> rewrite_prefix
  in
  let message =
    Printf.sprintf "\
Dear %s,

There has been a request to change the password of your account on our
Belenios server. To confirm this, please click on the following link:

  %s

or copy and paste it in a web browser.

Warning: this link is valid for 1 day, and previous links sent to this
address are no longer valid.

Best regards,

-- \n\
Belenios Server" username uri
  in
  let%lwt () = send_email address "Belenios password change" message in
  Lwt.return_unit

let confirm_link token =
  links := filter_links_by_time !links;
  match SMap.find_opt token !links with
  | None -> Lwt.return_none
  | Some x -> Lwt.return_some (token, x.service, x.address, x.kind)

let remove_link token =
  links := filter_links_by_time !links;
  links := SMap.remove token !links;
  Lwt.return_unit

let cracklib =
  let x = "cracklib-check" in (x, [| x |])

let extract_comment x =
  let n = String.length x in
  match String.rindex_opt x ':' with
  | Some i when i < n - 2 ->
     let x = String.sub x (i + 2) (n - i - 3) in
     if x = "OK" then None else Some x
  | _ -> Some "unknown error"

let cracklib_check password =
  match String.index_opt password '\n' with
  | None ->
     let%lwt x = Lwt_process.pmap ~env:[| "LANG=C" |] cracklib password in
     Lwt.return (extract_comment x)
  | Some _ -> Lwt.return_some "newline in password"
