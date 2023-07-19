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

type link_kind = [ `CreateAccount | `ChangePassword of string ]

type link = {
  service : string;
  code : string;
  l_expiration_time : Datetime.t;
  kind : link_kind;
}

let links = ref SMap.empty

let filter_links_by_time table =
  let now = Datetime.now () in
  SMap.filter
    (fun _ { l_expiration_time; _ } ->
      Datetime.compare now l_expiration_time <= 0)
    table

let send_confirmation_link l ~service address =
  let code = generate_numeric () in
  let l_expiration_time = Period.add (Datetime.now ()) (Period.second 900) in
  let kind = `CreateAccount in
  let link = { service; code; l_expiration_time; kind } in
  let nlinks = filter_links_by_time !links in
  links := SMap.add address link nlinks;
  let subject, body = Pages_admin.mail_confirmation_link l address code in
  let* () = send_email MailAccountCreation ~recipient:address ~subject ~body in
  Lwt.return_unit

let send_changepw_link l ~service ~address ~username =
  let code = generate_numeric () in
  let l_expiration_time = Period.add (Datetime.now ()) (Period.second 900) in
  let kind = `ChangePassword username in
  let link = { service; code; l_expiration_time; kind } in
  let nlinks = filter_links_by_time !links in
  links := SMap.add address link nlinks;
  let subject, body = Pages_admin.mail_changepw_link l address code in
  let* () = send_email MailPasswordChange ~recipient:address ~subject ~body in
  Lwt.return_unit

let confirm_link address =
  links := filter_links_by_time !links;
  let&* x = SMap.find_opt address !links in
  Lwt.return_some (x.code, x.service, x.kind)

let remove_link address =
  links := filter_links_by_time !links;
  links := SMap.remove address !links;
  Lwt.return_unit

let cracklib =
  let x = "cracklib-check" in
  (x, [| x |])

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
      let* x = Lwt_process.pmap ~env:[| "LANG=C" |] cracklib password in
      Lwt.return (extract_comment x)
  | Some _ -> Lwt.return_some "newline in password"

let is_lower = function 'a' .. 'z' -> true | _ -> false
let is_upper = function 'A' .. 'Z' -> true | _ -> false
let is_digit = function '0' .. '9' -> true | _ -> false

let is_special c =
  let i = int_of_char c in
  (32 < i && i < 48)
  || (57 < i && i < 65)
  || (90 < i && i < 97)
  || (122 < i && i < 127)

let complexity_check password =
  if String.length password < 12 then Some "less than 12 characters"
  else if not (String.exists is_lower password) then Some "no lowercase letter"
  else if not (String.exists is_upper password) then Some "no uppercase letter"
  else if not (String.exists is_digit password) then Some "no digit"
  else if not (String.exists is_special password) then
    Some "no special character"
  else None

let check_password password =
  match complexity_check password with
  | Some x -> Lwt.return_some x
  | None -> cracklib_check password
