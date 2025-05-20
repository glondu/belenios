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
open Belenios

module Sender = struct
  type payload = Web_state_sig.signup_env

  type context = {
    kind : Web_state_sig.signup_kind;
    gettext : (module Belenios_ui.I18n.GETTEXT);
  }

  let send ~context ~recipient ~code =
    let open (val context.gettext) in
    match context.kind with
    | CreateAccount ->
        Send_message.send @@ Account_create { lang; recipient; code }
    | ChangePassword _ ->
        Send_message.send @@ Account_change_password { lang; recipient; code }
end

module Otp = Otp.Make (Sender) ()

let send_confirmation_code gettext ~service ~recipient =
  let kind = Web_state_sig.CreateAccount in
  let payload = Web_state_sig.{ kind; service } in
  let context = Sender.{ kind; gettext } in
  Otp.generate ~payload ~context ~recipient

let send_changepw_code gettext ~service ~recipient =
  let kind = Web_state_sig.ChangePassword { username = fst recipient } in
  let payload = Web_state_sig.{ kind; service } in
  let context = Sender.{ kind; gettext } in
  Otp.generate ~payload ~context ~recipient

let confirm_code = Otp.check

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
