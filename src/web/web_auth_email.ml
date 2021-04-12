(**************************************************************************)
(*                                BELENIOS                                *)
(*                                                                        *)
(*  Copyright © 2012-2021 Inria                                           *)
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

open Lwt
open Lwt.Syntax
open Belenios
open Common
open Web_serializable_builtin_t
open Web_common

let run_post_login_handler =
  Web_auth.register_pre_login_handler ~auth_system:"email"
    (fun _ ~state ->
      Pages_common.login_email ~state >>= (fun x -> return @@ Web_auth.Html x)
    )

type code =
  {
    code : string;
    expiration_time : datetime;
  }

let codes = ref SMap.empty

let filter_codes_by_time table =
  let now = now () in
  SMap.filter (fun _ {expiration_time; _} ->
      datetime_compare now expiration_time <= 0
    ) table

let generate_new_code name =
  let codes_ = filter_codes_by_time !codes in
  let* code = generate_numeric () in
  let expiration_time = datetime_add (now ()) (second 900.) in
  codes := SMap.add name {code; expiration_time} codes_;
  let* subject, body = Pages_common.email_email ~address:name ~code in
  send_email ~subject ~body ~recipient:name MailLogin

let check_code name code =
  let codes_ = filter_codes_by_time !codes in
  codes := codes_;
  match SMap.find_opt name codes_ with
  | None -> false
  | Some x ->
     if x.code = code then (
       codes := SMap.remove name codes_;
       true
     ) else false

let scope = Eliom_common.default_session_scope

let env = Eliom_reference.eref ~scope None

let () =
  Eliom_registration.Any.register ~service:Web_services.email_post
    (fun () (state, name) ->
      if is_email name then (
        let* () = generate_new_code name in
        let* () = Eliom_reference.set env (Some (state, name)) in
        Pages_common.email_login () >>= Eliom_registration.Html.send
      ) else forbidden ()
    )

let () =
  Eliom_registration.Any.register ~service:Web_services.email_login_post
    (fun () code ->
      let* x = Eliom_reference.get env in
      match x with
      | Some (state, name) ->
         if check_code name code then (
           let* () = Eliom_reference.unset env in
           run_post_login_handler ~state
             (fun _ _ authenticate ->
               authenticate name >>= fun x -> return (Ok x))
         ) else forbidden ()
      | None -> forbidden ()
    )
