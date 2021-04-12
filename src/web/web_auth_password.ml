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
open Belenios_platform
open Belenios
open Serializable_builtin_t
open Web_serializable_j
open Platform
open Web_common

let ( / ) = Filename.concat

let check_password_with_file db name_or_email password =
  let name_or_email = String.trim name_or_email |> String.lowercase_ascii in
  let check_name_or_email =
    if is_email name_or_email then
      function
      | u :: _ :: _ :: _ when String.lowercase_ascii u = name_or_email ->
         (* When authenticating as a voter, the username may be an email *)
         true
      | _ :: _ :: _ :: e :: _ when String.lowercase_ascii e = name_or_email ->
         (* When authenticating as an admin, email is 4th CSV field *)
         true
      | _ -> false
    else
      function
      | u :: _ :: _ :: _ when String.lowercase_ascii u = name_or_email -> true
      | _ -> false
  in
  let* db = Lwt_preemptive.detach Csv.load db in
  match List.find_opt check_name_or_email db with
  | Some (u :: salt :: hashed :: _) ->
     if sha256_hex (salt ^ String.trim password) = hashed then
       return_some u
     else
       return_none
  | _ -> return_none

let does_allow_signups c =
  match List.assoc_opt "allowsignups" c with
  | Some x -> bool_of_string x
  | None -> false

let run_post_login_handler =
  Web_auth.register_pre_login_handler ~auth_system:"password"
    (fun { auth_config; auth_instance = service; _ } ~state ->
      let allowsignups = does_allow_signups auth_config in
      Pages_common.login_password ~service ~allowsignups ~state
      >>= (fun x -> return @@ Web_auth.Html x)
    )

let password_handler () (state, (name, password)) =
  run_post_login_handler ~state
    {
      Web_auth.post_login_handler =
        fun uuid a authenticate fail ->
        let* ok =
          match uuid with
          | None ->
             begin
               match List.assoc_opt "db" a.auth_config with
               | Some db -> check_password_with_file db name password
               | _ -> failwith "invalid configuration for admin site"
             end
          | Some uuid ->
             let uuid_s = raw_string_of_uuid uuid in
             let db = !Web_config.spool_dir / uuid_s / "passwords.csv" in
             check_password_with_file db name password
        in
        match ok with
        | Some name -> authenticate name
        | None -> fail ()
    }

let () = Eliom_registration.Any.register ~service:Web_services.password_post password_handler

let get_password_db_fname service =
  let rec find = function
    | [] -> None
    | { auth_system = "password"; auth_config = c; auth_instance = i } :: _
         when i = service && does_allow_signups c -> List.assoc_opt "db" c
    | _ :: xs -> find xs
  in find !Web_config.site_auth_config

let password_db_mutex = Lwt_mutex.create ()

let do_add_account ~db_fname ~username ~password ~email () =
  let username_ = String.lowercase_ascii username in
  let email_ = String.lowercase_ascii email in
  let* db = Lwt_preemptive.detach Csv.load db_fname in
  let* salt = generate_token ~length:8 () in
  let hashed = sha256_hex (salt ^ password) in
  let rec append accu = function
    | [] -> Ok (List.rev ([username; salt; hashed; email] :: accu))
    | (u :: _ :: _ :: _) :: _ when String.lowercase_ascii u = username_ -> Error UsernameTaken
    | (_ :: _ :: _ :: e :: _) :: _ when String.lowercase_ascii e = email_ -> Error AddressTaken
    | x :: xs -> append (x :: accu) xs
  in
  match append [] db with
  | Error _ as x -> Lwt.return x
  | Ok db ->
     let db = List.map (String.concat ",") db in
     let* () = write_file db_fname db in
     Lwt.return (Ok ())

let do_change_password ~db_fname ~username ~password () =
  let username = String.lowercase_ascii username in
  let* db = Lwt_preemptive.detach Csv.load db_fname in
  let* salt = generate_token ~length:8 () in
  let hashed = sha256_hex (salt ^ password) in
  let rec change accu = function
    | [] -> accu
    | (u :: _ :: _ :: x) :: xs when String.lowercase_ascii u = username ->
       change ((u :: salt :: hashed :: x) :: accu) xs
    | x :: xs -> change (x :: accu) xs
  in
  let db = List.rev_map (String.concat ",") (change [] db) in
  let* () = write_file db_fname db in
  return_unit

let username_rex = "^[A-Z0-9._%+-]+$"

let is_username =
  let rex = Pcre.regexp ~flags:[`CASELESS] username_rex in
  fun x ->
  match pcre_exec_opt ~rex x with
  | Some _ -> true
  | None -> false

let add_account user ~password ~email =
  if String.trim password = password then (
    if is_username user.user_name then
      let* c = Web_signup.cracklib_check password in
      match c with
      | Some e -> return (Error (BadPassword e))
      | None ->
         match get_password_db_fname user.user_domain with
         | None -> forbidden ()
         | Some db_fname ->
            Lwt_mutex.with_lock password_db_mutex
              (do_add_account ~db_fname ~username:user.user_name ~password ~email)
    else return (Error BadUsername)
  ) else return (Error BadSpaceInPassword)

let change_password user ~password =
  if String.trim password = password then (
    let* c = Web_signup.cracklib_check password in
    match c with
    | Some e -> return (Error (BadPassword e))
    | None ->
       match get_password_db_fname user.user_domain with
       | None -> forbidden ()
       | Some db_fname ->
          let* () =
            Lwt_mutex.with_lock password_db_mutex
              (do_change_password ~db_fname ~username:user.user_name ~password)
          in return (Ok ())
  ) else return (Error BadSpaceInPassword)

let lookup_account ~service ~username ~email =
  let username = String.trim username |> String.lowercase_ascii in
  let email = email |> String.lowercase_ascii in
  match get_password_db_fname service with
  | None -> return_none
  | Some db ->
     let* db = Lwt_preemptive.detach Csv.load db in
     match
       List.find_opt (function
           | u :: _ :: _ :: _ when String.lowercase_ascii u = username -> true
           | _ :: _ :: _ :: e :: _ when String.lowercase_ascii e = email -> true
           | _ -> false
         ) db
     with
     | Some (u :: _ :: _ :: e :: _) when is_email e -> return_some (u, e)
     | _ -> return_none
