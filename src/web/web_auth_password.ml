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

open Lwt
open Serializable_builtin_t
open Web_serializable_j
open Platform
open Web_common

let ( / ) = Filename.concat

let check_password_with_file db name password =
  let%lwt db = Lwt_preemptive.detach Csv.load db in
  match
    List.find_opt (function
        | username :: _ :: _ :: _ -> username = name
        | _ -> false
      ) db
  with
  | Some (_ :: salt :: hashed :: _) ->
     return (sha256_hex (salt ^ password) = hashed)
  | _ -> return false

let password_handler () (name, password) =
  Web_auth.run_post_login_handler "password" (fun uuid config authenticate ->
      let%lwt ok =
        match uuid with
        | None ->
           begin
             match List.assoc_opt "db" config with
             | Some db -> check_password_with_file db name password
             | _ -> failwith "invalid configuration for admin site"
           end
        | Some uuid ->
           let uuid_s = raw_string_of_uuid uuid in
           let db = !Web_config.spool_dir / uuid_s / "passwords.csv" in
           check_password_with_file db name password
      in
      if ok then authenticate name else fail_http 401
    )

let () = Eliom_registration.Any.register ~service:Web_services.password_post password_handler

let does_allow_signups c =
  match List.assoc_opt "allowsignups" c with
  | Some x -> bool_of_string x
  | None -> false

let get_password_db_fname () =
  let rec find = function
    | [] -> None
    | { auth_system = "password"; auth_config = c; _ } :: _
         when does_allow_signups c -> List.assoc_opt "db" c
    | _ :: xs -> find xs
  in find !Web_config.site_auth_config

let allowsignups () = get_password_db_fname () <> None

let password_db_mutex = Lwt_mutex.create ()

let do_add_account ~db_fname ~username ~password ~email () =
  let%lwt db = Lwt_preemptive.detach Csv.load db_fname in
  let%lwt salt = generate_token ~length:8 () in
  let hashed = sha256_hex (salt ^ password) in
  let rec append accu = function
    | [] -> Ok (List.rev ([username; salt; hashed; email] :: accu))
    | (u :: _ :: _ :: _) :: _ when u = username -> Pervasives.Error UsernameTaken
    | (_ :: _ :: _ :: e :: _) :: _ when e = email -> Pervasives.Error AddressTaken
    | x :: xs -> append (x :: accu) xs
  in
  match append [] db with
  | Pervasives.Error _ as x -> Lwt.return x
  | Ok db ->
     let db = List.map (String.concat ",") db in
     let%lwt () = write_file db_fname db in
     Lwt.return (Ok ())

let do_change_password ~db_fname ~username ~password () =
  let%lwt db = Lwt_preemptive.detach Csv.load db_fname in
  let%lwt salt = generate_token ~length:8 () in
  let hashed = sha256_hex (salt ^ password) in
  let rec change accu = function
    | [] -> accu
    | (u :: _ :: _ :: x) :: xs when u = username ->
       change ((u :: salt :: hashed :: x) :: accu) xs
    | x :: xs -> change (x :: accu) xs
  in
  let db = List.rev_map (String.concat ",") (change [] db) in
  let%lwt () = write_file db_fname db in
  return_unit

let username_rex = "^[A-Z0-9._%+-]+$"

let is_username =
  let rex = Pcre.regexp ~flags:[`CASELESS] username_rex in
  fun x ->
  match pcre_exec_opt ~rex x with
  | Some _ -> true
  | None -> false

let add_account ~username ~password ~email =
  if is_username username then
    match%lwt Web_signup.cracklib_check password with
    | Some e -> return (Pervasives.Error (BadPassword e))
    | None ->
       match get_password_db_fname () with
       | None -> forbidden ()
       | Some db_fname ->
          Lwt_mutex.with_lock password_db_mutex
            (do_add_account ~db_fname ~username ~password ~email)
  else return (Pervasives.Error BadUsername)

let change_password ~username ~password =
  match%lwt Web_signup.cracklib_check password with
  | Some e -> return (Pervasives.Error e)
  | None ->
     match get_password_db_fname () with
     | None -> forbidden ()
     | Some db_fname ->
        let%lwt () =
          Lwt_mutex.with_lock password_db_mutex
            (do_change_password ~db_fname ~username ~password)
        in return (Ok ())

let () =
  Web_auth.register_pre_login_handler "password"
    (fun config ->
      let allowsignups = does_allow_signups config in
      Web_templates.login_password ~allowsignups >>= Eliom_registration.Html.send
    )

let lookup_account ~username ~email =
  match get_password_db_fname () with
  | None -> return None
  | Some db ->
     let%lwt db = Lwt_preemptive.detach Csv.load db in
     match
       List.find_opt (function
           | u :: _ :: _ :: _ when u = username -> true
           | _ :: _ :: _ :: e :: _ when e = email -> true
           | _ -> false
         ) db
     with
     | Some (u :: _ :: _ :: e :: _) when is_email e -> return (Some (u, e))
     | _ -> return None
