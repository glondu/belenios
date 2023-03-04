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

open Lwt
open Lwt.Syntax
open Belenios_platform
open Belenios_core.Common
open Web_serializable_j
open Platform
open Web_common

let does_allow_signups c =
  match List.assoc_opt "allowsignups" c with
  | Some x -> bool_of_string x
  | None -> false

module Make (Web_services : Web_services_sig.S) (Pages_common : Pages_common_sig.S) (Web_auth : Web_auth_sig.S) = struct

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
    | Some (u :: salt :: hashed :: xs) ->
       if sha256_hex (salt ^ String.trim password) = hashed then (
         let email = match xs with [] -> "" | x :: _ -> x in
         return_some (u, email)
       ) else
         return_none
    | _ -> return_none

  let check uuid a name password =
    match uuid with
    | None ->
       begin
         match List.assoc_opt "db" a.auth_config with
         | Some db -> check_password_with_file db name password
         | _ -> failwith "invalid configuration for admin site"
       end
    | Some uuid ->
       let db = Web_persist.get_password_filename uuid in
       check_password_with_file db name password

  let auth_system uuid a =
    let module X =
      struct
        let pre_login_handler username_or_address ~state =
          let allowsignups = does_allow_signups a.auth_config in
          let site_or_election =
            match uuid with
            | None -> `Site
            | Some _ -> `Election
          in
          let service = a.auth_instance in
          Pages_common.login_password site_or_election username_or_address ~service ~allowsignups ~state
          >>= (fun x -> return @@ Web_auth_sig.Html x)

        let direct x =
          let fail () = failwith "invalid direct password authentication" in
          match x with
          | `Assoc x ->
             begin
               match List.assoc_opt "username" x, List.assoc_opt "password" x with
               | Some (`String username), Some (`String password) ->
                  begin
                    let* x = check uuid a username password in
                    match x with
                    | Some (username, _) -> Lwt.return username
                    | None -> fail ()
                  end
               | _ -> fail ()
             end
          | _ -> fail ()
      end
    in
    (module X : Web_auth_sig.AUTH_SYSTEM)

  let run_post_login_handler =
    Web_auth.register ~auth_system:"password" auth_system

  let password_handler () (state, (name, password)) =
    run_post_login_handler ~state
      {
        Web_auth.post_login_handler =
          fun uuid a cont ->
          let* ok = check uuid a name password in
          cont ok
      }

  let () = Eliom_registration.Any.register ~service:Web_services.password_post password_handler

end

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
  let salt = generate_token ~length:8 () in
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
  let salt = generate_token ~length:8 () in
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

let add_account user ~password ~email =
  if String.trim password = password then (
    if is_username user.user_name then
      let* c = Web_signup.cracklib_check password in
      match c with
      | Some e -> return (Error (BadPassword e))
      | None ->
         match get_password_db_fname user.user_domain with
         | None -> Lwt.fail (Failure (Printf.sprintf "add_account: unknown domain: %s" user.user_domain))
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
       | None -> Lwt.fail (Failure (Printf.sprintf "change_password: unknown domain: %s" user.user_domain))
       | Some db_fname ->
          let* () =
            Lwt_mutex.with_lock password_db_mutex
              (do_change_password ~db_fname ~username:user.user_name ~password)
          in return (Ok ())
  ) else return (Error BadSpaceInPassword)

let lookup_account ~service ~username ~email =
  let username = String.trim username |> String.lowercase_ascii in
  let email = email |> String.lowercase_ascii in
  let&* db = get_password_db_fname service in
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
