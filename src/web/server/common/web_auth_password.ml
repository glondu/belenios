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
open Belenios
open Belenios_storage_api
open Belenios_server_core
open Web_common

let does_allow_signups c =
  match List.assoc_opt "allowsignups" c with
  | Some x -> bool_of_string x
  | None -> false

module Channel = struct
  type t = { uuid : uuid option; name : string } [@@warning "-69"]

  let equal = Stdlib.( = )
  let hash = Hashtbl.hash
end

module Throttle = Lwt_throttle.Make (Channel)

module Make
    (Web_services : Web_services_sig.S)
    (Pages_common : Pages_common_sig.S)
    (Web_auth : Web_auth_sig.S) =
struct
  let throttle = Throttle.create ~rate:1 ~max:5 ~n:!Web_config.maxmailsatonce

  let check s uuid a name password =
    let channel = Channel.{ uuid; name } in
    let* b = Throttle.wait throttle channel in
    if b then
      let* r =
        match uuid with
        | None ->
            let@ file cont =
              match List.assoc_opt "db" a.auth_config with
              | None ->
                  Lwt.fail @@ Failure "invalid configuration for admin site"
              | Some x -> cont x
            in
            let key : admin_password_file =
              if is_email name then Address name else Username name
            in
            Storage.get s (Admin_password (file, key))
        | Some uuid -> Storage.get s (Election (uuid, Password name))
      in
      let&* r = Lopt.get_value r in
      if check_password r password then Lwt.return_some r else Lwt.return_none
    else Lwt.return_none

  let handler uuid a =
    let module X = struct
      let pre_login_handler username_or_address ~state =
        let allowsignups = does_allow_signups a.auth_config in
        let site_or_election =
          match uuid with None -> `Site | Some _ -> `Election
        in
        let service = a.auth_instance in
        Pages_common.login_password site_or_election username_or_address
          ~service ~allowsignups ~state
        >>= fun x -> return (Web_auth_sig.Html x)

      let direct s x =
        let fail () = failwith "invalid direct password authentication" in
        match x with
        | `Assoc x -> (
            match
              (List.assoc_opt "username" x, List.assoc_opt "password" x)
            with
            | Some (`String username), Some (`String password) -> (
                let* x = check s uuid a username password in
                match x with
                | Some { username; _ } -> Lwt.return username
                | None -> fail ())
            | _ -> fail ())
        | _ -> fail ()
    end in
    (module X : Web_auth_sig.AUTH_SYSTEM)

  let run_post_login_handler =
    Web_auth.register ~auth_system:"password" { handler; extern = false }

  let password_handler () (state, (name, password)) =
    run_post_login_handler ~state
      {
        Web_auth.post_login_handler =
          (fun uuid a cont ->
            let* result =
              let@ s = Storage.with_transaction in
              let* x = check s uuid a name password in
              (* NB: This is required to finalize the transaction being held *)
              Lwt.return
                (match x with
                | None -> None
                | Some { username; address; _ } ->
                    let info : Belenios_web_api.user_info =
                      {
                        login = username;
                        name = None;
                        address;
                        timestamp = None;
                      }
                    in
                    Some info)
            in
            cont result);
      }

  let () =
    Eliom_registration.Any.register ~service:Web_services.password_post
      password_handler
end

let get_password_db_fname service =
  let rec find = function
    | [] -> None
    | { auth_system = "password"; auth_config = c; auth_instance = i } :: _
      when i = service && does_allow_signups c ->
        List.assoc_opt "db" c
    | _ :: xs -> find xs
  in
  find !Web_config.site_auth_config

let do_add_account s ~db_fname ~username ~password ~email =
  let@ () =
   fun cont ->
    let* r = Storage.get s (Admin_password (db_fname, Username username)) in
    match Lopt.get_value r with
    | None -> cont ()
    | Some _ -> Lwt.return @@ Error UsernameTaken
  in
  let@ () =
   fun cont ->
    let* r = Storage.get s (Admin_password (db_fname, Address email)) in
    match Lopt.get_value r with
    | None -> cont ()
    | Some _ -> Lwt.return @@ Error AddressTaken
  in
  let salt = generate_token ~length:8 () in
  let hashed = sha256_hex (salt ^ password) in
  let r = { username; salt; hashed; address = Some email } in
  Lwt.try_bind
    (fun () ->
      r |> Storage.set s (Admin_password (db_fname, Username username)) Value)
    (fun () -> Lwt.return @@ Ok ())
    (fun _ -> Lwt.return @@ Error DatabaseError)

let do_change_password s ~db_fname ~username ~password =
  let@ r, set =
   fun cont ->
    let fail () =
      Lwt.fail @@ Failure "password record not found in do_change_password"
    in
    let@ r, set =
      Storage.update s (Admin_password (db_fname, Username username))
    in
    match Lopt.get_value r with None -> fail () | Some r -> cont (r, set)
  in
  let salt = generate_token ~length:8 () in
  let hashed = sha256_hex (salt ^ password) in
  let r = { r with salt; hashed } in
  Lwt.try_bind
    (fun () -> set Value r)
    (fun () -> Lwt.return_unit)
    (fun _ -> Lwt.fail @@ Failure "database error")

let add_account user ~password ~email =
  if String.trim password = password then
    if is_username user.user_name then
      let* c = Web_signup.check_password password in
      match c with
      | Some e -> return (Error (BadPassword e))
      | None -> (
          match get_password_db_fname user.user_domain with
          | None ->
              Lwt.fail
                (Failure
                   (Printf.sprintf "add_account: unknown domain: %s"
                      user.user_domain))
          | Some db_fname ->
              let@ s = Storage.with_transaction in
              do_add_account s ~db_fname ~username:user.user_name ~password
                ~email)
    else return (Error BadUsername)
  else return (Error BadSpaceInPassword)

let change_password user ~password =
  if String.trim password = password then
    let* c = Web_signup.check_password password in
    match c with
    | Some e -> return (Error (BadPassword e))
    | None -> (
        match get_password_db_fname user.user_domain with
        | None ->
            Lwt.fail
              (Failure
                 (Printf.sprintf "change_password: unknown domain: %s"
                    user.user_domain))
        | Some db_fname ->
            let* () =
              let@ s = Storage.with_transaction in
              do_change_password s ~db_fname ~username:user.user_name ~password
            in
            return (Ok ()))
  else return (Error BadSpaceInPassword)

let lookup_account ~service ~username ~email =
  let@ s = Storage.with_transaction in
  let&* db_fname = get_password_db_fname service in
  let username = String.trim username in
  let* r = Storage.get s (Admin_password (db_fname, Username username)) in
  match Lopt.get_value r with
  | Some { username; address; _ } -> Lwt.return_some (username, address)
  | None ->
      let address = String.trim email in
      let* r = Storage.get s (Admin_password (db_fname, Address address)) in
      let&* { username; address; _ } = Lopt.get_value r in
      Lwt.return_some (username, address)
