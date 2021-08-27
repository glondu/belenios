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

open Lwt.Syntax
open Lwt.Infix
open Belenios_core.Common
open Web_serializable_builtin_t
open Web_common
open Web_serializable_j

let ( / ) = Filename.concat

let ( let& ) x f =
  match x with
  | None -> Lwt.return_none
  | Some x -> f x

let counter_mutex = Lwt_mutex.create ()
let account_mutex = Lwt_mutex.create ()

let account_of_filename filename =
  let& id = Filename.chop_suffix_opt ~suffix:".json" filename in
  let& _ = int_of_string_opt id in
  let* contents = read_file (!Web_config.accounts_dir / filename) in
  match contents with
  | Some [x] -> Lwt.return (try Some (account_of_string x) with _ -> None)
  | _ -> Lwt.return_none

let update_account account =
  let@ () = Lwt_mutex.with_lock account_mutex in
  write_file
    (!Web_config.accounts_dir / Printf.sprintf "%d.json" account.account_id)
    [string_of_account account]

let create_account ~email user =
  let@ () = Lwt_mutex.with_lock counter_mutex in
  let* counter =
    let* x = read_file (!Web_config.accounts_dir / "counter") in
    match x with
    | Some [x] ->
       Lwt.return (match int_of_string_opt x with None -> 1 | Some x -> x)
    | _ -> Lwt.return 1
  in
  let rec find_free_id n =
    let* x = account_of_filename (Printf.sprintf "%d.json" n) in
    match x with
    | None -> Lwt.return n
    | Some _ -> find_free_id (n + 1)
  in
  let* account_id = find_free_id counter in
  let account_last_connected = now () in
  let account =
    {
      account_id;
      account_name = Printf.sprintf "User #%d" account_id;
      account_email = email;
      account_last_connected;
      account_authentications = [user];
      account_consent = None;
    }
  in
  let* () = update_account account in
  let* () = write_file (!Web_config.accounts_dir / "counter") [string_of_int (account_id + 1)] in
  Lwt.return account

let get_account user =
  Lwt_unix.files_of_directory !Web_config.accounts_dir
  |> Lwt_stream.to_list
  >>= fun files ->
  let rec loop = function
    | [] -> Lwt.return_none
    | f :: fs ->
       let* account = account_of_filename f in
       match account with
       | None -> loop fs
       | Some account ->
          if List.mem user account.account_authentications then
            Lwt.return_some account
          else loop fs
  in
  loop files
