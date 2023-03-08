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

open Lwt.Syntax
open Lwt.Infix
open Belenios_core.Common
open Web_common
open Web_serializable_j

module UMap = Map.Make (struct type t = user let compare = compare end)

let cache = ref None

let counter_mutex = Lwt_mutex.create ()
let account_mutex = Lwt_mutex.create ()
let cache_mutex = Lwt_mutex.create ()

let clear_account_cache () =
  let@ () = Lwt_mutex.with_lock cache_mutex in
  cache := None;
  Lwt.return_unit

let account_of_filename filename =
  let&* id = Filename.chop_suffix_opt ~suffix:".json" filename in
  let&* _ = int_of_string_opt id in
  let* contents = Filesystem.read_file (!Web_config.accounts_dir // filename) in
  match contents with
  | Some [x] -> Lwt.return (try Some (account_of_string x) with _ -> None)
  | _ -> Lwt.return_none

let get_account_by_id id =
  account_of_filename (Printf.sprintf "%d.json" id)

let update_hooks = ref []

let add_update_hook f = update_hooks := f :: !update_hooks

let update_account account =
  let* () =
    let@ () = Lwt_mutex.with_lock account_mutex in
    Filesystem.write_file
      (!Web_config.accounts_dir // Printf.sprintf "%d.json" account.id)
      [string_of_account account]
  in
  Lwt_list.iter_s (fun f -> f account) !update_hooks

let drop_after_at x =
  match String.index_opt x '@' with
  | None -> x
  | Some i -> String.sub x 0 i

let create_account ~email user =
  let@ () = Lwt_mutex.with_lock counter_mutex in
  let* counter =
    let* x = Filesystem.read_file (!Web_config.accounts_dir // "counter") in
    match x with
    | Some [x] ->
       Lwt.return (match int_of_string_opt x with None -> 1 | Some x -> x)
    | _ -> Lwt.return 1
  in
  let rec find_free_id n =
    let* x = get_account_by_id n in
    match x with
    | None -> Lwt.return n
    | Some _ -> find_free_id (n + 1)
  in
  let* id = find_free_id counter in
  let last_connected = Datetime.now () in
  let name =
    let x = drop_after_at user.user_name in
    if x = "" then Printf.sprintf "User #%d" id else x
  in
  let account =
    {
      id;
      name;
      email = email;
      last_connected;
      authentications = [user];
      consent = None;
      capabilities = None;
      language = None;
      default_voter_languages = [];
      default_contact = "";
    }
  in
  let* () = update_account account in
  let* () = Filesystem.write_file (!Web_config.accounts_dir // "counter") [string_of_int (id + 1)] in
  let* () = clear_account_cache () in
  Lwt.return account

let build_account_cache () =
  Lwt_unix.files_of_directory !Web_config.accounts_dir
  |> Lwt_stream.to_list
  >>= Lwt_list.fold_left_s
        (fun accu f ->
          let* account = account_of_filename f in
          match account with
          | None -> Lwt.return accu
          | Some account ->
             List.fold_left
               (fun accu u -> UMap.add u account.id accu)
               accu account.authentications
             |> Lwt.return
        ) UMap.empty

let get_account user =
  let* cache =
    match !cache with
    | Some x -> Lwt.return x
    | None ->
       let@ () = Lwt_mutex.with_lock cache_mutex in
       let* x = build_account_cache () in
       cache := Some x;
       Lwt.return x
  in
  let&* id = UMap.find_opt user cache in
  get_account_by_id id

type capability =
  | Sudo

let mask_of_capability = function
  | Sudo -> 1

let has_capability cap account =
  match account.capabilities with
  | None -> false
  | Some i -> i land (mask_of_capability cap) <> 0

let check a i = List.mem a.id i
