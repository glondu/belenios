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
open Belenios
open Core
open Web_serializable_j

module UMap = Map.Make (struct
  type t = user

  let compare = compare
end)

let cache = ref None
let cache_mutex = Lwt_mutex.create ()

let clear_account_cache () =
  let@ () = Lwt_mutex.with_lock cache_mutex in
  cache := None;
  Lwt.return_unit

let update_hooks = ref []
let add_update_hook f = update_hooks := f :: !update_hooks

let run_update_hooks account =
  Lwt_list.iter_s (fun f -> f account) !update_hooks

let get_account_by_id s id =
  let module S = (val s : Storage_sig.BACKEND) in
  let* x = S.get (Account id) in
  match x with
  | None -> Lwt.return_none
  | Some x -> (
      match account_of_string x with
      | exception _ -> Lwt.return_none
      | x -> Lwt.return_some x)

let update_account_by_id s id =
  let module S = (val s : Storage_sig.BACKEND) in
  let* x = S.update (Account id) in
  match x with
  | None -> Lwt.return_none
  | Some (x, set) -> (
      let set x =
        let* () = set (string_of_account x) in
        run_update_hooks x
      in
      match account_of_string x with
      | exception _ -> Lwt.return_none
      | x -> Lwt.return (Some (x, set)))

let drop_after_at x =
  match String.index_opt x '@' with None -> x | Some i -> String.sub x 0 i

let create_account s ~email user =
  let module S = (val s : Storage_sig.BACKEND) in
  let@ id, u =
   fun cont ->
    let* x = S.new_account_id () in
    match x with
    | None -> Lwt.fail (Failure "impossible to create a new account")
    | Some x -> cont x
  in
  let last_connected = Datetime.now () in
  let name =
    let x = drop_after_at user.user_name in
    if x = "" then Printf.sprintf "User #%d" id else x
  in
  let account =
    {
      id;
      name;
      email;
      last_connected;
      authentications = [ user ];
      consent = None;
      capabilities = None;
      language = None;
      default_voter_languages = [];
      default_contact = "";
      voters_limit = None;
    }
  in
  let* () =
    Lwt.finalize
      (fun () ->
        let* () = S.create (Account id) (string_of_account account) in
        run_update_hooks account)
      (fun () ->
        Lwt.wakeup_later u ();
        Lwt.return_unit)
  in
  let* () = clear_account_cache () in
  Lwt.return account

let build_account_cache s =
  let module S = (val s : Storage_sig.BACKEND) in
  S.list_accounts ()
  >>= Lwt_list.fold_left_s
        (fun accu id ->
          let* account = get_account_by_id s id in
          match account with
          | None -> Lwt.return accu
          | Some account ->
              List.fold_left
                (fun accu u -> UMap.add u account.id accu)
                accu account.authentications
              |> Lwt.return)
        UMap.empty

let lookup_by_user lookup_by_id s user =
  let* cache =
    match !cache with
    | Some x -> Lwt.return x
    | None ->
        let@ () = Lwt_mutex.with_lock cache_mutex in
        let* x = build_account_cache s in
        cache := Some x;
        Lwt.return x
  in
  let&* id = UMap.find_opt user cache in
  lookup_by_id s id

let get_account = lookup_by_user get_account_by_id
let update_account = lookup_by_user update_account_by_id

type capability = Sudo

let mask_of_capability = function Sudo -> 1

let has_capability cap account =
  match account.capabilities with
  | None -> false
  | Some i -> i land mask_of_capability cap <> 0

let check a i = List.mem a.id i

let max_voters a =
  let default = !Web_config.maxmailsatonce in
  max default (Option.value ~default a.voters_limit)
