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
open Belenios
open Belenios_storage_api

let update_hooks = ref []
let add_update_hook f = update_hooks := f :: !update_hooks

let run_update_hooks account =
  Lwt_list.iter_s (fun f -> f account) !update_hooks

let get_account_by_id s id =
  let* x = Storage.get s (Account id) in
  x |> Lopt.get_value |> Lwt.return

let update_account_by_id s id cont =
  let@ x, set = Storage.update s (Account id) in
  let set x =
    let* () = set Value x in
    run_update_hooks x
  in
  cont (x, set)

let drop_after_at x =
  match String.index_opt x '@' with None -> x | Some i -> String.sub x 0 i

let create_account s ~name ~email user =
  let@ id, u =
   fun cont ->
    let* x = Storage.new_account_id s in
    match x with
    | None -> Lwt.fail (Failure "impossible to create a new account")
    | Some x -> cont x
  in
  let last_connected = Unix.gettimeofday () in
  let name =
    match name with
    | None ->
        let x = drop_after_at user.user_name in
        if x = "" then Printf.sprintf "User #%d" id else x
    | Some x -> x
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
        let* () = Storage.set s (Account id) Value account in
        run_update_hooks account)
      (fun () ->
        Lwt.wakeup_later u ();
        Lwt.return_unit)
  in
  Lwt.return account

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
