(**************************************************************************)
(*                                BELENIOS                                *)
(*                                                                        *)
(*  Copyright © 2012-2024 Inria                                           *)
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
open Belenios_server_core

module UMap = Map.Make (struct
  type t = user

  let compare = Stdlib.compare
end)

module type INPUT = sig
  type session

  val list_accounts : session -> int list Lwt.t
  val get_account_by_id : session -> int -> account option Lwt.t
end

module Make (I : INPUT) () = struct
  let cache = ref None
  let cache_mutex = Lwt_mutex.create ()

  module Clear = struct
    let clear () = cache := None
  end

  let build_account_cache s =
    let* accounts = I.list_accounts s in
    Lwt_list.fold_left_s
      (fun accu id ->
        let* account = I.get_account_by_id s id in
        match account with
        | None -> Lwt.return accu
        | Some account ->
            List.fold_left
              (fun accu u -> UMap.add u account.id accu)
              accu account.authentications
            |> Lwt.return)
      UMap.empty accounts

  let get_user_id s user =
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
    Lwt.return_some id
end
