(**************************************************************************)
(*                                BELENIOS                                *)
(*                                                                        *)
(*  Copyright © 2020 Inria                                                *)
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

open Serializable_builtin_t
open Common

let mutexes = ref SMap.empty

let lock uuid_s =
  match SMap.find_opt uuid_s !mutexes with
  | None ->
     mutexes := SMap.add uuid_s (Queue.create ()) !mutexes;
     Lwt.return_unit
  | Some waiters ->
     let t, u = Lwt.task () in
     Queue.push u waiters;
     t

let unlock uuid_s =
  match SMap.find_opt uuid_s !mutexes with
  | None -> ()
  | Some waiters ->
     match Queue.take_opt waiters with
     | None -> mutexes := SMap.remove uuid_s !mutexes
     | Some u -> Lwt.wakeup_later u ()

let with_lock uuid f =
  let uuid_s = raw_string_of_uuid uuid in
  Lwt.bind
    (lock uuid_s)
    (fun () ->
      Lwt.finalize f (fun () -> unlock uuid_s; Lwt.return_unit))
