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

module type S = sig
  type t

  module TMap : Map.S with type key = t

  val mutexes : unit Lwt.u Queue.t TMap.t ref
end

type 'a t = (module S with type t = 'a)

let create (type a) () =
  let module X = struct
    type t = a

    module TMap = Map.Make (struct
      type t = a

      let compare = Stdlib.compare
    end)

    let mutexes = ref TMap.empty
  end in
  (module X : S with type t = a)

let lock (type a) x key =
  let open (val x : S with type t = a) in
  match TMap.find_opt key !mutexes with
  | None ->
      mutexes := TMap.add key (Queue.create ()) !mutexes;
      Lwt.return_unit
  | Some waiters ->
      let t, u = Lwt.task () in
      Queue.push u waiters;
      t

let unlock (type a) x key =
  let open (val x : S with type t = a) in
  match TMap.find_opt key !mutexes with
  | None -> ()
  | Some waiters -> (
      match Queue.take_opt waiters with
      | None -> mutexes := TMap.remove key !mutexes
      | Some u -> Lwt.wakeup_later u ())

let with_lock x key f =
  Lwt.bind (lock x key) (fun () ->
      Lwt.finalize f (fun () ->
          unlock x key;
          Lwt.return_unit))
