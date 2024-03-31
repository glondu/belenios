(**************************************************************************)
(*                                BELENIOS                                *)
(*                                                                        *)
(*  Copyright © 2023-2023 Inria                                           *)
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

open Belenios

module type S = sig
  type t

  module TSet : Set.S with type elt = t

  val mutexes : t Election_mutex.t
  val task : t -> unit Lwt.t
  val deferred : TSet.t ref
end

type 'a t = (module S with type t = 'a)

let create (type a) mutexes task =
  let module X = struct
    type t = a

    module TSet = Set.Make (struct
      type t = a

      let compare = Stdlib.compare
    end)

    let mutexes = mutexes
    let task = task
    let deferred = ref TSet.empty
  end in
  (module X : S with type t = a)

let defer (type a) x key =
  let open (val x : S with type t = a) in
  match TSet.mem key !deferred with
  | false ->
      deferred := TSet.add key !deferred;
      Lwt.async (fun () ->
          Lwt.finalize
            (fun () ->
              let@ () = Election_mutex.with_lock mutexes key in
              task key)
            (fun () ->
              deferred := TSet.remove key !deferred;
              Lwt.return_unit))
  | true -> ()
