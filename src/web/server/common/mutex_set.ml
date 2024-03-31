(**************************************************************************)
(*                                BELENIOS                                *)
(*                                                                        *)
(*  Copyright © 2024-2024 Inria                                           *)
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

  module TSet : Set.S with type elt = t

  val indexed : t Election_mutex.t
  val mutexes : TSet.t ref
end

type 'a t = (module S with type t = 'a)

let create (type a) indexed =
  let module X = struct
    type t = a

    module TSet = Set.Make (struct
      type t = a

      let compare = Stdlib.compare
    end)

    let indexed = indexed
    let mutexes = ref TSet.empty
  end in
  (module X : S with type t = a)

let lock (type a) x key =
  let open (val x : S with type t = a) in
  match TSet.mem key !mutexes with
  | false ->
      mutexes := TSet.add key !mutexes;
      Election_mutex.lock indexed key
  | true -> Lwt.return_unit

let unlock (type a) x =
  let open (val x : S with type t = a) in
  TSet.iter (Election_mutex.unlock indexed) !mutexes;
  mutexes := TSet.empty
