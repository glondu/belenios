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

include Web_serializable_j
include Core
module Filesystem = Filesystem
module Defaults = Defaults

module Storage = struct
  include Storage_sig

  let storage_backend = ref None

  let get_backend () =
    match !storage_backend with
    | None -> failwith "no storage backend set"
    | Some x -> x

  let register_passwords_db f =
    let module X = (val get_backend () : S) in
    X.register_passwords_db f

  let register_auth_db f =
    let module X = (val get_backend () : S) in
    X.register_auth_db f

  let with_transaction f =
    let module X = (val get_backend () : S) in
    X.with_transaction f

  let init_backend x = storage_backend := Some x
end
