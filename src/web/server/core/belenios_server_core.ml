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

include Serializable_j
include Core
module Filesystem = Filesystem
module Defaults = Defaults

module Storage = struct
  include Storage_sig

  let backends = ref []
  let backend = ref None

  let get_backend () =
    match !backend with
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

  let get_user_id x =
    let module X = (val get_backend () : S) in
    X.get_user_id x

  let get_elections_by_owner x =
    let module X = (val get_backend () : S) in
    X.get_elections_by_owner x

  let register_backend name x = backends := (name, x) :: !backends

  let init_backend name config =
    match List.assoc_opt name !backends with
    | None -> Printf.ksprintf failwith "backend %s not found" name
    | Some f -> backend := Some (f config)
end
