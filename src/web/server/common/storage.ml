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

let get_backend () =
  match !Web_config.storage_backend with
  | None -> failwith "no storage backend set"
  | Some x -> x

let get_as_file f =
  let module X = (val get_backend ()) in
  X.get_as_file f

let get f =
  let module X = (val get_backend ()) in
  X.get f

let set f =
  let module X = (val get_backend ()) in
  X.set f

let del f =
  let module X = (val get_backend ()) in
  X.del f

let list_accounts () =
  let module X = (val get_backend ()) in
  X.list_accounts ()

let list_elections () =
  let module X = (val get_backend ()) in
  X.list_elections ()

let new_election () =
  let module X = (val get_backend ()) in
  X.new_election ()

let cleanup_election uuid =
  let module X = (val get_backend ()) in
  X.cleanup_election uuid

let new_account_id () =
  let module X = (val get_backend ()) in
  X.new_account_id ()

let with_lock uuid f =
  let module X = (val get_backend ()) in
  X.with_lock uuid f

let init_credential_mapping uuid =
  let module X = (val get_backend ()) in
  X.init_credential_mapping uuid

let delete_sensitive_data uuid =
  let module X = (val get_backend ()) in
  X.delete_sensitive_data uuid

let delete_live_data uuid =
  let module X = (val get_backend ()) in
  X.delete_live_data uuid

let append ?lock uuid ?last ops =
  let module X = (val get_backend ()) in
  X.append ?lock uuid ?last ops
