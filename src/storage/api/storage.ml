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
open Types

module type TX = sig
  module S : STORAGE

  val tx : S.t
end

type t = (module TX)
type 'a u = t -> uuid -> 'a

let backends = ref []
let backend = ref None

let get_backend () : (module STORAGE) =
  match !backend with None -> failwith "no storage backend set" | Some x -> x

let with_transaction f =
  let module S = (val get_backend ()) in
  let@ tx = S.with_transaction in
  let module T = struct
    module S = S

    let tx = tx
  end in
  f (module T : TX)

let get_user_id x =
  let module X = (val get_backend ()) in
  X.get_user_id x

let get_elections_by_owner x =
  let module X = (val get_backend ()) in
  X.get_elections_by_owner x

let register_backend name x = backends := (name, x) :: !backends

let init_backend name config =
  match List.assoc_opt name !backends with
  | None -> Printf.ksprintf failwith "backend %s not found" name
  | Some f ->
      let* b = f config in
      backend := Some b;
      Lwt.return_unit

let get_unixfilename tx f =
  let module T = (val tx : TX) in
  T.S.get_unixfilename T.tx f

let get tx f =
  let module T = (val tx : TX) in
  T.S.get T.tx f

let set tx f k x =
  let module T = (val tx : TX) in
  T.S.set T.tx f k x

let del tx f =
  let module T = (val tx : TX) in
  T.S.del T.tx f

let update tx f set =
  let module T = (val tx : TX) in
  T.S.update T.tx f set

let append tx u ?last ops =
  let module T = (val tx : TX) in
  T.S.append T.tx u ?last ops

let append_sealing tx sealing =
  let module T = (val tx : TX) in
  T.S.append_sealing T.tx sealing

let new_election tx =
  let module T = (val tx : TX) in
  T.S.new_election T.tx

let archive_election tx u =
  let module T = (val tx : TX) in
  T.S.archive_election T.tx u

let delete_election tx u =
  let module T = (val tx : TX) in
  T.S.delete_election T.tx u

let validate_election tx u =
  let module T = (val tx : TX) in
  T.S.validate_election T.tx u

let new_account_id tx =
  let module T = (val tx : TX) in
  T.S.new_account_id T.tx
