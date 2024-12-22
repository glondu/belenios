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
open Belenios_server_core

type 'a t = 'a Storage.election_file

let get s uuid file =
  let module S = (val s : Storage.BACKEND) in
  let* x = S.get (Election (uuid, file)) in
  let&* x = Lopt.get_value x in
  Lwt.return_some x

let del s uuid file =
  let module S = (val s : Storage.BACKEND) in
  S.del (Election (uuid, file))

let update s uuid (type a) file =
  let module S = (val s : Storage.BACKEND) in
  let* x = S.update (Election (uuid, file)) in
  let&* x, set = x in
  let&* x = Lopt.get_value x in
  let set x = set Value x in
  Lwt.return_some (x, set)

let set s uuid file x =
  let module S = (val s : Storage.BACKEND) in
  S.set (Election (uuid, file)) Value x
