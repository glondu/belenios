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
open Belenios
open Belenios_server_core

type 'a t = { to_string : 'a -> string; filename : 'a Storage.election_file }

let get s uuid file =
  let module S = (val s : Storage.BACKEND) in
  let* x = S.get (Election (uuid, file.filename)) in
  let&* x = Lopt.get_value x in
  Lwt.return_some x

let del s uuid file =
  let module S = (val s : Storage.BACKEND) in
  S.del (Election (uuid, file.filename))

let update s uuid (type a) (file : a t) =
  let module S = (val s : Storage.BACKEND) in
  let* x = S.update (Election (uuid, file.filename)) in
  let&* x, set = x in
  let&* x = Lopt.get_value x in
  let set x = set (x |> Lopt.some_value file.to_string) in
  Lwt.return_some (x, set)

let create s uuid file x =
  let module S = (val s : Storage.BACKEND) in
  S.create (Election (uuid, file.filename)) (x |> Lopt.some_value file.to_string)

let ensure s uuid file x =
  let module S = (val s : Storage.BACKEND) in
  S.ensure (Election (uuid, file.filename)) (x |> Lopt.some_value file.to_string)

let draft = { to_string = string_of_draft_election; filename = Storage.Draft }

let draft_public_credentials =
  { to_string = string_of_public_credentials; filename = Storage.Public_creds }

let dates_full =
  let open Belenios_storage_api in
  { to_string = string_of_election_dates; filename = Storage.Dates_full }

let state = { to_string = string_of_election_state; filename = Storage.State }

let decryption_tokens =
  {
    to_string = string_of_decryption_tokens;
    filename = Storage.Decryption_tokens;
  }

let metadata = { to_string = string_of_metadata; filename = Storage.Metadata }

let private_key =
  { to_string = Yojson.Safe.to_string; filename = Storage.Private_key }

let private_keys = { to_string = join_lines; filename = Storage.Private_keys }

let skipped_shufflers =
  {
    to_string = string_of_skipped_shufflers;
    filename = Storage.Skipped_shufflers;
  }

let shuffle_token =
  { to_string = string_of_shuffle_token; filename = Storage.Shuffle_token }

let audit_cache =
  { to_string = string_of_audit_cache; filename = Storage.Audit_cache }

let last_event =
  { to_string = string_of_last_event; filename = Storage.Last_event }
