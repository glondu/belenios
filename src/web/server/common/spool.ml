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
open Web_common

type 'a file = { to_string : 'a -> string; filename : 'a Storage.election_file }

type 'a abstract = {
  get : 'a option Lwt.t Storage.u;
  del : unit Lwt.t Storage.u;
  update : 'a updatable option Lwt.t Storage.u;
  create : ('a -> unit Lwt.t) Storage.u;
  ensure : ('a -> unit Lwt.t) Storage.u;
}

type 'a t = File of 'a file | Abstract of 'a abstract

let get s uuid file =
  match file with
  | File file ->
      let module S = (val s : Storage.BACKEND) in
      let* x = S.get (Election (uuid, file.filename)) in
      let&* x = Lopt.get_value x in
      Lwt.return_some x
  | Abstract a -> a.get s uuid

let del s uuid file =
  match file with
  | File file ->
      let module S = (val s : Storage.BACKEND) in
      S.del (Election (uuid, file.filename))
  | Abstract a -> a.del s uuid

let update s uuid (type a) (file : a t) =
  match file with
  | File file ->
      let module S = (val s : Storage.BACKEND) in
      let* x = S.update (Election (uuid, file.filename)) in
      let&* x, set = x in
      let&* x = Lopt.get_value x in
      let set x = set (x |> Lopt.some_value file.to_string) in
      Lwt.return_some (x, set)
  | Abstract a -> a.update s uuid

let create s uuid file x =
  match file with
  | File file ->
      let module S = (val s : Storage.BACKEND) in
      S.create
        (Election (uuid, file.filename))
        (x |> Lopt.some_value file.to_string)
  | Abstract a -> a.create s uuid x

let ensure s uuid file x =
  match file with
  | File file ->
      let module S = (val s : Storage.BACKEND) in
      S.ensure
        (Election (uuid, file.filename))
        (x |> Lopt.some_value file.to_string)
  | Abstract a -> a.ensure s uuid x

let make_file x = File x

let draft =
  { to_string = string_of_draft_election; filename = Storage.Draft }
  |> make_file

let draft_public_credentials =
  { to_string = string_of_public_credentials; filename = Storage.Public_creds }
  |> make_file

let dates_full =
  let open Belenios_storage_api in
  { to_string = string_of_election_dates; filename = Storage.Dates_full }
  |> make_file

let state =
  { to_string = string_of_election_state; filename = Storage.State }
  |> make_file

let decryption_tokens =
  {
    to_string = string_of_decryption_tokens;
    filename = Storage.Decryption_tokens;
  }
  |> make_file

let metadata =
  { to_string = string_of_metadata; filename = Storage.Metadata } |> make_file

let private_key =
  { to_string = Yojson.Safe.to_string; filename = Storage.Private_key }
  |> make_file

let private_keys =
  let filename = Storage.Private_keys in
  let get s uuid =
    let module S = (val s : Storage.BACKEND) in
    let* x = S.get (Election (uuid, filename)) in
    let&* x = Lopt.get_value x in
    x |> split_lines |> Lwt.return_some
  in
  let del s uuid =
    let module S = (val s : Storage.BACKEND) in
    S.del (Election (uuid, filename))
  in
  let update s uuid =
    let module S = (val s : Storage.BACKEND) in
    let* x = S.update (Election (uuid, filename)) in
    let&* x, set = x in
    let&* x = Lopt.get_value x in
    let set x = set (x |> join_lines |> Lopt.some_string Fun.id) in
    Lwt.return_some (x |> split_lines, set)
  in
  let create s uuid x =
    let module S = (val s : Storage.BACKEND) in
    S.create
      (Election (uuid, filename))
      (x |> join_lines |> Lopt.some_string Fun.id)
  in
  let ensure s uuid x =
    let module S = (val s : Storage.BACKEND) in
    S.ensure
      (Election (uuid, filename))
      (x |> join_lines |> Lopt.some_string Fun.id)
  in
  Abstract { get; del; update; create; ensure }

let skipped_shufflers =
  {
    to_string = string_of_skipped_shufflers;
    filename = Storage.Skipped_shufflers;
  }
  |> make_file

let shuffle_token =
  { to_string = string_of_shuffle_token; filename = Storage.Shuffle_token }
  |> make_file

let audit_cache =
  { to_string = string_of_audit_cache; filename = Storage.Audit_cache }
  |> make_file

let last_event =
  { to_string = string_of_last_event; filename = Storage.Last_event }
  |> make_file
