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

type 'a file = {
  of_string : string -> 'a;
  to_string : 'a -> string;
  filename : Storage.election_file;
}

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
  | File file -> (
      let module S = (val s : Storage.BACKEND) in
      let* x = S.get (Election (uuid, file.filename)) in
      let&* x = x in
      try Lwt.return_some (file.of_string x) with _ -> Lwt.return_none)
  | Abstract a -> a.get s uuid

let del s uuid file =
  match file with
  | File file ->
      let module S = (val s : Storage.BACKEND) in
      S.del (Election (uuid, file.filename))
  | Abstract a -> a.del s uuid

let update s uuid file =
  match file with
  | File file ->
      let module S = (val s : Storage.BACKEND) in
      let* x = S.update (Election (uuid, file.filename)) in
      let&* x, set = x in
      let set x = set (file.to_string x) in
      Lwt.return_some (file.of_string x, set)
  | Abstract a -> a.update s uuid

let create s uuid file x =
  match file with
  | File file ->
      let module S = (val s : Storage.BACKEND) in
      S.create (Election (uuid, file.filename)) (file.to_string x)
  | Abstract a -> a.create s uuid x

let ensure s uuid file x =
  match file with
  | File file ->
      let module S = (val s : Storage.BACKEND) in
      S.ensure (Election (uuid, file.filename)) (file.to_string x)
  | Abstract a -> a.ensure s uuid x

let make_file x = File x

let draft =
  {
    of_string = draft_election_of_string;
    to_string = string_of_draft_election;
    filename = Draft;
  }
  |> make_file

let draft_public_credentials =
  {
    of_string = public_credentials_of_string;
    to_string = string_of_public_credentials;
    filename = Storage.Public_creds;
  }
  |> make_file

let dates_full =
  let open Belenios_storage_api in
  {
    of_string = election_dates_of_string;
    to_string = string_of_election_dates;
    filename = Storage.Dates_full;
  }
  |> make_file

let state =
  {
    of_string = election_state_of_string;
    to_string = string_of_election_state;
    filename = Storage.State;
  }
  |> make_file

let decryption_tokens =
  {
    of_string = decryption_tokens_of_string;
    to_string = string_of_decryption_tokens;
    filename = Storage.Decryption_tokens;
  }
  |> make_file

let metadata =
  {
    of_string = metadata_of_string;
    to_string = string_of_metadata;
    filename = Storage.Metadata;
  }
  |> make_file

let private_key =
  {
    of_string = Yojson.Safe.from_string;
    to_string = Yojson.Safe.to_string;
    filename = Storage.Private_key;
  }
  |> make_file

let private_keys =
  let filename = Storage.Private_keys in
  let get s uuid =
    let module S = (val s : Storage.BACKEND) in
    let* x = S.get (Election (uuid, filename)) in
    let&* x = x in
    Lwt.return_some @@ split_lines x
  in
  let del s uuid =
    let module S = (val s : Storage.BACKEND) in
    S.del (Election (uuid, filename))
  in
  let update s uuid =
    let module S = (val s : Storage.BACKEND) in
    let* x = S.update (Election (uuid, filename)) in
    let&* x, set = x in
    let set x = set (join_lines x) in
    Lwt.return_some (split_lines x, set)
  in
  let create s uuid x =
    let module S = (val s : Storage.BACKEND) in
    S.create (Election (uuid, filename)) (join_lines x)
  in
  let ensure s uuid x =
    let module S = (val s : Storage.BACKEND) in
    S.ensure (Election (uuid, filename)) (join_lines x)
  in
  Abstract { get; del; update; create; ensure }

let skipped_shufflers =
  {
    of_string = skipped_shufflers_of_string;
    to_string = string_of_skipped_shufflers;
    filename = Storage.Skipped_shufflers;
  }
  |> make_file

let shuffle_token =
  {
    of_string = shuffle_token_of_string;
    to_string = string_of_shuffle_token;
    filename = Storage.Shuffle_token;
  }
  |> make_file

let audit_cache =
  {
    of_string = audit_cache_of_string;
    to_string = string_of_audit_cache;
    filename = Storage.Audit_cache;
  }
  |> make_file

let last_event =
  {
    of_string = last_event_of_string;
    to_string = string_of_last_event;
    filename = Storage.Last_event;
  }
  |> make_file
