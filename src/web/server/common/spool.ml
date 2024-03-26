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
open Web_serializable_j
open Web_common

type 'a file = {
  of_string : string -> 'a;
  to_string : 'a -> string;
  filename : Storage_sig.election_file;
}

type 'a abstract = {
  get : uuid -> 'a option Lwt.t;
  set : uuid -> 'a -> unit Lwt.t;
  del : uuid -> unit Lwt.t;
}

type 'a t = File of 'a file | Abstract of 'a abstract

let get ~uuid file =
  match file with
  | File file -> (
      let* x = Storage.(get (Election (uuid, file.filename))) in
      let&* x = x in
      try Lwt.return_some (file.of_string x) with _ -> Lwt.return_none)
  | Abstract a -> a.get uuid

let set ~uuid file x =
  match file with
  | File file ->
      Storage.(set (Election (uuid, file.filename)) (file.to_string x))
  | Abstract a -> a.set uuid x

let del ~uuid file =
  match file with
  | File file -> Storage.(del (Election (uuid, file.filename)))
  | Abstract a -> a.del uuid

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
    of_string = Fun.id;
    to_string = Fun.id;
    filename = Storage_sig.Public_creds;
  }
  |> make_file

let draft_private_credentials =
  {
    of_string = Fun.id;
    to_string = Fun.id;
    filename = Storage_sig.Private_creds;
  }
  |> make_file

let hide_result =
  {
    of_string = datetime_of_string;
    to_string = string_of_datetime;
    filename = Storage_sig.Hide_result;
  }
  |> make_file

let dates =
  {
    of_string = election_dates_of_string;
    to_string = string_of_election_dates;
    filename = Storage_sig.Dates;
  }
  |> make_file

let state =
  {
    of_string = election_state_of_string;
    to_string = string_of_election_state;
    filename = Storage_sig.State;
  }
  |> make_file

let decryption_tokens =
  {
    of_string = decryption_tokens_of_string;
    to_string = string_of_decryption_tokens;
    filename = Storage_sig.Decryption_tokens;
  }
  |> make_file

let metadata =
  {
    of_string = metadata_of_string;
    to_string = string_of_metadata;
    filename = Storage_sig.Metadata;
  }
  |> make_file

let private_key =
  {
    of_string = Yojson.Safe.from_string;
    to_string = Yojson.Safe.to_string;
    filename = Storage_sig.Private_key;
  }
  |> make_file

let private_keys =
  let filename = Storage_sig.Private_keys in
  let get uuid =
    let* x = Storage.(get (Election (uuid, filename))) in
    let&* x = x in
    Lwt.return_some @@ split_lines x
  in
  let set uuid x = Storage.(set (Election (uuid, filename)) (join_lines x)) in
  let del uuid = Storage.(del (Election (uuid, filename))) in
  Abstract { get; set; del }

let skipped_shufflers =
  {
    of_string = skipped_shufflers_of_string;
    to_string = string_of_skipped_shufflers;
    filename = Storage_sig.Skipped_shufflers;
  }
  |> make_file

let shuffle_token =
  {
    of_string = shuffle_token_of_string;
    to_string = string_of_shuffle_token;
    filename = Storage_sig.Shuffle_token;
  }
  |> make_file

let audit_cache =
  {
    of_string = audit_cache_of_string;
    to_string = string_of_audit_cache;
    filename = Storage_sig.Audit_cache;
  }
  |> make_file

let last_event =
  {
    of_string = last_event_of_string;
    to_string = string_of_last_event;
    filename = Storage_sig.Last_event;
  }
  |> make_file

let salts =
  {
    of_string = salts_of_string;
    to_string = string_of_salts;
    filename = Storage_sig.Salts;
  }
  |> make_file
