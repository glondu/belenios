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
open Belenios_core.Serializable_j
open Web_serializable_j
open Web_common

type 'a file =
  {
    of_string : string -> 'a;
    to_string : 'a -> string;
    filename : string;
  }

type 'a abstract =
  {
    get : uuid -> 'a option Lwt.t;
    set : uuid -> 'a -> unit Lwt.t;
    del : uuid -> unit Lwt.t;
  }

type 'a t =
  | File of 'a file
  | Abstract of 'a abstract

let get ~uuid file =
  match file with
  | File file ->
     begin
       let* x = Filesystem.read_file_single_line ~uuid file.filename in
       let&* x in
       try Lwt.return_some (file.of_string x)
       with _ -> Lwt.return_none
     end
  | Abstract a -> a.get uuid

let set ~uuid file x =
  match file with
  | File file -> Filesystem.write_file ~uuid file.filename [file.to_string x]
  | Abstract a -> a.set uuid x

let del ~uuid file =
  match file with
  | File file -> Filesystem.cleanup_file (uuid /// file.filename)
  | Abstract a -> a.del uuid

let make_file x = File x

let draft =
  {
    of_string = draft_election_of_string;
    to_string = string_of_draft_election;
    filename = "draft.json";
  } |> make_file

let draft_public_credentials =
  let filename = "public_creds.json" in
  let get uuid = Filesystem.read_whole_file ~uuid filename in
  let set uuid x = Filesystem.write_file ~uuid filename [x] in
  let del uuid = Filesystem.cleanup_file (uuid /// filename) in
  Abstract {get; set; del}

let draft_private_credentials =
  let filename = "private_creds.txt" in
  let get uuid = Filesystem.read_whole_file ~uuid filename in
  let set uuid x = Filesystem.write_whole_file ~uuid filename x in
  let del uuid = Filesystem.cleanup_file (uuid /// filename) in
  Abstract {get; set; del}

let hide_result =
  {
    of_string = datetime_of_string;
    to_string = string_of_datetime;
    filename = "hide_result";
  } |> make_file

let dates =
  {
    of_string = election_dates_of_string;
    to_string = string_of_election_dates;
    filename = "dates.json";
  } |> make_file

let state =
  {
    of_string = election_state_of_string;
    to_string = string_of_election_state;
    filename = "state.json";
  } |> make_file

let decryption_tokens =
  {
    of_string = decryption_tokens_of_string;
    to_string = string_of_decryption_tokens;
    filename = "decryption_tokens.json";
  } |> make_file

let metadata =
  {
    of_string = metadata_of_string;
    to_string = string_of_metadata;
    filename = "metadata.json";
  } |> make_file

let private_key =
  {
    of_string = number_of_string;
    to_string = string_of_number;
    filename = "private_key.json";
  } |> make_file

let private_keys =
  let filename = "private_keys.jsons" in
  let get uuid = Filesystem.read_file ~uuid filename in
  let set uuid x = Filesystem.write_file ~uuid filename x in
  let del uuid = Filesystem.cleanup_file (uuid /// filename) in
  Abstract {get; set; del}

let skipped_shufflers =
  {
    of_string = skipped_shufflers_of_string;
    to_string = string_of_skipped_shufflers;
    filename = "skipped_shufflers.json";
  } |> make_file

let shuffle_token =
  {
    of_string = shuffle_token_of_string;
    to_string = string_of_shuffle_token;
    filename = "shuffle_token.json";
  } |> make_file

let audit_cache =
  {
    of_string = audit_cache_of_string;
    to_string = string_of_audit_cache;
    filename = "audit_cache.json";
  } |> make_file

let last_event =
  {
    of_string = last_event_of_string;
    to_string = string_of_last_event;
    filename = "last_event.json";
  } |> make_file
