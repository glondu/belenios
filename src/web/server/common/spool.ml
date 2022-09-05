(**************************************************************************)
(*                                BELENIOS                                *)
(*                                                                        *)
(*  Copyright © 2012-2022 Inria                                           *)
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
open Belenios_core.Serializable_builtin_t
open Belenios_core.Serializable_j
open Web_serializable_j
open Web_common

type 'a t =
  {
    of_string : string -> 'a;
    to_string : 'a -> string;
    filename : string;
  }

type 'a list = 'a t

let filename x = x.filename

let raw filename =
  {
    of_string = Fun.id;
    to_string = Fun.id;
    filename;
  }

let get ~uuid file =
  let* x = read_file_single_line ~uuid file.filename in
  let&* x in
  try Lwt.return_some (file.of_string x)
  with _ -> Lwt.return_none

let get_default ~default ~uuid file =
  let* x = get ~uuid file in
  Lwt.return @@ Option.value ~default x

let get_raw_list ~uuid filename =
  read_file ~uuid filename.filename

let get_fold_s ~uuid file f accu =
  let* xs = read_file ~uuid file.filename in
  let&* xs in
  let* x = Lwt_list.fold_left_s (fun accu x -> f (file.of_string x) accu) accu xs in
  Lwt.return_some x

let get_fold_s_default ~uuid file f accu =
  let* x = read_file ~uuid file.filename in
  match x with
  | None -> Lwt.return accu
  | Some xs -> Lwt_list.fold_left_s (fun accu x -> f (file.of_string x) accu) accu xs

let set ~uuid file x =
  write_file ~uuid file.filename [file.to_string x]

let set_list ~uuid file xs =
  write_file ~uuid file.filename @@ List.map file.to_string xs

let del ~uuid file =
  let filename = uuid /// file.filename in
  Lwt.catch
    (fun () -> Lwt_unix.unlink filename)
    (fun _ -> Lwt.return_unit)

let draft =
  {
    of_string = draft_election_of_string;
    to_string = string_of_draft_election;
    filename = "draft.json";
  }

let hide_result =
  {
    of_string = datetime_of_string;
    to_string = string_of_datetime;
    filename = "hide_result";
  }

let dates =
  {
    of_string = election_dates_of_string;
    to_string = string_of_election_dates;
    filename = "dates.json";
  }

let state =
  {
    of_string = election_state_of_string;
    to_string = string_of_election_state;
    filename = "state.json";
  }

let decryption_tokens =
  {
    of_string = decryption_tokens_of_string;
    to_string = string_of_decryption_tokens;
    filename = "decryption_tokens.json";
  }

let metadata =
  {
    of_string = metadata_of_string;
    to_string = string_of_metadata;
    filename = "metadata.json";
  }

let private_key =
  {
    of_string = number_of_string;
    to_string = string_of_number;
    filename = "private_key.json";
  }

let private_keys = raw "private_keys.jsons"

let ballots_index =
  {
    of_string = Yojson.Safe.from_string;
    to_string = Yojson.Safe.to_string;
    filename = "ballots_index.json";
  }

let skipped_shufflers =
  {
    of_string = skipped_shufflers_of_string;
    to_string = string_of_skipped_shufflers;
    filename = "skipped_shufflers.json";
  }

let shuffle_token =
  {
    of_string = shuffle_token_of_string;
    to_string = string_of_shuffle_token;
    filename = "shuffle_token.json";
  }

let extended_records =
  {
    of_string = extended_record_of_string;
    to_string = string_of_extended_record;
    filename = "extended_records.jsons";
  }

let records =
  {
    of_string = Fun.id;
    to_string = Fun.id;
    filename = string_of_election_file ESRecords;
  }

let credential_mappings =
  {
    of_string = credential_mapping_of_string;
    to_string = string_of_credential_mapping;
    filename = "credential_mappings.jsons";
  }

let audit_cache =
  {
    of_string = audit_cache_of_string;
    to_string = string_of_audit_cache;
    filename = "audit_cache.json";
  }

let voters = raw "voters.txt"

let chain_filename uuid = raw_string_of_uuid uuid ^ ".bel"
let chain uuid = raw (chain_filename uuid)

let last_event =
  {
    of_string = last_event_of_string;
    to_string = string_of_last_event;
    filename = "last_event.json";
  }
