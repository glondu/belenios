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

let ( !! ) x = !Web_config.spool_dir // x
let ( /// ) uuid x = !!(Uuid.unwrap uuid // x)

type kind = Raw | Trim

type election_file =
  | Draft
  | State
  | Public_creds
  | Private_creds
  | Hide_result
  | Dates
  | Decryption_tokens
  | Metadata
  | Private_key
  | Private_keys
  | Skipped_shufflers
  | Shuffle_token
  | Audit_cache
  | Last_event
  | Salts
  | Extended_records
  | Credential_mappings
  | Deleted
  | Public_archive
  | Passwords
  | Records
  | Voters
  | Confidential_archive
  | Private_creds_downloaded

type t =
  | Spool_version
  | Account_counter
  | Account of int
  | Election of uuid * election_file
  | Auth_db of string

let files_of_directory d = Lwt_unix.files_of_directory d |> Lwt_stream.to_list

let list_accounts () =
  let* xs = files_of_directory !Web_config.accounts_dir in
  Lwt.return
  @@ List.fold_left
       (fun accu x ->
         match Filename.chop_suffix_opt ~suffix:".json" x with
         | None -> accu
         | Some x -> (
             match int_of_string_opt x with None -> accu | Some x -> x :: accu))
       [] xs

let list_elections () =
  let* xs = files_of_directory !Web_config.spool_dir in
  Lwt.return
  @@ List.fold_left
       (fun accu x ->
         match Uuid.wrap x with exception _ -> accu | x -> x :: accu)
       [] xs

let get_election_file_props uuid = function
  | Draft -> ("draft.json", Trim)
  | State -> ("state.json", Trim)
  | Public_creds -> ("public_creds.json", Trim)
  | Private_creds -> ("private_creds.txt", Raw)
  | Hide_result -> ("hide_result", Trim)
  | Dates -> ("dates.json", Trim)
  | Decryption_tokens -> ("decryption_tokens.json", Trim)
  | Metadata -> ("metadata.json", Trim)
  | Private_key -> ("private_key.json", Trim)
  | Private_keys -> ("private_keys.jsons", Raw)
  | Skipped_shufflers -> ("skipped_shufflers.json", Trim)
  | Shuffle_token -> ("shuffle_token.json", Trim)
  | Audit_cache -> ("audit_cache.json", Trim)
  | Last_event -> ("last_event.json", Trim)
  | Salts -> ("salts.json", Trim)
  | Extended_records -> ("extended_records.jsons", Raw)
  | Credential_mappings -> ("credential_mappings.jsons", Raw)
  | Deleted -> ("deleted.json", Trim)
  | Public_archive -> (Uuid.unwrap uuid ^ ".bel", Raw)
  | Passwords -> ("passwords.csv", Raw)
  | Records -> ("records", Raw)
  | Voters -> ("voters.txt", Raw)
  | Confidential_archive -> ("archive.zip", Raw)
  | Private_creds_downloaded -> ("private_creds.downloaded", Raw)

let get_props = function
  | Spool_version -> (!!"version", Trim)
  | Account_counter -> (!Web_config.accounts_dir // "counter", Trim)
  | Account id -> (!Web_config.accounts_dir // Printf.sprintf "%d.json" id, Trim)
  | Election (uuid, f) ->
      let fname, kind = get_election_file_props uuid f in
      (uuid /// fname, kind)
  | Auth_db f -> (f, Raw)

let file_exists x =
  let x = fst @@ get_props x in
  Filesystem.file_exists x

let get f =
  let path, kind = get_props f in
  let* x = Filesystem.read_file path in
  match kind with
  | Raw -> Lwt.return x
  | Trim -> Lwt.return @@ Option.map String.trim x

let set f data =
  let fname, kind = get_props f in
  let data = match kind with Raw -> data | Trim -> data ^ "\n" in
  Filesystem.write_file fname data

let append_to_file f lines =
  let fname = fst @@ get_props f in
  let open Lwt_io in
  let@ oc =
    with_file ~mode:Output ~flags:[ O_WRONLY; O_APPEND; O_CREAT ] fname
  in
  Lwt_list.iter_s (write_line oc) lines

let del f =
  let f = fst @@ get_props f in
  Filesystem.cleanup_file f

let rmdir dir =
  let command = ("rm", [| "rm"; "-rf"; dir |]) in
  let* _ = Lwt_process.exec command in
  Lwt.return_unit

let new_election () =
  let length = !Web_config.uuid_length in
  let rec loop trials =
    if trials > 0 then
      let uuid = generate_token ?length () in
      Lwt.try_bind
        (fun () -> Lwt_unix.mkdir !!uuid 0o700)
        (fun () -> Lwt.return_some @@ Uuid.wrap uuid)
        (fun _ -> loop (trials - 1))
    else Lwt.return_none
  in
  loop 10

let cleanup_election uuid = rmdir !!(Uuid.unwrap uuid)

let copy_file src dst =
  let open Lwt_io in
  chars_of_file src |> chars_to_file dst

let try_copy_file src dst =
  let* b = Filesystem.file_exists src in
  if b then copy_file src dst else Lwt.return_unit

let make_archive uuid =
  let uuid_s = Uuid.unwrap uuid in
  let* temp_dir =
    Lwt_preemptive.detach
      (fun () ->
        let temp_dir = Filename.temp_file "belenios" "archive" in
        Sys.remove temp_dir;
        Unix.mkdir temp_dir 0o700;
        Unix.mkdir (temp_dir // "public") 0o755;
        Unix.mkdir (temp_dir // "restricted") 0o700;
        temp_dir)
      ()
  in
  let* () =
    Lwt_list.iter_p
      (fun x -> try_copy_file (uuid /// x) (temp_dir // "public" // x))
      [ Uuid.unwrap uuid ^ ".bel" ]
  in
  let* () =
    Lwt_list.iter_p
      (fun x -> try_copy_file (uuid /// x) (temp_dir // "restricted" // x))
      [ "voters.txt"; "records" ]
  in
  let command =
    Printf.ksprintf Lwt_process.shell
      "cd \"%s\" && zip -r archive public restricted" temp_dir
  in
  let* r = Lwt_process.exec command in
  match r with
  | Unix.WEXITED 0 ->
      let fname = uuid /// "archive.zip" in
      let fname_new = fname ^ ".new" in
      let* () = copy_file (temp_dir // "archive.zip") fname_new in
      let* () = Lwt_unix.rename fname_new fname in
      rmdir temp_dir
  | _ ->
      Printf.ksprintf Ocsigen_messages.errlog
        "Error while creating archive.zip for election %s, temporary directory \
         left in %s"
        uuid_s temp_dir;
      Lwt.return_unit

let get_archive uuid =
  let* state = get (Election (uuid, State)) in
  let final =
    match state with
    | None -> true
    | Some x -> (
        match Web_serializable_j.election_state_of_string x with
        | `Tallied | `Archived -> true
        | _ -> false)
  in
  if final then
    let archive_name = Election (uuid, Confidential_archive) in
    let* b = file_exists archive_name in
    let* () = if not b then make_archive uuid else Lwt.return_unit in
    Lwt.return (fst @@ get_props archive_name)
  else Lwt.fail Not_found

let get_as_file = function
  | Election (_, (Public_archive | Private_creds)) as x ->
      Lwt.return @@ fst @@ get_props x
  | Election (uuid, Confidential_archive) -> get_archive uuid
  | _ -> Lwt.fail Not_found

let account_id_promise = ref Lwt.return_unit

let new_account_id () =
  let min = !Web_config.account_id_min in
  let max = !Web_config.account_id_max in
  let delta = Z.(max - min) in
  let* () = !account_id_promise in
  let t, u = Lwt.task () in
  account_id_promise := t;
  let rec loop trials =
    if trials > 0 then
      let id = Z.(to_int (min + Random.random delta)) in
      let* b = file_exists (Account id) in
      if b then loop (trials - 1) else Lwt.return_some (id, u)
    else Lwt.fail Exit
  in
  Lwt.catch
    (fun () -> loop 10)
    (fun _ ->
      Lwt.wakeup_later u ();
      Lwt.return_none)

module ExtendedRecordsCacheTypes = struct
  type key = uuid
  type value = (datetime * string) SMap.t
end

module ExtendedRecordsCache = Ocsigen_cache.Make (ExtendedRecordsCacheTypes)

let raw_get_extended_records uuid =
  let* x = get (Election (uuid, Extended_records)) in
  let x = match x with None -> [] | Some x -> split_lines x in
  Lwt_list.fold_left_s
    (fun accu x ->
      let x = extended_record_of_string x in
      Lwt.return @@ SMap.add x.r_username (x.r_date, x.r_credential) accu)
    SMap.empty x

let dump_extended_records uuid rs =
  let rs = SMap.bindings rs in
  let extended_records =
    List.map
      (fun (r_username, (r_date, r_credential)) ->
        { r_username; r_date; r_credential } |> string_of_extended_record)
      rs
    |> join_lines
  in
  let records =
    List.map
      (fun (u, (d, _)) -> Printf.sprintf "%s %S" (string_of_datetime d) u)
      rs
    |> join_lines
  in
  let* () = set (Election (uuid, Extended_records)) extended_records in
  set (Election (uuid, Records)) records

let extended_records_cache =
  new ExtendedRecordsCache.cache raw_get_extended_records ~timer:3600. 10

let extended_records_deferrer =
  Election_defer.create (fun uuid ->
      let* x = extended_records_cache#find uuid in
      dump_extended_records uuid x)

let find_extended_record uuid username =
  let* rs = extended_records_cache#find uuid in
  Lwt.return (SMap.find_opt username rs)

let add_extended_record uuid username r =
  let* rs = extended_records_cache#find uuid in
  let rs = SMap.add username r rs in
  extended_records_cache#add uuid rs;
  let* () =
    let r_date, r_credential = r in
    { r_username = username; r_date; r_credential }
    |> string_of_extended_record
    |> (fun x -> [ x ])
    |> append_to_file (Election (uuid, Extended_records))
  in
  Election_defer.defer extended_records_deferrer uuid;
  Lwt.return_unit

module CredMappingsCacheTypes = struct
  type key = uuid
  type value = string option SMap.t
end

module CredMappingsCache = Ocsigen_cache.Make (CredMappingsCacheTypes)

let raw_get_credential_mappings uuid =
  let* x = get (Election (uuid, Credential_mappings)) in
  let x = match x with None -> [] | Some x -> split_lines x in
  Lwt_list.fold_left_s
    (fun accu x ->
      let x = credential_mapping_of_string x in
      Lwt.return @@ SMap.add x.c_credential x.c_ballot accu)
    SMap.empty x

let dump_credential_mappings uuid xs =
  SMap.fold
    (fun c_credential c_ballot accu -> { c_credential; c_ballot } :: accu)
    xs []
  |> List.rev_map string_of_credential_mapping
  |> join_lines
  |> set (Election (uuid, Credential_mappings))

let credential_mappings_cache =
  new CredMappingsCache.cache raw_get_credential_mappings ~timer:3600. 10

let credential_mappings_deferrer =
  Election_defer.create (fun uuid ->
      let* x = credential_mappings_cache#find uuid in
      dump_credential_mappings uuid x)

let init_credential_mapping uuid =
  let* file = get (Election (uuid, Public_creds)) in
  match file with
  | Some x ->
      let public_credentials =
        public_credentials_of_string x |> List.map strip_public_credential
      in
      let xs =
        List.fold_left
          (fun accu x ->
            let x = (parse_public_credential Fun.id x).credential in
            if SMap.mem x accu then
              failwith "trying to add duplicate credential"
            else SMap.add x None accu)
          SMap.empty public_credentials
      in
      credential_mappings_cache#add uuid xs;
      let* () = dump_credential_mappings uuid xs in
      Lwt.return public_credentials
  | None -> Lwt.fail @@ Election_not_found (uuid, "init_credential_mapping")

let find_credential_mapping uuid cred =
  let* xs = credential_mappings_cache#find uuid in
  Lwt.return @@ SMap.find_opt cred xs

let add_credential_mapping uuid cred mapping =
  let* xs = credential_mappings_cache#find uuid in
  let xs = SMap.add cred mapping xs in
  credential_mappings_cache#add uuid xs;
  let* () =
    { c_credential = cred; c_ballot = mapping }
    |> string_of_credential_mapping
    |> (fun x -> [ x ])
    |> append_to_file (Election (uuid, Credential_mappings))
  in
  Election_defer.defer credential_mappings_deferrer uuid;
  Lwt.return_unit

let delete_sensitive_data uuid =
  Lwt_list.iter_p
    (fun x -> del (Election (uuid, x)))
    [
      State;
      Private_key;
      Private_keys;
      Decryption_tokens;
      Extended_records;
      Credential_mappings;
      Public_creds;
    ]

let delete_live_data uuid =
  Lwt_list.iter_p
    (fun x -> del (Election (uuid, x)))
    [
      Last_event;
      Dates;
      Metadata;
      Audit_cache;
      Hide_result;
      Shuffle_token;
      Skipped_shufflers;
      Salts;
      Public_archive;
      Passwords;
      Records;
      Voters;
      Confidential_archive;
    ]
