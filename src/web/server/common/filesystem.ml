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
  | Partial_decryptions
  | Ballots_index
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
  | Absolute of string

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
  | Partial_decryptions -> ("partial_decryptions.json", Raw)
  | Ballots_index -> ("ballots_index.json", Raw)
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
  | Absolute f -> (f, Raw)

let to_election_file uuid = function
  | ESArchive x when x = uuid -> Public_archive
  | ESVoters -> Voters
  | ESRecords -> Records
  | ESSalts -> Salts
  | _ -> raise Not_found

let get_path x = fst (get_props x)

let file_exists x =
  let x = get_path x in
  Lwt.catch
    (fun () ->
      let* () = Lwt_unix.(access x [ R_OK ]) in
      Lwt.return_true)
    (fun _ -> Lwt.return_false)

let read_file f =
  Lwt.catch
    (fun () ->
      let path, kind = get_props f in
      let* x = Lwt_io.chars_of_file path |> Lwt_stream.to_string in
      match kind with
      | Raw -> Lwt.return_some x
      | Trim -> Lwt.return_some (String.trim x))
    (fun _ -> Lwt.return_none)

let read_file_i18n ~lang f =
  let* f =
    let f' = Printf.sprintf "%s.%s" f lang in
    let* b = file_exists (Absolute f') in
    Lwt.return (if b then f' else f)
  in
  read_file (Absolute f)

let write_file f data =
  let fname, kind = get_props f in
  let fname_new = fname ^ ".new" in
  let* () =
    let open Lwt_io in
    let@ oc = with_file ~mode:Output fname_new in
    let* () = write oc data in
    match kind with Raw -> Lwt.return_unit | Trim -> write oc "\n"
  in
  Lwt_unix.rename fname_new fname

let mk_election_dir uuid = Lwt_unix.mkdir !!(Uuid.unwrap uuid) 0o700

let create_file f what lines =
  let fname = get_path f in
  Lwt_io.with_file
    ~flags:Unix.[ O_WRONLY; O_NONBLOCK; O_CREAT; O_TRUNC ]
    ~perm:0o600 ~mode:Lwt_io.Output fname
    (fun oc ->
      Lwt_list.iter_s
        (fun v ->
          let* () = Lwt_io.write oc (what v) in
          Lwt_io.write oc "\n")
        lines)

let create_whole_file f data =
  let fname = get_path f in
  Lwt_io.with_file
    ~flags:Unix.[ O_WRONLY; O_NONBLOCK; O_CREAT; O_TRUNC ]
    ~perm:0o600 ~mode:Lwt_io.Output fname
    (fun oc -> Lwt_io.write oc data)

let append_to_file f lines =
  let fname = get_path f in
  let open Lwt_io in
  let@ oc =
    with_file ~mode:Output ~flags:[ O_WRONLY; O_APPEND; O_CREAT ] fname
  in
  Lwt_list.iter_s (write_line oc) lines

let cleanup_file f =
  let f = get_path f in
  Lwt.catch (fun () -> Lwt_unix.unlink f) (fun _ -> Lwt.return_unit)

let rmdir dir =
  let command = ("rm", [| "rm"; "-rf"; dir |]) in
  let* _ = Lwt_process.exec command in
  Lwt.return_unit

let rm_election_dir uuid = rmdir !!(Uuid.unwrap uuid)

let exhaust_file file =
  let fname = file.Ocsigen_extensions.tmp_filename in
  let* result = Lwt_stream.to_string (Lwt_io.chars_of_file fname) in
  let* () = Lwt_unix.unlink fname in
  Lwt.return result

let copy_file src dst =
  let open Lwt_io in
  chars_of_file src |> chars_to_file dst

let try_copy_file src dst =
  let* b = file_exists (Absolute src) in
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
  let* state = read_file (Election (uuid, State)) in
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
    Lwt.return_some (get_path archive_name)
  else Lwt.return_none

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
