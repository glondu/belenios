(**************************************************************************)
(*                                BELENIOS                                *)
(*                                                                        *)
(*  Copyright © 2026-2026 Inria                                           *)
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

open Ppx_yojson_conv_lib.Yojson_conv
open Lwt.Syntax
open Belenios

type record = {
  username : string;
  salt : string;
  hashed : string;
  address : string option; [@yojson.option]
}
[@@deriving yojson]

let ( let*& ) x f =
  let* x = x in
  match x with None -> Lwt.return_none | Some x -> f x

let dir ~spool db =
  if db = "" || db.[0] = '.' || String.contains db '/' then
    Printf.ksprintf failwith "invalid password database name %S" db
  else spool // "passwords" // db

(* Layout of a database directory: [users] has one file per account,
   named after a hash of its lowercased username, and [addresses] has,
   for each address, a symbolic link named after a hash of the
   lowercased address and pointing to the file of the account. Links are
   read with readlink rather than followed, so that copies of the spool
   that do not preserve them (and contain stale copies of records
   instead) fail closed. *)

let key x = sha256_hex (String.lowercase_ascii x) ^ ".json"
let user_file dir u = dir // "users" // key u
let user_link u = "../users" // key u
let address_file dir a = dir // "addresses" // key a
let lowercase = Option.map String.lowercase_ascii

let read_record f =
  let*& x = Files.read_file f in
  Lwt.return_some (!*record_of_yojson x)

let get_by_username ~dir ~username = read_record (user_file dir username)

let get_by_address ~dir ~address =
  let*& target = Files.read_symlink (address_file dir address) in
  let*& r =
    if Filename.dirname target = "../users" then
      read_record (dir // "users" // Filename.basename target)
    else Lwt.return_none
  in
  if lowercase r.address = lowercase (Some address) then Lwt.return_some r
  else Lwt.return_none

let make_dirs dir =
  let* () = Files.mkdir (Filename.dirname dir) in
  let* () = Files.mkdir dir in
  let* () = Files.mkdir (dir // "users") in
  Files.mkdir (dir // "addresses")

let set ~dir r =
  let* () = make_dirs dir in
  let user_file = user_file dir r.username in
  let* old = read_record user_file in
  let old_address = Option.bind old (fun x -> x.address) in
  let* () = Files.write_file user_file (!+yojson_of_record r) in
  if lowercase old_address = lowercase r.address then Lwt.return_unit
  else
    let* () =
      match r.address with
      | Some a ->
          Files.write_symlink (address_file dir a) (user_link r.username)
      | None -> Lwt.return_unit
    in
    match old_address with
    | Some a ->
        (* stale links are ignored by lookups anyway *)
        let f = address_file dir a in
        let* x = Files.read_symlink f in
        if x = Some (user_link r.username) then Files.cleanup_file f
        else Lwt.return_unit
    | None -> Lwt.return_unit

let records_of_csv csv =
  split_lines csv
  |> List.map (fun line ->
      match String.split_on_char ',' line with
      | username :: salt :: hashed :: rest ->
          let address =
            match rest with a :: _ when a <> "" -> Some a | _ -> None
          in
          { username; salt; hashed; address }
      | _ -> Printf.ksprintf failwith "invalid CSV line: %S" line)

let csv_of_records records =
  records
  |> List.map (fun r ->
      let fields =
        [ r.username; r.salt; r.hashed ]
        @ match r.address with None -> [] | Some a -> [ a ]
      in
      (* Defensive check: records imported from CSV or written by the
         server cannot contain these characters (usernames and
         addresses are validated), but a hand-edited record could, and
         would then silently corrupt the CSV (e.g. a username with a
         newline would become an extra account on re-import). *)
      if
        List.exists
          (fun x ->
            String.exists (function ',' | '\n' | '\r' -> true | _ -> false) x)
          fields
      then Printf.ksprintf failwith "cannot export %S to CSV" r.username
      else String.concat "," fields)
  |> join_lines

let check_duplicates records =
  let check what xs =
    ignore
    @@ List.fold_left
         (fun seen x ->
           let x' = String.lowercase_ascii x in
           if SSet.mem x' seen then
             Printf.ksprintf failwith "duplicate %s: %S" what x
           else SSet.add x' seen)
         SSet.empty xs
  in
  check "username" (List.map (fun r -> r.username) records);
  check "address" (List.filter_map (fun r -> r.address) records)

let import ~spool ~db ~force records =
  check_duplicates records;
  let dir = dir ~spool db in
  let parent = Filename.dirname dir in
  let tmp = parent // ("." ^ db ^ ".new") in
  let old = parent // ("." ^ db ^ ".old") in
  let* exists = Files.file_exists dir in
  if exists && not force then
    Printf.ksprintf failwith "password database %s already exists" db
  else
    let* () = Files.mkdir parent in
    let remove_if_exists d =
      let* b = Files.file_exists d in
      if b then Files.remove_tree d else Lwt.return_unit
    in
    let* () = remove_if_exists tmp in
    let* () = remove_if_exists old in
    let* () = make_dirs tmp in
    let* () = Lwt_list.iter_s (fun r -> set ~dir:tmp r) records in
    let* () = if exists then Lwt_unix.rename dir old else Lwt.return_unit in
    let* () = Lwt_unix.rename tmp dir in
    let* () = Files.fsync_dir parent in
    remove_if_exists old

let export ~spool ~db =
  let dir = dir ~spool db in
  let* exists = Files.file_exists dir in
  if not exists then
    Printf.ksprintf failwith "password database %s does not exist" db
  else
    let* files =
      Lwt_unix.files_of_directory (dir // "users") |> Lwt_stream.to_list
    in
    let* records =
      Lwt_list.filter_map_s
        (fun f ->
          if Filename.check_suffix f ".json" then
            let*& x = Files.read_file (dir // "users" // f) in
            Lwt.return_some (!*record_of_yojson x)
          else Lwt.return_none)
        files
    in
    records
    |> List.sort (fun a b ->
        compare
          (String.lowercase_ascii a.username)
          (String.lowercase_ascii b.username))
    |> Lwt.return
