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

let files_of_directory d =
  let* all = Lwt_unix.files_of_directory d |> Lwt_stream.to_list in
  Lwt.return @@ List.filter (fun x -> x <> "." && x <> "..") all

let file_exists x =
  Lwt.catch
    (fun () ->
      let* () = Lwt_unix.(access x [ R_OK ]) in
      Lwt.return_true)
    (fun _ -> Lwt.return_false)

let get_fname uuid x = match uuid with None -> x | Some uuid -> uuid /// x

let read_file ?uuid x =
  Lwt.catch
    (fun () ->
      let* lines =
        Lwt_io.lines_of_file (get_fname uuid x) |> Lwt_stream.to_list
      in
      Lwt.return_some lines)
    (fun _ -> Lwt.return_none)

let read_whole_file ?uuid x =
  Lwt.catch
    (fun () ->
      let* x =
        Lwt_io.chars_of_file (get_fname uuid x) |> Lwt_stream.to_string
      in
      Lwt.return_some x)
    (fun _ -> Lwt.return_none)

let read_whole_file_i18n ~lang f =
  let* f =
    let f' = Printf.sprintf "%s.%s" f lang in
    let* b = file_exists f' in
    Lwt.return (if b then f' else f)
  in
  read_whole_file f

let read_file_single_line ?uuid filename =
  let* x = read_file ?uuid filename in
  match x with Some [ x ] -> Lwt.return_some x | _ -> Lwt.return_none

let write_file ?uuid x lines =
  let fname = get_fname uuid x in
  let fname_new = fname ^ ".new" in
  let* () =
    let open Lwt_io in
    let@ oc = with_file ~mode:Output fname_new in
    Lwt_list.iter_s (write_line oc) lines
  in
  Lwt_unix.rename fname_new fname

let write_whole_file ?uuid x data =
  let fname = get_fname uuid x in
  let fname_new = fname ^ ".new" in
  let* () =
    let open Lwt_io in
    let@ oc = with_file ~mode:Output fname_new in
    write oc data
  in
  Lwt_unix.rename fname_new fname

let create_file ~uuid x what lines =
  let fname = get_fname (Some uuid) x in
  Lwt_io.with_file
    ~flags:Unix.[ O_WRONLY; O_NONBLOCK; O_CREAT; O_TRUNC ]
    ~perm:0o600 ~mode:Lwt_io.Output fname
    (fun oc ->
      Lwt_list.iter_s
        (fun v ->
          let* () = Lwt_io.write oc (what v) in
          Lwt_io.write oc "\n")
        lines)

let create_whole_file ~uuid x data =
  let fname = get_fname (Some uuid) x in
  Lwt_io.with_file
    ~flags:Unix.[ O_WRONLY; O_NONBLOCK; O_CREAT; O_TRUNC ]
    ~perm:0o600 ~mode:Lwt_io.Output fname
    (fun oc -> Lwt_io.write oc data)

let append_to_file ?uuid x lines =
  let fname = get_fname uuid x in
  let open Lwt_io in
  let@ oc =
    with_file ~mode:Output ~flags:[ O_WRONLY; O_APPEND; O_CREAT ] fname
  in
  Lwt_list.iter_s (write_line oc) lines

let cleanup_file f =
  Lwt.catch (fun () -> Lwt_unix.unlink f) (fun _ -> Lwt.return_unit)

let rmdir dir =
  let command = ("rm", [| "rm"; "-rf"; dir |]) in
  let* _ = Lwt_process.exec command in
  Lwt.return_unit

let exhaust_file file =
  let fname = file.Ocsigen_extensions.tmp_filename in
  let* result = Lwt_stream.to_string (Lwt_io.chars_of_file fname) in
  let* () = Lwt_unix.unlink fname in
  Lwt.return result

let copy_file src dst =
  let open Lwt_io in
  chars_of_file src |> chars_to_file dst

let try_copy_file src dst =
  let* b = file_exists src in
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
  let* state = read_file_single_line ~uuid "state.json" in
  let final =
    match state with
    | None -> true
    | Some x -> (
        match Web_serializable_j.election_state_of_string x with
        | `Tallied | `Archived -> true
        | _ -> false)
  in
  if final then
    let archive_name = uuid /// "archive.zip" in
    let* b = file_exists archive_name in
    let* () = if not b then make_archive uuid else Lwt.return_unit in
    Lwt.return_some archive_name
  else Lwt.return_none
