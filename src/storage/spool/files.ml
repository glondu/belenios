(**************************************************************************)
(*                                BELENIOS                                *)
(*                                                                        *)
(*  Copyright © 2012-2026 Inria                                           *)
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

let file_exists f =
  Lwt.try_bind
    (fun () -> Lwt_unix.(access f [ R_OK ]))
    (fun () -> Lwt.return_true)
    (fun _ -> Lwt.return_false)

let read_file f =
  Lwt.catch
    (fun () ->
      let* f = Lwt_io.chars_of_file f |> Lwt_stream.to_string in
      Lwt.return_some f)
    (fun _ -> Lwt.return_none)

let fsync_dir dir =
  let open Lwt_unix in
  let* fd = openfile dir [ O_RDONLY; O_CLOEXEC ] 0 in
  Lwt.finalize (fun () -> fsync fd) (fun () -> close fd)

(* The new contents and the rename must be on disk before returning:
   callers acknowledge operations (e.g. ballot casting) once this
   returns. *)
let write_file f x =
  let fnew = f ^ ".new" in
  let* () =
    let open Lwt_unix in
    let* fd = openfile fnew [ O_WRONLY; O_CREAT; O_TRUNC; O_CLOEXEC ] 0o600 in
    Lwt.finalize
      (fun () ->
        let oc = Lwt_io.of_fd ~mode:Output fd in
        let* () = Lwt_io.write oc x in
        let* () = Lwt_io.flush oc in
        fsync fd)
      (fun () -> close fd)
  in
  let* () = Lwt_unix.rename fnew f in
  fsync_dir (Filename.dirname f)

let cleanup_file f =
  Lwt.catch (fun () -> Lwt_unix.unlink f) (fun _ -> Lwt.return_unit)

let write_symlink f target =
  let fnew = f ^ ".new" in
  let* () = cleanup_file fnew in
  let* () = Lwt_unix.symlink target fnew in
  let* () = Lwt_unix.rename fnew f in
  fsync_dir (Filename.dirname f)

let read_symlink f =
  Lwt.catch
    (fun () ->
      let* x = Lwt_unix.readlink f in
      Lwt.return_some x)
    (fun _ -> Lwt.return_none)

let mkdir dir =
  Lwt.try_bind
    (fun () -> Lwt_unix.mkdir dir 0o700)
    (fun () -> fsync_dir (Filename.dirname dir))
    (function
      | Unix.Unix_error (Unix.EEXIST, _, _) -> Lwt.return_unit
      | e -> Lwt.reraise e)

let rec remove_tree_exn dir =
  let* xs = Lwt_unix.files_of_directory dir |> Lwt_stream.to_list in
  let* () =
    Lwt_list.iter_s
      (fun x ->
        if x = "." || x = ".." then Lwt.return_unit
        else
          let f = Filename.concat dir x in
          let* st = Lwt_unix.lstat f in
          match st.st_kind with
          | S_DIR -> remove_tree_exn f
          | _ -> Lwt_unix.unlink f)
      xs
  in
  Lwt_unix.rmdir dir

let remove_tree dir =
  Lwt.try_bind
    (fun () -> Lwt_unix.lstat dir)
    (fun _ -> remove_tree_exn dir)
    (function
      | Unix.Unix_error (Unix.ENOENT, _, _) -> Lwt.return_unit
      | e -> Lwt.reraise e)
