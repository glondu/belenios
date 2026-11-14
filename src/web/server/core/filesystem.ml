(**************************************************************************)
(*                                BELENIOS                                *)
(*                                                                        *)
(*  Copyright © 2012-2024 Inria                                           *)
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

let read_file_i18n ~lang f =
  let* f =
    let f' = Printf.sprintf "%s.%s" f (Language.unwrap lang) in
    let* b = file_exists f' in
    Lwt.return (if b then f' else f)
  in
  read_file f
