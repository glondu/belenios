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
