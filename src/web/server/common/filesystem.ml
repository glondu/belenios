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
open Belenios_core.Common
open Web_common

let file_exists x =
  Lwt.catch
    (fun () ->
      let* () = Lwt_unix.(access x [R_OK]) in
      Lwt.return_true
    )
    (fun _ -> Lwt.return_false)

let get_fname uuid x =
  match uuid with
  | None -> x
  | Some uuid -> uuid /// x

let read_file ?uuid x =
  Lwt.catch
    (fun () ->
      let* lines = Lwt_io.lines_of_file (get_fname uuid x) |> Lwt_stream.to_list in
      Lwt.return_some lines
    )
    (fun _ -> Lwt.return_none)

let read_whole_file ?uuid x =
  Lwt.catch
    (fun () ->
      let* x = Lwt_io.chars_of_file (get_fname uuid x) |> Lwt_stream.to_string in
      Lwt.return_some x
    )
    (fun _ -> Lwt.return_none)

let read_file_single_line ?uuid filename =
  let* x = read_file ?uuid filename in
  match x with
  | Some [x] -> Lwt.return_some x
  | _ -> Lwt.return_none

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

let cleanup_file f =
  Lwt.catch
    (fun () -> Lwt_unix.unlink f)
    (fun _ -> Lwt.return_unit)

let rmdir dir =
  let command = "rm", [| "rm"; "-rf"; dir |] in
  let* _ = Lwt_process.exec command in
  Lwt.return_unit
