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

let lines_of_file fname =
  let ic = open_in fname in
  let rec loop accu =
    match input_line ic with
    | line -> loop (line :: accu)
    | exception End_of_file -> close_in ic; List.rev accu
  in
  loop []

let string_of_file f =
  lines_of_file f |> String.concat "\n"

let load_from_file of_string filename =
  if Sys.file_exists filename then (
    Printf.eprintf "I: loading %s...\n%!" (Filename.basename filename);
    Some (lines_of_file filename |> List.rev_map of_string)
  ) else None

exception Cmdline_error of string

let failcmd fmt = Printf.ksprintf (fun x -> raise (Cmdline_error x)) fmt
