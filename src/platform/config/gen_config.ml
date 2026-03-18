(**************************************************************************)
(*                                BELENIOS                                *)
(*                                                                        *)
(*  Copyright © 2025-2025 Inria                                           *)
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

let get_version package =
  let cmd =
    ("ocamlfind", [| "ocamlfind"; "query"; "-format"; "%v"; package |])
  in
  Lwt_process.pread_line cmd

let make_version package =
  let* v = get_version package in
  match String.split_on_char '.' v with
  | a :: _ ->
      let a = Option.value ~default:0 (int_of_string_opt a) in
      Lwt.return [ Printf.sprintf "[%%%%define %s_version %d]" package a ]
  | _ -> Lwt.return_nil

let main () =
  let* jsoo = make_version "js_of_ocaml" in
  let* ocsigenserver = make_version "ocsigenserver" in
  [ jsoo; ocsigenserver ] |> List.flatten |> Lwt_stream.of_list
  |> Lwt_io.(write_lines stdout)

let () = main () |> Lwt_main.run
