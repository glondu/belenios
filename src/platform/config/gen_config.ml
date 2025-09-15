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

let jsoo_version () =
  let cmd = ("js_of_ocaml", [| "js_of_ocaml"; "--version" |]) in
  Lwt_process.pread_line cmd

let main () =
  let* jsoo_version =
    let* v = jsoo_version () in
    match String.split_on_char '.' v with
    | a :: b :: c :: _ ->
        let a = Option.value ~default:0 (int_of_string_opt a) in
        let b = Option.value ~default:0 (int_of_string_opt b) in
        let c = Option.value ~default:0 (int_of_string_opt c) in
        Lwt.return
          [ Printf.sprintf "[%%%%define jsoo_version (%d, %d, %d)]" a b c ]
    | _ -> Lwt.return_nil
  in
  [ jsoo_version ] |> List.flatten |> Lwt_stream.of_list
  |> Lwt_io.(write_lines stdout)

let () = main () |> Lwt_main.run
