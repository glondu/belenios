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

open Lwt.Syntax
open Belenios
open Common

type verifydiff_error = NotPrefix | ErrorInFirst

exception VerifydiffError of verifydiff_error

let explain_error = function
  | NotPrefix -> "first is not a prefix of second"
  | ErrorInFirst -> "error in first"

let () =
  Printexc.register_printer (function
    | VerifydiffError e -> Some ("verify-diff error: " ^ explain_error e)
    | _ -> None)

let verifydiff dir1 dir2 =
  let* bel1 = find_bel_in_dir dir1 in
  let* bel2 = find_bel_in_dir dir2 in
  let file1 = dir1 // bel1 in
  let file2 = dir2 // bel2 in
  let* () =
    let open Tool_events in
    let* index1 = get_index ~file:file1 in
    let* index2 = get_index ~file:file2 in
    let* b =
      Lwt.try_bind
        (fun () -> fsck index1)
        (fun () -> Lwt.return_false)
        (fun _ -> Lwt.return_true)
    in
    if b then raise (VerifydiffError ErrorInFirst);
    if not (starts_with ~prefix:index1 index2) then
      raise (VerifydiffError NotPrefix);
    Lwt.return_unit
  in
  let* x = Tool_election.make file2 in
  let module X = (val x) in
  X.verify ()
