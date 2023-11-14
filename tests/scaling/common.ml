(**************************************************************************)
(*                                BELENIOS                                *)
(*                                                                        *)
(*  Copyright © 2023-2023 Inria                                           *)
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

open Cmdliner

let url_t =
  let doc = "Use server at address $(docv)." in
  Arg.(value & opt (some string) None & info [ "url" ] ~docv:"URL" ~doc)

let get_mandatory what = function
  | None -> Printf.ksprintf failwith "missing %s" what
  | Some x -> x

let wrap_main f =
  match f () with
  | () -> `Ok ()
  | exception Failure e -> `Error (false, e)
  | exception e -> `Error (false, Printexc.to_string e)
