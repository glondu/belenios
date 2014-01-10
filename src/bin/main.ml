(**************************************************************************)
(*                                BELENIOS                                *)
(*                                                                        *)
(*  Copyright © 2012-2014 Inria                                           *)
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

let usage () =
  let cmd = Sys.argv.(0) in
  Printf.eprintf
    "Usage: %s { trustee-keygen | election | credgen } [--help] [<args>]
To get help for a specific command, run: %s <command> --help\n"
    cmd cmd;
  exit 1


let () =
  let n = Array.length Sys.argv in
  if n < 2 then usage ()
  else (
    Arg.current := 1;
    match Sys.argv.(1) with
    | "trustee-keygen" -> Tkeygen.main ()
    | "election" -> Election_tool.main ()
    | "credgen" -> Credgen.main ()
    | _ -> usage ()
  )
