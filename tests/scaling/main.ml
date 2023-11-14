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

let cmds = [ Setup.cmd; Vote.cmd ]

let default_cmd =
  let open Belenios_platform.Version in
  let version = Printf.sprintf "%s (%s)" version build in
  let version = if debug then version ^ " [debug]" else version in
  let doc = "Belenios scaling test tool" in
  let man = [] in
  ( Term.(ret (const (`Help (`Pager, None)))),
    Cmd.info "belenios-scaling-test" ~version ~doc ~man )

let root_cmd =
  let default, i = default_cmd in
  Cmd.(group ~default i cmds)

let () = exit (Cmd.eval root_cmd)
