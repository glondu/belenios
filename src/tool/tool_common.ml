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

exception Cmdline_error of string

let failcmd fmt = Printf.ksprintf (fun x -> raise (Cmdline_error x)) fmt

let common_man = [
  `S "MORE INFORMATION";
  `P "This command is part of the Belenios command-line tool.";
  `P "See $(i,http://belenios.gforge.inria.fr/).";
]

let get_mandatory_opt name = function
  | Some x -> x
  | None -> failcmd "%s is mandatory" name

let wrap_main f =
  try
    let () = f () in `Ok ()
  with
  | Cmdline_error e -> `Error (true, e)
  | Failure e -> `Error (false, e)
  | e -> `Error (false, Printexc.to_string e)

let group_c =
  (fun fname ->
    if Sys.file_exists fname then (
      try
        let ic = open_in fname in
        let ls = Yojson.init_lexer () in
        let lb = Lexing.from_channel ic in
        let r = Group.read ls lb in
        close_in ic;
        `Ok (fname, r)
      with e ->
        let e = Printexc.to_string e and s = Printf.sprintf in
        `Error (s "could not read group parameters from %s (%s)" fname e)
    ) else `Error (Printf.sprintf "file %s does not exist" fname)
  ), (fun fmt (fname, _) -> Format.pp_print_string fmt fname)

let uuid_c =
  (fun u ->
    match Uuidm.of_string u with
    | Some uuid -> `Ok uuid
    | None -> `Error (Printf.sprintf "%s is not a valid UUID" u)
  ), (fun fmt u -> Format.pp_print_string fmt (Uuidm.to_string u))

open Cmdliner

let group_t =
  let doc = "Take group parameters from file $(docv)." in
  Arg.(value & opt (some group_c) None & info ["group"] ~docv:"GROUP" ~doc)

let uuid_t =
  let doc = "UUID of the election." in
  Arg.(value & opt (some uuid_c) None & info ["uuid"] ~docv:"UUID" ~doc)
