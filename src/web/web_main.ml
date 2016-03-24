(**************************************************************************)
(*                                BELENIOS                                *)
(*                                                                        *)
(*  Copyright © 2012-2016 Inria                                           *)
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

open Lwt
open Common
open Web_serializable_j
open Web_common

(** Global initialization *)

(* FIXME: the following should be in configuration file... but
   <maxrequestbodysize> doesn't work *)
let () = Ocsigen_config.set_maxrequestbodysizeinmemory 128000

let () = CalendarLib.Time_Zone.(change Local)

(** Parse configuration from <eliom> *)

let spool_dir = ref None
let source_file = ref None
let auth_instances = ref []

let () =
  Eliom_config.get_config () |>
  let open Simplexmlparser in
  List.iter @@ function
  | PCData x ->
    Ocsigen_extensions.Configuration.ignore_blank_pcdata ~in_tag:"belenios" x
  | Element ("log", ["file", file], []) ->
    Lwt_main.run (open_security_log file)
  | Element ("source", ["file", file], []) ->
    source_file := Some file
  | Element ("spool", ["dir", dir], []) ->
    spool_dir := Some dir
  | Element ("rewrite-prefix", ["src", src; "dst", dst], []) ->
    set_rewrite_prefix ~src ~dst
  | Element ("auth", ["name", auth_instance],
             [Element (auth_system, auth_config, [])]) ->
    let i = {auth_system; auth_instance; auth_config} in
    auth_instances := i :: !auth_instances
  | Element (tag, _, _) ->
    Printf.ksprintf failwith
      "invalid configuration for tag %s in belenios"
      tag

(** Parse configuration from other sources *)

let file_exists x =
  try_lwt
    Lwt_unix.(access x [R_OK]) >>
    return true
  with _ ->
    return false

lwt source_file =
  match !source_file with
  | Some f ->
    lwt b = file_exists f in
    if b then (
      return f
    ) else (
      Printf.ksprintf failwith "file %s does not exist" f
    )
  | None -> failwith "missing <source> in configuration"

let spool_dir =
  match !spool_dir with
  | Some d -> d
  | None -> failwith "missing <spool> in configuration"

(** Build up the site *)

let () = Web_site.source_file := source_file
let () = Web_common.spool_dir := spool_dir
let () = Web_auth.configure (List.rev !auth_instances)
