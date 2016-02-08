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

open Lwt
open Common
open Web_serializable_j
open Web_signatures
open Web_common

(** Global initialization *)

(* FIXME: the following should be in configuration file... but
   <maxrequestbodysize> doesn't work *)
let () = Ocsigen_config.set_maxrequestbodysizeinmemory 128000

let () = CalendarLib.Time_Zone.(change Local)

(** Parse configuration from <eliom> *)

let spool_dir = ref None
let import_dirs = ref []
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
  | Element ("import", ["dir", dir], []) ->
    import_dirs := dir :: !import_dirs
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

let ( / ) = Filename.concat

let file_exists x =
  try_lwt
    Lwt_unix.(access x [R_OK]) >>
    return true
  with _ ->
    return false

let read_election_dir dir =
  Lwt_io.chars_of_file (dir/"index.json") |>
  Lwt_stream.to_string >>=
  wrap1 datadir_index_of_string >>=
  Lwt_list.map_p (fun item ->
    let path = dir/item.datadir_dir in
    return {
      f_election = path/"election.json";
      f_metadata = path/"metadata.json";
      f_public_keys = path/"public_keys.jsons";
      f_public_creds = path/"public_creds.txt";
      f_voters = path/"voters.txt";
    }
  )

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
let () = Web_site_auth.configure (List.rev !auth_instances)

lwt () =
  Lwt_list.iter_s (fun dir ->
    read_election_dir dir >>=
    Lwt_list.iter_s (fun f ->
      match_lwt Web_site.import_election f [] with
      | None ->
        Ocsigen_messages.console (fun () ->
          Printf.sprintf "Ignored: %s" f.f_election
        ); return ()
      | Some w ->
        let module W = (val w : REGISTRABLE_ELECTION) in
        lwt _ = W.register () in
        return ()
    )
  ) !import_dirs
