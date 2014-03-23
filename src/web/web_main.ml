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

open Signatures
open Util
open Serializable_t
open Web_serializable_j
open Lwt
open Web_common
open Web_signatures

(** Global initialization *)

(* FIXME: the following should be in configuration file... but
   <maxrequestbodysize> doesn't work *)
let () = Ocsigen_config.set_maxrequestbodysizeinmemory 128000

let () = CalendarLib.Time_Zone.(change Local)

(** Parse configuration from <eliom> *)

let data_dirs = ref []
let source_file = ref None
let main_election_uuid = ref None
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
  | Element ("data", ["dir", dir], []) ->
    data_dirs := dir :: !data_dirs
  | Element ("rewrite-prefix", ["src", src; "dst", dst], []) ->
    set_rewrite_prefix ~src ~dst
  | Element ("main-election", ["uuid", uuid], []) ->
    (match  Uuidm.of_string uuid with
    | Some u -> main_election_uuid := Some u
    | None -> failwith "Incorrect UUID in configuration <main-election> tag"
    )
  | Element ("auth", ["name", auth_instance],
             [Element (auth_system, auth_config, [])]) ->
    let open Auth_common in
    let i = {auth_system; auth_instance; auth_config} in
    auth_instances := i :: !auth_instances
  | Element (tag, _, _) ->
    Printf.ksprintf failwith
      "invalid configuration for tag %s in belenios"
      tag

(** Parse configuration from other sources *)

let get_single_line x =
  match_lwt Lwt_stream.get x with
  | None -> return None
  | Some _ as line ->
    lwt b = Lwt_stream.is_empty x in
    if b then (
      return line
    ) else (
      Lwt_stream.junk_while (fun _ -> true) x >>
      return None
    )

let ( / ) = Filename.concat

let file_exists x =
  try_lwt
    Lwt_unix.(access x [R_OK]) >>
    return true
  with _ ->
    return false

let parse_election_dir dir =
  Ocsigen_messages.debug (fun () ->
    "Loading data from " ^ dir ^ "..."
  );
  lwt index =
    Lwt_io.chars_of_file (dir/"index.json") |>
    Lwt_stream.to_string >>=
    wrap1 datadir_index_of_string
  in
  Lwt_list.map_p (fun item ->
    let subdir = item.datadir_dir in
    let path = dir/subdir in
    let params_fname = path/"election.json" in
    let public_keys_fname = path/"public_keys.jsons" in
    Ocsigen_messages.debug (fun () ->
      "-- loading " ^ subdir
    );
    lwt raw_election =
      Lwt_io.lines_of_file params_fname |>
      get_single_line >>=
      (function
      | Some e -> return e
      | None -> failwith "election.json is invalid")
    in
    lwt metadata =
      let fname = path/"metadata.json" in
      lwt b = file_exists fname in
      if b then (
        Lwt_io.chars_of_file fname |>
        Lwt_stream.to_string >>=
        wrap1 metadata_of_string
      ) else return empty_metadata
    in
    let public_creds_fname = path/"public_creds.txt" in
    (* public credentials will be parsed later *)
    return Web_election.({
      raw_election;
      metadata;
      featured = item.datadir_featured;
      params_fname;
      public_keys_fname;
    }, public_creds_fname)
  ) index

lwt election_configs =
  Lwt_list.map_p parse_election_dir !data_dirs >>=
  wrap1 List.flatten

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

(** Build up the site *)

module Site_config = struct
  let name = "site"
  let path = []
  let source_file = source_file
  let instances = !auth_instances
end

module Site = Web_site.Make (Site_config)

let populate accu f s = Lwt_stream.fold_s f s accu

lwt () =
  Lwt_list.iter_s (fun (config, public_creds_fname) ->
    lwt election = Site.register_election config in
    let module W = (val election : WEB_ELECTION) in
    (match !main_election_uuid with
    | Some u when u = W.election.e_params.e_uuid ->
      Site.set_main_election election
    | _ -> ()
    );
    lwt public_creds =
      Lwt_io.lines_of_file public_creds_fname |>
      populate SSet.empty (fun c accu ->
        return (SSet.add c accu)
      )
    in
    W.B.inject_creds public_creds >>
    return ()
  ) election_configs
