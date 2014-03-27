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
open Serializable_t
open Signatures
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

let import_dirs = ref []
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
  | Element ("import", ["dir", dir], []) ->
    import_dirs := dir :: !import_dirs
  | Element ("rewrite-prefix", ["src", src; "dst", dst], []) ->
    set_rewrite_prefix ~src ~dst
  | Element ("main-election", ["uuid", uuid], []) ->
    (match  Uuidm.of_string uuid with
    | Some u -> main_election_uuid := Some (Uuidm.to_string u)
    | None -> failwith "Incorrect UUID in configuration <main-election> tag"
    )
  | Element ("auth", ["name", auth_instance],
             [Element (auth_system, auth_config, [])]) ->
    let open Web_auth in
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

let election_table = Ocsipersist.open_table "elections"

let import_election_dir accu dir =
  Ocsigen_messages.debug (fun () ->
    "Importing data from " ^ dir ^ "..."
  );
  lwt index =
    Lwt_io.chars_of_file (dir/"index.json") |>
    Lwt_stream.to_string >>=
    wrap1 datadir_index_of_string
  in
  Lwt_list.fold_left_s (fun accu item ->
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
    let params = Group.election_params_of_string raw_election in
    let module P = (val params : ELECTION_PARAMS) in
    let uuid = Uuidm.to_string P.params.e_uuid in
    lwt exists =
      try_lwt
        lwt _ = Ocsipersist.find election_table uuid in
        return true
      with Not_found -> return false
    in
    if exists then (
      let () = Ocsigen_messages.debug (fun () ->
        "-- election already present in database, skipping"
      ) in return accu
    ) else if SMap.mem uuid accu then (
      let () = Ocsigen_messages.debug (fun () ->
        "-- duplicate election, skipping"
      ) in return accu
    ) else (
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
      let module X = struct
        let metadata = metadata
        let featured = item.datadir_featured
        let params_fname = params_fname
        let public_keys_fname = public_keys_fname
      end in
      let web_params = (module X : WEB_PARAMS) in
      Ocsipersist.add election_table uuid (raw_election, web_params) >>
      return @@ SMap.add uuid public_creds_fname accu
    )
  ) accu index

lwt imported =
  Lwt_list.fold_left_s import_election_dir SMap.empty !import_dirs

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
  let auth_config = !auth_instances
end

module Site = Web_site.Make (Site_config)

lwt () =
  Ocsipersist.iter_step (fun uuid (raw_election, web_params) ->
    let params = Group.election_params_of_string raw_election in
    let module P = (val params : ELECTION_PARAMS) in
    let module D = struct
      module G = P.G
      let election = {
        e_params = P.params;
        e_pks = None;
        e_fingerprint = P.fingerprint;
      }
    end in
    let election_data = (module D : ELECTION_DATA) in
    lwt election = Site.register_election election_data web_params in
    let module W = (val election : WEB_ELECTION) in
    (match !main_election_uuid with
    | Some u when u = uuid -> Site.set_main_election election
    | _ -> ()
    );
    try_lwt
      let public_creds_fname = SMap.find uuid imported in
      (* if the election has just been imported, inject its credentials *)
      let () =
        Ocsigen_messages.debug (fun () ->
          Printf.sprintf "Injecting credentials for %s" uuid
        )
      in
      Lwt_io.lines_of_file public_creds_fname |>
      Lwt_stream.iter_s W.B.inject_cred
    with Not_found -> return ()
  ) election_table
