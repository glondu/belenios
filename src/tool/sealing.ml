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

open Belenios_core.Serializable_builtin_t
open Belenios_core.Serializable_j
open Belenios_core.Common
open Common

let parse_config x =
  sealing_config_of_string x
  |> List.map
       (fun (r, i) ->
         Str.regexp r,
         List.fold_left (Fun.flip SSet.add) SSet.empty i
       )

let find_ign cfg path =
  List.filter_map
    (fun (r, i) ->
      if Str.string_match r path 0 then Some i else None
    ) cfg
  |> List.fold_left SSet.union SSet.empty

let rec measure full cfg ign path =
  let make name x = if SSet.mem name ign then None else Some x in
  let st = Unix.LargeFile.lstat path in
  let st_contents =
    if SSet.mem "contents" ign then (
      None
    ) else (
      match st.st_kind with
      | S_CHR | S_BLK | S_FIFO | S_SOCK -> None
      | S_REG -> Some (`REG (Hash.hash_string (string_of_file path)))
      | S_LNK -> Some (`LNK (Unix.readlink path))
      | S_DIR ->
         let files =
           Sys.readdir path
           |> Array.to_list
           |> List.filter (fun x -> x <> "." && x <> "..")
           |> List.sort String.compare
           |> List.map
                (fun x ->
                  let path = path // x in
                  let ign = find_ign cfg path in
                  (if full then path else x), measure full cfg ign path
                )
         in
         Some (`DIR files)
    )
  in
  let kind =
    match st.st_kind with
    | S_CHR -> `CHR
    | S_BLK -> `BLK
    | S_FIFO -> `FIFO
    | S_SOCK -> `SOCK
    | S_REG -> `REG
    | S_LNK -> `LNK
    | S_DIR -> `DIR
  in
  {
    st_dev = make "dev" st.st_dev;
    st_ino = make "ino" st.st_ino;
    st_kind = make "kind" kind;
    st_perm = make "perm" st.st_perm;
    st_nlink = make "nlink" st.st_nlink;
    st_uid = make "uid" st.st_uid;
    st_gid = make "gid" st.st_gid;
    st_rdev = make "rdev" st.st_rdev;
    st_size = make "size" st.st_size;
    st_atime = make "atime" st.st_atime;
    st_mtime = make "mtime" st.st_mtime;
    st_ctime = make "ctime" st.st_ctime;
    st_contents;
  }

let main full cfg path =
  let cfg =
    match cfg with
    | None -> []
    | Some x -> parse_config (string_of_file x)
  in
  let ign = find_ign cfg path in
  let x = measure full cfg ign path in
  print_endline (string_of_stats x);
 `Ok ()

open Cmdliner

let full_t =
  let doc = "Use full paths in directory contents." in
  let the_info = Arg.info ["full-paths"] ~doc in
  Arg.(value & flag the_info)

let cfg_t =
  let doc = "Read configuration from $(docv). It must be a JSON file with a single object mapping regular expressions to properties that should be ignored. Regular expressions must be in OCaml's Str format. Possible properties are: dev, ino, kind, perm, nlink, uid, gid, rdev, size, atime, mtime, ctime and contents." in
  let the_info = Arg.info ["config"] ~docv:"CONFIG" ~doc in
  Arg.(value & opt (some file) None the_info)

let path_t =
  let doc = "Measure path $(docv)." in
  let the_info = Arg.info ["path"] ~docv:"PATH" ~doc in
  Arg.(value & opt file Filename.current_dir_name the_info)

let cmd =
  let doc = "measure a directory" in
  let man =
    [
      `S "DESCRIPTION";
      `P "This command recursively reads all the files in a directory and outputs (on standard output) a canonical representation of its contents. Given a suitable configuration file, this output can be a measure of the integrity of the system.";
    ] @ common_man
  in
  Cmd.v (Cmd.info "measure" ~doc ~man)
    Term.(ret (const main $ full_t $ cfg_t $ path_t))

let cmds = [cmd]
