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

let parse_config x =
  sealing_config_of_string x
  |> List.map (fun (r, i) ->
         (Re.Str.regexp r, List.fold_left (Fun.flip SSet.add) SSet.empty i))

let find_ign cfg path =
  List.filter_map
    (fun (r, i) -> if Re.Str.string_match r path 0 then Some i else None)
    cfg
  |> List.fold_left SSet.union SSet.empty

let rec measure pool full cfg ign path =
  let make name x = if SSet.mem name ign then None else Some x in
  let* st = Lwt_unix.LargeFile.lstat path in
  let* st_contents =
    if SSet.mem "contents" ign then Lwt.return_none
    else
      match st.st_kind with
      | S_CHR | S_BLK | S_FIFO | S_SOCK -> Lwt.return_none
      | S_REG ->
          let* contents =
            let@ () = Lwt_pool.use pool in
            let@ ic = Lwt_io.with_file ~mode:Input path in
            Lwt_io.read ic
          in
          Lwt.return_some @@ `REG (Hash.hash_string contents)
      | S_LNK ->
          let* contents = Lwt_unix.readlink path in
          Lwt.return_some @@ `LNK contents
      | S_DIR ->
          let* files =
            let@ () = Lwt_pool.use pool in
            Lwt_unix.files_of_directory path |> Lwt_stream.to_list
          in
          let files =
            files
            |> List.filter (fun x -> x <> "." && x <> "..")
            |> List.sort String.compare
          in
          let* files =
            files
            |> Lwt_list.map_p (fun x ->
                   let path = path // x in
                   let ign = find_ign cfg path in
                   let* r = measure pool full cfg ign path in
                   Lwt.return ((if full then path else x), r))
          in
          Lwt.return_some @@ `DIR files
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
  |> Lwt.return

let main full cfg path jobs =
  let@ () = wrap_main in
  let* cfg =
    match cfg with
    | None -> Lwt.return_nil
    | Some x ->
        let* x = string_of_file x in
        Lwt.return @@ parse_config x
  in
  let ign = find_ign cfg path in
  let pool = Lwt_pool.create jobs (fun () -> Lwt.return_unit) in
  let* x = measure pool full cfg ign path in
  let* () = Lwt_io.printl (string_of_stats x) in
  Lwt.return_unit

open Cmdliner

let full_t =
  let doc = "Use full paths in directory contents." in
  let the_info = Arg.info [ "full-paths" ] ~doc in
  Arg.(value & flag the_info)

let cfg_t =
  let doc =
    "Read configuration from $(docv). It must be a JSON file with a single \
     object mapping regular expressions to properties that should be ignored. \
     Regular expressions must be in OCaml's Str format. Possible properties \
     are: dev, ino, kind, perm, nlink, uid, gid, rdev, size, atime, mtime, \
     ctime and contents."
  in
  let the_info = Arg.info [ "config" ] ~docv:"CONFIG" ~doc in
  Arg.(value & opt (some file) None the_info)

let path_t =
  let doc = "Measure path $(docv)." in
  let the_info = Arg.info [ "path" ] ~docv:"PATH" ~doc in
  Arg.(value & opt file Filename.current_dir_name the_info)

let jobs_t =
  let doc = "Use up to $(docv) parallel I/O jobs" in
  let the_info = Arg.info [ "jobs" ] ~docv:"JOBS" ~doc in
  Arg.(value & opt int 1 the_info)

let cmd =
  let doc = "measure a directory" in
  let man =
    [
      `S "DESCRIPTION";
      `P
        "This command recursively reads all the files in a directory and \
         outputs (on standard output) a canonical representation of its \
         contents. Given a suitable configuration file, this output can be a \
         measure of the integrity of the system.";
    ]
    @ common_man
  in
  Cmd.v
    (Cmd.info "measure" ~doc ~man)
    Term.(ret (const main $ full_t $ cfg_t $ path_t $ jobs_t))
