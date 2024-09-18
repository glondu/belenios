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

let get_last_event dir =
  let rec loop ((accu_event, accu_files) as accu) = function
    | [] -> Lwt.return accu
    | file :: files ->
        let accu_files = SSet.add file accu_files in
        let* accu_event =
          if Filename.check_suffix file ".event.json" then
            let* event_s = string_of_file (dir // file) in
            let event = event_of_string event_s in
            match accu_event with
            | Some old when old.event_height > event.event_height ->
                Lwt.return accu_event
            | _ -> Lwt.return_some event
          else Lwt.return accu_event
        in
        loop (accu_event, accu_files) files
  in
  let* files = Lwt_unix.files_of_directory dir |> Lwt_stream.to_list in
  loop (None, SSet.empty) files

let mkarchive dir =
  let* header =
    let* x = string_of_file (dir // "BELENIOS") in
    Lwt.return @@ archive_header_of_string x
  in
  let* last_event, files = get_last_event dir in
  let last_event =
    match last_event with Some x -> x | None -> failwith "no events found"
  in
  let module IoArchiver = struct
    include Tool_events.LwtMonad

    let get_hash hash =
      let hash_s = Hash.to_hex hash in
      let event_filename = hash_s ^ ".event.json" in
      let data_filename = hash_s ^ ".data.json" in
      if SSet.mem event_filename files then
        let* contents = string_of_file (dir // event_filename) in
        Lwt.return_some contents
      else if SSet.mem data_filename files then
        let* contents = string_of_file (dir // data_filename) in
        Lwt.return_some contents
      else Lwt.return_none
  end in
  let module Archiver = Archive.MakeArchiver (IoArchiver) (Tool_events.Writer)
  in
  let oc : Tool_events.file = { pos = 0L; fd = Lwt_unix.stdout } in
  Archiver.write_archive oc header last_event

let main dir =
  let@ () = wrap_main in
  mkarchive dir

open Cmdliner

let dir_t =
  let doc = "Read objects from directory $(docv)." in
  let the_info = Arg.info [ "dir" ] ~docv:"DIR" ~doc in
  Arg.(value & opt dir Filename.current_dir_name the_info)

let cmd =
  let doc = "re-create an archive from an extracted archive" in
  let man =
    [
      `S "DESCRIPTION";
      `P
        "This command reads files from an extracted $(i,UUID.bel) and outputs \
         (on standard output) a canonical archive containing the same files.";
    ]
    @ common_man
  in
  Cmd.v (Cmd.info "make" ~doc ~man) Term.(ret (const main $ dir_t))
