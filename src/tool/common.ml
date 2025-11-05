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

let printl1 x =
  let* x = x in
  Lwt_io.printl x

let printl2 x =
  let* a, b = x in
  let* () = Lwt_io.printl a in
  Lwt_io.printl b

let lines_of_stdin () = Lwt_io.(read_lines stdin) |> Lwt_stream.to_list
let chars_of_stdin () = Lwt_io.(read stdin)

let download_archive =
  (* forward reference set in Tool_archive_pull *)
  ref (fun _ _ -> Lwt.return_unit)

let download dir url uuid =
  let url = if String.ends_with ~suffix:"/" url then url else url ^ "/" in
  let file = Printf.sprintf "%s.bel" (Uuid.unwrap uuid) in
  let* () =
    Lwt_io.eprintf "I: downloading %s from %s to %s...\n" (Uuid.unwrap uuid) url
      file
  in
  let* () = Lwt_io.(flush stderr) in
  let* oc =
    Lwt_unix.openfile (dir // file) [ O_WRONLY; O_CREAT; O_TRUNC ] 0o644
  in
  Lwt.catch
    (fun () ->
      let url = Printf.sprintf "%sapi/elections/%s" url (Uuid.unwrap uuid) in
      let* () = !download_archive url oc in
      let* () = Lwt_unix.close oc in
      Lwt.return_some file)
    (fun e ->
      let* () = Lwt_unix.close oc in
      let* () = Lwt_io.eprintf "E: %s\n" (Printexc.to_string e) in
      Lwt.return_none)

exception Cmdline_error of string

let failcmd fmt = Printf.ksprintf (fun x -> raise (Cmdline_error x)) fmt

let get_mandatory_opt name = function
  | Some x -> x
  | None -> failcmd "%s is mandatory" name

let key_value_list_of_json = function
  | `Assoc x as json ->
      x
      |> List.map (function
           | a, `String b -> (a, b)
           | _ ->
               failcmd "%s has not expected JSON type"
                 (Yojson.Safe.to_string json))
  | json ->
      failcmd "%s is not a proper JSON object" (Yojson.Safe.to_string json)

let lines_of_file fname = Lwt_io.lines_of_file fname |> Lwt_stream.to_list

let string_of_file f =
  let open Lwt_io in
  let@ ic = with_file ~mode:Input f in
  let* contents = read ic in
  Lwt.return @@ String.trim contents

let load_from_file of_string filename =
  let* b = Lwt_unix.file_exists filename in
  if b then
    let* () = Lwt_io.eprintlf "I: loading %s..." (Filename.basename filename) in
    let* lines = lines_of_file filename in
    Lwt.return_some @@ List.map of_string lines
  else Lwt.return_none

let find_bel_in_dir ?uuid dir =
  match uuid with
  | Some uuid -> Lwt.return @@ Printf.sprintf "%s.bel" (Uuid.unwrap uuid)
  | None -> (
      let* files = Lwt_stream.to_list @@ Lwt_unix.files_of_directory dir in
      match List.filter (fun x -> Filename.check_suffix x ".bel") files with
      | [ file ] -> Lwt.return file
      | _ ->
          Printf.ksprintf failwith
            "directory %s must contain a single .bel file" dir)

let wrap_main f =
  match Lwt_main.run @@ f () with
  | () -> `Ok ()
  | exception Cmdline_error e -> `Error (true, e)
  | exception Failure e -> `Error (false, e)
  | exception e -> `Error (false, Printexc.to_string e)

let common_man =
  [
    `S "MORE INFORMATION";
    `P "This command is part of the Belenios command-line tool.";
    `P "To get more help on a specific subcommand, run:";
    `P "$(b,belenios-tool) $(i,COMMAND) $(b,--help)";
    `P "See $(i,https://www.belenios.net/).";
  ]

open Cmdliner

module type CMDLINER_MODULE = sig
  val cmd : unit Cmd.t
end

let dir_t, optdir_t =
  let doc = "Use directory $(docv) for reading and writing election files." in
  let the_info = Arg.info [ "dir" ] ~docv:"DIR" ~doc in
  ( Arg.(value & opt dir Filename.current_dir_name the_info),
    Arg.(value & opt (some dir) None the_info) )

let uuid_t =
  let doc = "Election UUID." in
  let the_info = Arg.info [ "uuid" ] ~docv:"UUID" ~doc in
  Arg.(value & opt (some string) None the_info)

let url_t =
  let doc = "Download election files from $(docv)." in
  let the_info = Arg.info [ "url" ] ~docv:"URL" ~doc in
  Arg.(value & opt (some string) None the_info)

let key_t =
  let doc = "Read private key from file $(docv)." in
  let the_info = Arg.info [ "key" ] ~docv:"KEY" ~doc in
  Arg.(value & opt (some file) None the_info)

let default_version =
  let open Belenios.Election in
  let (Version v) = List.hd supported_crypto_versions in
  int_of_version v
