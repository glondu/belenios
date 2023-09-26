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

open Belenios_core.Common

let print_endline2 (a, b) =
  print_endline a;
  print_endline b

let lines_of_stdin () =
  let rec loop accu =
    match input_line stdin with
    | line -> loop (line :: accu)
    | exception End_of_file -> List.rev accu
  in
  loop []

let chars_of_stdin () =
  let buf = Buffer.create 1024 in
  let rec loop () =
    match input_char stdin with
    | c ->
        Buffer.add_char buf c;
        loop ()
    | exception End_of_file -> ()
  in
  loop ();
  Buffer.contents buf

let download dir url =
  let url, file =
    match String.split_on_char '/' url |> List.rev with
    | "" :: uuid :: _ -> (url, uuid ^ ".bel")
    | last :: rest -> (
        match Filename.chop_suffix_opt ~suffix:".bel" last with
        | None -> (url ^ "/", last ^ ".bel")
        | Some uuid -> (String.concat "/" (List.rev ("" :: rest)), uuid ^ ".bel")
        )
    | _ -> failwith "bad url"
  in
  let perform file =
    Printf.eprintf "I: downloading %s%s...\n%!" url file;
    let target = dir // file in
    let command =
      Printf.sprintf "curl --silent --fail \"%s%s\" > \"%s\"" url file target
    in
    let r = Sys.command command in
    if r <> 0 then (
      Sys.remove target;
      None)
    else Some file
  in
  match perform file with
  | None -> None
  | Some file -> Some (file, perform "salts.json")

let rm_rf dir =
  let files = Sys.readdir dir in
  Array.iter (fun f -> Unix.unlink (dir // f)) files;
  Unix.rmdir dir

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

let lines_of_file fname =
  let ic = open_in fname in
  let rec loop accu =
    match input_line ic with
    | line -> loop (line :: accu)
    | exception End_of_file ->
        close_in ic;
        List.rev accu
  in
  loop []

let string_of_file f = lines_of_file f |> String.concat "\n"

let load_from_file of_string filename =
  if Sys.file_exists filename then (
    Printf.eprintf "I: loading %s...\n%!" (Filename.basename filename);
    Some (lines_of_file filename |> List.rev_map of_string))
  else None

let find_bel_in_dir dir =
  match
    Sys.readdir dir |> Array.to_list
    |> List.filter (fun x -> Filename.check_suffix x ".bel")
  with
  | [ file ] -> file
  | _ ->
      Printf.ksprintf failwith "directory %s must contain a single .bel file"
        dir

let wrap_main f =
  match f () with
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
    `P "See $(i,https://www.belenios.org/).";
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

let url_t =
  let doc = "Download election files from $(docv)." in
  let the_info = Arg.info [ "url" ] ~docv:"URL" ~doc in
  Arg.(value & opt (some string) None the_info)

let key_t =
  let doc = "Read private key from file $(docv)." in
  let the_info = Arg.info [ "key" ] ~docv:"KEY" ~doc in
  Arg.(value & opt (some file) None the_info)
