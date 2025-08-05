(**************************************************************************)
(*                                BELENIOS                                *)
(*                                                                        *)
(*  Copyright © 2025-2025 Inria                                           *)
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

let get f url x =
  let url = Uri.of_string (Printf.sprintf "%s/%s" url x) in
  let* response, body = Cohttp_lwt_unix.Client.get url in
  match Cohttp.Code.code_of_status response.status with
  | 200 ->
      let* body = Cohttp_lwt.Body.to_string body in
      Lwt.return_some @@ f body
  | _ -> Lwt.return_none

let get_exn f url x =
  let* y = get f url x in
  match y with
  | Some x -> Lwt.return x
  | None -> Printf.ksprintf failwith "%s/%s not found" url x

let get_last_event url = get_exn last_event_of_string url "last-event"

let get_archive_header url =
  get_exn archive_header_of_string url "archive-header"

let get_hash_base base =
  match base with
  | None -> Lwt.return (fun _ -> Lwt.return_none)
  | Some (`Archive file) ->
      let* index = Tool_events.get_index ~file in
      Lwt.return (fun h -> Tool_events.get_data index h)
  | Some (`Dir dir) ->
      let get_hash hash =
        let hash_s = Hash.to_hex hash in
        let get suffix =
          Lwt.catch
            (fun () ->
              let x = Lwt_io.chars_of_file (dir // (hash_s ^ suffix)) in
              let* x = Lwt_stream.to_string x in
              Lwt.return_some x)
            (fun _ -> Lwt.return_none)
        in
        let* x = get ".event.json" in
        match x with None -> get ".data.json" | _ -> Lwt.return x
      in
      Lwt.return get_hash

let mkarchive base url fd =
  let* header = get_archive_header url in
  let* last = get_last_event url in
  let* get_hash_base = get_hash_base base in
  let module IoArchiver = struct
    include Tool_events.LwtMonad

    let get_hash hash =
      let* x = get_hash_base hash in
      match x with
      | Some x -> Lwt.return_some x
      | None ->
          let hash_s = Hash.to_hex hash in
          get Fun.id url (Printf.sprintf "objects/%s" hash_s)
  end in
  let module Archiver = Archive.MakeArchiver (IoArchiver) (Tool_events.Writer)
  in
  let oc : Tool_events.file = { pos = 0L; fd } in
  let* last_event =
    let* x = IoArchiver.get_hash last.last_hash in
    match x with
    | Some x -> Lwt.return @@ event_of_string x
    | None -> Printf.ksprintf failwith "last event not found at %s" url
  in
  Archiver.write_archive oc header last_event

let main base_archive base_dir url uuid =
  let@ () = wrap_main in
  let base =
    match (base_archive, base_dir) with
    | None, None -> None
    | Some a, None -> Some (`Archive a)
    | None, Some d -> Some (`Dir d)
    | Some _, Some _ ->
        failwith "--base-archive and --base-dir are mutually exclusive"
  in
  match (url, uuid) with
  | Some url, Some uuid ->
      let url = Printf.sprintf "%sapi/elections/%s" url uuid in
      mkarchive base url Lwt_unix.stdout
  | None, _ -> failwith "missing --url"
  | _, None -> failwith "missing --uuid"

open Cmdliner

let base_archive_t =
  let doc = "Path to base archive." in
  let the_info = Arg.info [ "base-archive" ] ~docv:"BASE-ARCHIVE" ~doc in
  Arg.(value & opt (some file) None the_info)

let base_dir_t =
  let doc = "Path to extracted base archive." in
  let the_info = Arg.info [ "base-dir" ] ~docv:"BASE-DIR" ~doc in
  Arg.(value & opt (some dir) None the_info)

let cmd =
  let doc = "pull election archive" in
  let man =
    [
      `S "DESCRIPTION";
      `P
        "This command downloads each new (w.r.t. a base archive) individual \
         object of an election and outputs (on standard output) a canonical \
         archive containing all the objects.";
    ]
    @ common_man
  in
  Cmd.v
    (Cmd.info "pull" ~doc ~man)
    Term.(ret (const main $ base_archive_t $ base_dir_t $ url_t $ uuid_t))
