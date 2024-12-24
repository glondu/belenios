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

let block_size = Archive.block_size

type index = {
  map : (hash, location) Hashtbl.t;
  mutable roots : roots;
  mutable last_event : event option;
  file : string;
  mutable lines : (Archive.data_or_event * hash) list;
  timestamp : int64;
  header : archive_header;
}

module LwtMonad = struct
  type 'a t = 'a Lwt.t

  let return = Lwt.return
  let bind = Lwt.bind
  let fail = Lwt.fail
  let yield = Lwt.pause
end

type file = { mutable pos : int64; fd : Lwt_unix.file_descr }

module IoReader = struct
  include LwtMonad

  type nonrec file = file

  let get_pos f = Lwt.return f.pos

  let set_pos f x =
    let* _ = Lwt_unix.LargeFile.lseek f.fd x SEEK_SET in
    f.pos <- x;
    Lwt.return_unit

  let read_block f buffer =
    let rec loop i =
      if i < block_size then (
        let* n = Lwt_unix.read f.fd buffer i (block_size - i) in
        f.pos <- Int64.add f.pos (Int64.of_int n);
        if n = 0 then raise End_of_file else loop (i + n))
      else Lwt.return_unit
    in
    loop 0
end

module Reader = Archive.MakeReader (IoReader)

module IoWriter = struct
  include LwtMonad

  type nonrec file = file

  let get_pos f = Lwt.return f.pos

  let write_block f buffer =
    let rec loop i =
      if i < block_size then (
        let* n = Lwt_unix.write f.fd buffer i (block_size - i) in
        f.pos <- Int64.add f.pos (Int64.of_int n);
        loop (i + n))
      else Lwt.return_unit
    in
    loop 0
end

module Writer = Archive.MakeWriter (IoWriter)

let build_index filename =
  let r = Hashtbl.create 1000 in
  let* fd = Lwt_unix.openfile filename [ O_RDONLY ] 0o600 in
  let ic = { pos = 0L; fd } in
  let* header = Reader.read_header ic in
  let rec loop last accu lines =
    Lwt.try_bind
      (fun () -> Reader.read_record ic)
      (fun record ->
        let last, accu =
          match record.typ with
          | Data -> (last, accu)
          | Event event ->
              (Some event, Events.update_roots record.hash event accu)
        in
        Hashtbl.add r record.hash record.location;
        loop last accu ((record.typ, record.hash) :: lines))
      (function
        | End_of_file -> Lwt.return (r, last, accu, lines, header)
        | e -> Lwt.reraise e)
  in
  Lwt.finalize
    (fun () -> loop None Events.empty_roots [])
    (fun () -> Lwt_unix.close fd)

let get_index ~file =
  let* map, last_event, roots, lines, header = build_index file in
  let timestamp = Archive.get_timestamp header in
  { map; roots; last_event; file; lines; timestamp; header } |> Lwt.return

let really_input_string ic n =
  let buffer = Bytes.create n in
  let rec loop i =
    if i < n then
      let* j = Lwt_unix.read ic buffer i (n - i) in
      loop (i + j)
    else Lwt.return @@ Bytes.to_string buffer
  in
  loop 0

let gethash ~index ~filename x =
  match Hashtbl.find_opt index x with
  | None -> Lwt.return_none
  | Some i ->
      let* ic = Lwt_unix.openfile filename [ O_RDONLY ] 0o600 in
      Lwt.finalize
        (fun () ->
          let* _ = Lwt_unix.LargeFile.lseek ic i.location_offset SEEK_SET in
          assert (i.location_length <= Int64.of_int Sys.max_string_length);
          let* contents =
            really_input_string ic (Int64.to_int i.location_length)
          in
          Lwt.return_some contents)
        (fun () -> Lwt_unix.close ic)

let get_data i x = gethash ~index:i.map ~filename:i.file x

let get_event i x =
  let* x = gethash ~index:i.map ~filename:i.file x in
  Lwt.return @@ Option.map event_of_string x

let get_last_event i =
  i.last_event |> Option.map (string_of_event >> Hash.hash_string)

let get_roots i = i.roots

let fold_on_event_payload_hashes index typ last_event f accu =
  let rec loop e accu =
    let* x = get_event index e in
    match x with
    | None -> assert false
    | Some e ->
        if e.event_typ = typ then
          match (e.event_payload, e.event_parent) with
          | Some payload, Some parent ->
              let* x = f payload accu in
              loop parent x
          | _ -> assert false
        else Lwt.return accu
  in
  loop last_event accu

let fold_on_event_payloads index typ last_event f accu =
  fold_on_event_payload_hashes index typ last_event
    (fun payload accu ->
      let* x = get_data index payload in
      match x with None -> assert false | Some x -> f x accu)
    accu

let fsck index =
  let last_event =
    match index.last_event with None -> failwith "no events" | Some x -> x
  in
  let module IoComparer = struct
    include LwtMonad

    type nonrec file = file

    let get_pos f = Lwt.return f.pos
    let buffer = Bytes.create block_size

    let write_block f expected =
      try
        let rec loop i =
          if i < block_size then (
            let* n = Lwt_unix.read f.fd buffer i (block_size - i) in
            f.pos <- Int64.add f.pos (Int64.of_int n);
            loop (i + n))
          else Lwt.return_unit
        in
        let* () = loop 0 in
        if expected <> buffer then
          failwith "generated archive is not identical to original one"
        else Lwt.return_unit
      with End_of_file ->
        failwith "generated archive is longer than original one"
  end in
  let module Comparer = Archive.MakeWriter (IoComparer) in
  let module IoArchiver = struct
    include LwtMonad

    let get_hash hash = gethash ~index:index.map ~filename:index.file hash
  end in
  let module Archiver = Archive.MakeArchiver (IoArchiver) (Comparer) in
  let* fd = Lwt_unix.openfile index.file [ O_RDONLY ] 0o600 in
  let* length = Lwt_unix.LargeFile.lseek fd 0L SEEK_END in
  let* _ = Lwt_unix.LargeFile.lseek fd 0L SEEK_SET in
  let ic = { pos = 0L; fd } in
  Lwt.finalize
    (fun () ->
      let* () = Archiver.write_archive ic index.header last_event in
      if ic.pos <> length then
        failwith "generated archive is shorter than original one"
      else Lwt.return_unit)
    (fun () -> Lwt_unix.close fd)

let starts_with ~(prefix : index) (index : index) =
  let rec loop x y =
    match (x, y) with
    | x :: xs, y :: ys when x = y -> loop xs ys
    | [], _ -> true
    | _ -> false
  in
  loop (List.rev prefix.lines) (List.rev index.lines)

let write_header filename header =
  let* fd = Lwt_unix.openfile filename [ O_WRONLY; O_APPEND; O_CREAT ] 0o644 in
  let* pos = Lwt_unix.LargeFile.lseek fd 0L SEEK_CUR in
  let oc = { pos; fd } in
  Lwt.finalize
    (fun () -> Writer.write_header oc header)
    (fun () -> Lwt_unix.close fd)

let raw_append ~filename ~timestamp xs =
  let* fd = Lwt_unix.openfile filename [ O_WRONLY; O_APPEND ] 0o644 in
  let* pos = Lwt_unix.LargeFile.lseek fd 0L SEEK_CUR in
  let oc = { pos; fd } in
  Lwt.finalize
    (fun () ->
      Lwt_list.fold_left_s
        (fun accu (typ, x) ->
          let* y = Writer.write_record oc ~timestamp typ x in
          Lwt.return @@ (y :: accu))
        [] xs)
    (fun () -> Lwt_unix.close fd)

type append_operation = Data of string | Event of event_type * hash option

let append index ops =
  let last_event, roots, items, lines =
    List.fold_left
      (fun (last_event, roots, items, lines) x ->
        match x with
        | Data x ->
            let typ = Archive.Data in
            let items = (typ, x) :: items in
            let lines = (typ, Hash.hash_string x) :: lines in
            (last_event, roots, items, lines)
        | Event (event_typ, event_payload) ->
            let event_parent, event_height =
              match last_event with
              | None -> (None, 0)
              | Some x ->
                  ( Some (Hash.hash_string (string_of_event x)),
                    x.event_height + 1 )
            in
            let event =
              { event_parent; event_height; event_typ; event_payload }
            in
            let typ = Archive.Event event in
            let event_s = string_of_event event in
            let event_h = Hash.hash_string event_s in
            let roots = Events.update_roots event_h event roots in
            let items = (typ, event_s) :: items in
            (Some event, roots, items, (typ, event_h) :: lines))
      (index.last_event, index.roots, [], index.lines)
      ops
  in
  let items = List.rev items in
  let* records =
    raw_append ~filename:index.file ~timestamp:index.timestamp items
  in
  List.iter (fun r -> Hashtbl.add index.map r.Archive.hash r.location) records;
  index.roots <- roots;
  index.last_event <- last_event;
  index.lines <- lines;
  Lwt.return_unit

let init ~file ~election ~trustees ~public_creds =
  let* b = Lwt_unix.file_exists file in
  if b then Printf.ksprintf failwith "%s already exists" file;
  let timestamp = Unix.time () |> Int64.of_float in
  let header = Archive.new_header ~timestamp in
  let index =
    {
      map = Hashtbl.create 1000;
      roots = Events.empty_roots;
      last_event = None;
      file;
      lines = [];
      timestamp = Archive.get_timestamp header;
      header;
    }
  in
  let* () = write_header file header in
  let setup_election = Hash.hash_string election in
  let setup_trustees = Hash.hash_string trustees in
  let setup_credentials = Hash.hash_string public_creds in
  let setup_data = { setup_election; setup_trustees; setup_credentials } in
  let setup_data_s = string_of_setup_data setup_data in
  let* () =
    append index
      [
        Data election;
        Data trustees;
        Data public_creds;
        Data setup_data_s;
        Event (`Setup, Some (Hash.hash_string setup_data_s));
      ]
  in
  Lwt.return index
