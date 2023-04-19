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

open Belenios_core.Serializable_j
open Belenios_core.Common
open Belenios_core.Events
module Archive = Belenios_core.Archive

let block_size = Archive.block_size

type index = {
  map : (hash, location) Hashtbl.t;
  mutable roots : roots;
  mutable last_event : event option;
  file : string;
  mutable lines : (Archive.data_or_event * hash) list;
  timestamp : int64;
}

module DirectMonad = struct
  type 'a t = 'a

  let return x = x
  let bind x f = f x
  let fail x = raise x
  let yield () = ()
end

module IoReader = struct
  include DirectMonad

  type file = in_channel

  let get_pos = LargeFile.pos_in
  let set_pos = LargeFile.seek_in
  let read_block ic buffer = really_input ic buffer 0 block_size
end

module Reader = Archive.MakeReader (IoReader)

module IoWriter = struct
  include DirectMonad

  type file = out_channel

  let get_pos = LargeFile.pos_out
  let write_block oc buffer = output_bytes oc buffer
end

module Writer = Archive.MakeWriter (IoWriter)

let build_index filename =
  let r = Hashtbl.create 1000 in
  let ic = open_in filename in
  let header = Reader.read_header ic in
  let rec loop last accu lines =
    match Reader.read_record ic with
    | exception End_of_file -> (r, last, accu, lines, header.timestamp)
    | record ->
        let last, accu =
          match record.typ with
          | Data -> (last, accu)
          | Event event -> (Some event, update_roots record.hash event accu)
        in
        Hashtbl.add r record.hash record.location;
        loop last accu ((record.typ, record.hash) :: lines)
  in
  Fun.protect
    ~finally:(fun () -> close_in ic)
    (fun () -> loop None empty_roots [])

let get_index ~file =
  let map, last_event, roots, lines, timestamp = build_index file in
  { map; roots; last_event; file; lines; timestamp }

let gethash ~index ~filename x =
  match Hashtbl.find_opt index x with
  | None -> None
  | Some i ->
      let ic = open_in filename in
      Fun.protect
        ~finally:(fun () -> close_in ic)
        (fun () ->
          LargeFile.seek_in ic i.location_offset;
          assert (i.location_length <= Int64.of_int Sys.max_string_length);
          Some (really_input_string ic (Int64.to_int i.location_length)))

let get_data i x = gethash ~index:i.map ~filename:i.file x

let get_event i x =
  gethash ~index:i.map ~filename:i.file x |> Option.map event_of_string

let get_roots i = i.roots

let fold_on_event_payload_hashes index typ last_event f accu =
  let rec loop e accu =
    match get_event index e with
    | None -> assert false
    | Some e ->
        if e.event_typ = typ then
          match (e.event_payload, e.event_parent) with
          | Some payload, Some parent -> loop parent (f payload accu)
          | _ -> assert false
        else accu
  in
  loop last_event accu

let fold_on_event_payloads index typ last_event f accu =
  fold_on_event_payload_hashes index typ last_event
    (fun payload accu ->
      match get_data index payload with
      | None -> assert false
      | Some x -> f x accu)
    accu

let fsck index =
  let last_event =
    match index.last_event with None -> failwith "no events" | Some x -> x
  in
  let header = { version = 1; timestamp = index.timestamp } in
  let module IoComparer = struct
    include DirectMonad

    type file = in_channel

    let get_pos = LargeFile.pos_in
    let buffer = Bytes.create block_size

    let write_block ic expected =
      try
        let () = really_input ic buffer 0 block_size in
        if expected <> buffer then
          failwith "generated archive is not identical to original one"
      with End_of_file ->
        failwith "generate archive is longer than original one"
  end in
  let module Comparer = Archive.MakeWriter (IoComparer) in
  let module IoArchiver = struct
    include DirectMonad

    let get_hash hash = gethash ~index:index.map ~filename:index.file hash
  end in
  let module Archiver = Archive.MakeArchiver (IoArchiver) (Comparer) in
  let ic = open_in_bin index.file in
  let length = LargeFile.in_channel_length ic in
  Fun.protect
    ~finally:(fun () -> close_in ic)
    (fun () ->
      let () = Archiver.write_archive ic header last_event in
      if LargeFile.pos_in ic <> length then
        failwith "generated archive is shorter than original one")

let starts_with ~(prefix : index) (index : index) =
  let rec loop x y =
    match (x, y) with
    | x :: xs, y :: ys when x = y -> loop xs ys
    | [], _ -> true
    | _ -> false
  in
  loop (List.rev prefix.lines) (List.rev index.lines)

let write_header filename ~timestamp =
  let oc =
    open_out_gen
      [ Open_wronly; Open_append; Open_creat; Open_binary ]
      0o644 filename
  in
  Fun.protect
    ~finally:(fun () -> close_out oc)
    (fun () -> Writer.write_header oc { version = 1; timestamp })

let raw_append ~filename ~timestamp xs =
  let oc =
    open_out_gen [ Open_wronly; Open_append; Open_binary ] 0o644 filename
  in
  Fun.protect
    ~finally:(fun () -> close_out oc)
    (fun () ->
      List.fold_left
        (fun accu (typ, x) -> Writer.write_record oc ~timestamp typ x :: accu)
        [] xs)

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
            let roots = update_roots event_h event roots in
            let items = (typ, event_s) :: items in
            (Some event, roots, items, (typ, event_h) :: lines))
      (index.last_event, index.roots, [], index.lines)
      ops
  in
  let items = List.rev items in
  let records =
    raw_append ~filename:index.file ~timestamp:index.timestamp items
  in
  List.iter (fun r -> Hashtbl.add index.map r.Archive.hash r.location) records;
  index.roots <- roots;
  index.last_event <- last_event;
  index.lines <- lines

let init ~file ~election ~trustees ~public_creds =
  if Sys.file_exists file then Printf.ksprintf failwith "%s already exists" file;
  let timestamp = Unix.time () |> Int64.of_float in
  let index =
    {
      map = Hashtbl.create 1000;
      roots = empty_roots;
      last_event = None;
      file;
      lines = [];
      timestamp;
    }
  in
  write_header file ~timestamp;
  let setup_election = Hash.hash_string election in
  let setup_trustees = Hash.hash_string trustees in
  let setup_credentials = Hash.hash_string public_creds in
  let setup_data = { setup_election; setup_trustees; setup_credentials } in
  let setup_data_s = string_of_setup_data setup_data in
  append index
    [
      Data election;
      Data trustees;
      Data public_creds;
      Data setup_data_s;
      Event (`Setup, Some (Hash.hash_string setup_data_s));
    ];
  index
