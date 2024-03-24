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
let block_sizeL = Int64.of_int block_size

type index = {
  timeout : Lwt_timeout.t;
  map : (hash, location) Hashtbl.t;
  mutable roots : roots;
  timestamp : int64;
}

module IoReader = struct
  include Lwt

  let yield = Lwt.pause

  open Lwt_io

  type file = input_channel

  let get_pos ic = Lwt.return @@ position ic
  let set_pos = set_position
  let read_block ic buffer = read_into_exactly ic buffer 0 block_size
end

module Reader = Archive.MakeReader (IoReader)

module IoWriter = struct
  include Lwt

  let yield = Lwt.pause

  open Lwt_io

  (* `Lwt_io`'s position does not work with files opened in append
     mode, so we implement it here *)

  type file = { channel : output_channel; mutable position : int64 }

  let get_pos oc = Lwt.return oc.position

  let write_block oc buffer =
    let* () = write_from_exactly oc.channel buffer 0 block_size in
    oc.position <- Int64.add oc.position block_sizeL;
    Lwt.return_unit
end

module Writer = Archive.MakeWriter (IoWriter)

let indexes = Hashtbl.create 100

let write_header ~filename ~header =
  let open Lwt_unix in
  let* fd = openfile filename [ O_WRONLY; O_APPEND; O_CREAT ] 0o644 in
  Lwt.finalize
    (fun () ->
      let* () = LargeFile.ftruncate fd 0L in
      let oc =
        { IoWriter.channel = Lwt_io.of_fd ~mode:Output fd; position = 0L }
      in
      let* () = Writer.write_header oc header in
      Lwt_io.flush oc.channel)
    (fun () -> close fd)

let build_roots ~size ~pos filename =
  let r = Hashtbl.create size in
  let@ () =
   fun cont ->
    if pos > 0L then cont ()
    else
      let header = Archive.new_header () in
      let* () = write_header ~filename ~header in
      Lwt.return (r, Events.empty_roots, Archive.get_timestamp header)
  in
  let* fd = Lwt_unix.openfile filename [ Unix.O_RDONLY ] 0o644 in
  let open Lwt_io in
  let ic = of_fd ~mode:Input fd in
  let* header = Reader.read_header ic in
  let rec loop accu =
    let location_offset = position ic in
    if location_offset < pos then (
      let* record = Reader.read_record ic in
      let accu =
        match record.typ with
        | Data -> accu
        | Event event -> Events.update_roots record.hash event accu
      in
      Hashtbl.add r record.hash record.location;
      loop accu)
    else Lwt.return (r, accu, Archive.get_timestamp header)
  in
  Lwt.finalize (fun () -> loop Events.empty_roots) (fun () -> close ic)

exception Creation_not_requested

let do_get_index ~creat ~uuid =
  let* last = Spool.get ~uuid Spool.last_event in
  let size, pos =
    match last with
    | None when creat -> (100, 0L)
    | None -> raise Creation_not_requested
    | Some x -> (x.last_height + 100, x.last_pos)
  in
  let* filename = Filesystem.(get_path (Election (uuid, Public_archive))) in
  let* map, roots, timestamp = build_roots ~size ~pos filename in
  let remove () = Hashtbl.remove indexes uuid in
  let timeout = Lwt_timeout.create 3600 remove in
  let r = { timeout; map; roots; timestamp } in
  Hashtbl.add indexes uuid r;
  Lwt.return r

let get_index ?(lock = true) ~creat uuid =
  let* r =
    match Hashtbl.find_opt indexes uuid with
    | Some r -> Lwt.return r
    | None ->
        if lock then
          let@ () = Web_election_mutex.with_lock uuid in
          match Hashtbl.find_opt indexes uuid with
          | Some r -> Lwt.return r
          | None -> do_get_index ~creat ~uuid
        else do_get_index ~creat ~uuid
  in
  Lwt_timeout.start r.timeout;
  Lwt.return r

let raw_append ~filename ~timestamp offset xs =
  let open Lwt_unix in
  let* fd = openfile filename [ O_WRONLY; O_APPEND ] 0o644 in
  Lwt.finalize
    (fun () ->
      let* () =
        let* pos = LargeFile.lseek fd 0L SEEK_END in
        if pos = offset then Lwt.return_unit else LargeFile.ftruncate fd offset
      in
      let oc =
        { IoWriter.channel = Lwt_io.of_fd ~mode:Output fd; position = offset }
      in
      let* records =
        Lwt_list.fold_left_s
          (fun accu (typ, x) ->
            let* record = Writer.write_record oc ~timestamp typ x in
            Lwt.return @@ (record :: accu))
          [] xs
      in
      let* () = Lwt_io.flush oc.channel in
      let* () = fsync fd in
      Lwt.return (oc.position, records))
    (fun () -> close fd)

let gethash ~index ~filename x =
  match Hashtbl.find_opt index x with
  | None -> Lwt.return_none
  | Some i ->
      let open Lwt_unix in
      let* fd = openfile filename [ O_RDONLY ] 0o644 in
      Lwt.finalize
        (fun () ->
          let* _ = LargeFile.lseek fd i.location_offset SEEK_SET in
          assert (i.location_length <= Int64.of_int Sys.max_string_length);
          let length = Int64.to_int i.location_length in
          let buffer = Bytes.create length in
          let ic = Lwt_io.of_fd ~mode:Input fd in
          let* () = Lwt_io.read_into_exactly ic buffer 0 length in
          Lwt.return_some @@ Bytes.to_string buffer)
        (fun () -> close fd)

let get_data ~uuid x =
  Lwt.try_bind
    (fun () -> get_index ~creat:false uuid)
    (fun r ->
      let* filename = Filesystem.(get_path (Election (uuid, Public_archive))) in
      gethash ~index:r.map ~filename x)
    (function Creation_not_requested -> Lwt.return_none | e -> Lwt.reraise e)

let get_event ~uuid x =
  let* x = get_data ~uuid x in
  Lwt.return @@ Option.map event_of_string x

let get_roots ~uuid =
  Lwt.try_bind
    (fun () -> get_index ~creat:false uuid)
    (fun r -> Lwt.return r.roots)
    (function
      | Creation_not_requested -> Lwt.return Events.empty_roots
      | e -> Lwt.reraise e)

type append_operation = Data of string | Event of event_type * hash option

exception RaceCondition

let append ?(lock = true) ~uuid ?last ops =
  let@ () =
   fun cont -> if lock then Web_election_mutex.with_lock uuid cont else cont ()
  in
  let@ last cont =
    let* x = Spool.get ~uuid Spool.last_event in
    match last with
    | None -> cont x
    | Some last -> if x = Some last then cont x else Lwt.fail RaceCondition
  in
  let* index = get_index ~lock:false ~creat:true uuid in
  let event_parent, event_height, pos =
    match last with
    | None -> (None, -1, 1024L (* header size *))
    | Some x -> (Some x.last_hash, x.last_height, x.last_pos)
  in
  let last_hash, last_height, roots, items =
    List.fold_left
      (fun (event_parent, event_height, roots, accu) x ->
        match x with
        | Event (event_typ, event_payload) ->
            let event_height = event_height + 1 in
            let event =
              { event_parent; event_height; event_typ; event_payload }
            in
            let event_s = string_of_event event in
            let event_h = Hash.hash_string event_s in
            let accu = (Archive.Event event, event_s) :: accu in
            ( Some event_h,
              event_height,
              Events.update_roots event_h event roots,
              accu )
        | Data payload ->
            let accu = (Archive.Data, payload) :: accu in
            (event_parent, event_height, roots, accu))
      (event_parent, event_height, index.roots, [])
      ops
  in
  let last_hash = match last_hash with None -> assert false | Some x -> x in
  let items = List.rev items in
  let* last_pos, records =
    let* filename = Filesystem.(get_path (Election (uuid, Public_archive))) in
    raw_append ~filename ~timestamp:index.timestamp pos items
  in
  let* () =
    Spool.set ~uuid Spool.last_event { last_hash; last_height; last_pos }
  in
  List.iter (fun r -> Hashtbl.add index.map r.Archive.hash r.location) records;
  index.roots <- roots;
  Lwt.return_unit
