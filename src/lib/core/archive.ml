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

open Serializable_j
open Signatures
open Common

type data_or_event = Data | Event of event
type record = { typ : data_or_event; hash : hash; location : location }

let block_size = Tar.block_size
let block_sizeL = Int64.of_int block_size

let new_header ~timestamp =
  { version = 1; timestamp = `String (Int64.to_string timestamp) }

let get_timestamp header =
  match header.timestamp with
  | `String x | `Intlit x -> Int64.of_string x
  | `Int x -> Int64.of_int x
  | _ -> invalid_arg "get_timestamp"

module type IO_READER = sig
  include MONAD

  type file

  val get_pos : file -> int64 t
  val set_pos : file -> int64 -> unit t
  val read_block : file -> bytes -> unit t
end

module type ARCHIVE_READER = sig
  type 'a m
  type archive

  val read_header : archive -> archive_header m
  val read_record : archive -> record m
end

module MakeReader (M : IO_READER) = struct
  type archive = M.file

  let ( let* ) = M.bind

  let raw_read_header f buffer =
    let* () = M.read_block f buffer in
    M.return @@ Tar.header_of_bytes buffer

  let raw_read_body f buffer length =
    assert (length <= Int64.of_int Sys.max_string_length);
    let length = Int64.to_int length in
    let result = Bytes.create length in
    let rec loop offset length =
      if length > block_size then (
        let* () = M.read_block f buffer in
        Bytes.blit buffer 0 result offset block_size;
        loop (offset + block_size) (length - block_size))
      else if length > 0 then (
        let* () = M.read_block f buffer in
        Bytes.blit buffer 0 result offset length;
        M.return @@ Bytes.to_string result)
      else M.return @@ Bytes.to_string result
    in
    loop 0 length

  let read_header f =
    let buffer = Bytes.create block_size in
    let* { name; size; _ } = raw_read_header f buffer in
    let* header = raw_read_body f buffer size in
    if name = "BELENIOS" then
      let header = archive_header_of_string header in
      if header.version = 1 then M.return header
      else M.fail (Failure "unsupported archive header")
    else M.fail (Failure "ill-formed archive header found")

  let read_record f =
    let buffer = Bytes.create block_size in
    let* { name; size; _ } = raw_read_header f buffer in
    let* location_offset = M.get_pos f in
    let typ, hash =
      let i = 0 in
      let j = String.index_from name i '.' + 1 in
      let k = String.index_from name j '.' + 1 in
      ( (match String.sub name j (k - j - 1) with
        | "data" -> `Data
        | "event" -> `Event
        | _ -> assert false),
        String.sub name i (j - i - 1) |> Hash.of_hex )
    in
    let location = { location_offset; location_length = size } in
    let* typ =
      match typ with
      | `Event ->
          let* body = raw_read_body f buffer size in
          M.return @@ Event (event_of_string body)
      | `Data ->
          let new_pos =
            let open Int64 in
            let q = div size block_sizeL in
            let r = rem size block_sizeL in
            let blocks = add q (if r = 0L then 0L else 1L) in
            add location_offset (mul blocks block_sizeL)
          in
          let* () = M.set_pos f new_pos in
          M.return Data
    in
    M.return { typ; hash; location }
end

module type IO_WRITER = sig
  include MONAD

  type file

  val get_pos : file -> int64 t
  val write_block : file -> bytes -> unit t
end

module type ARCHIVE_WRITER = sig
  type 'a m
  type archive

  val write_header : archive -> archive_header -> unit m

  val write_record :
    archive -> timestamp:int64 -> data_or_event -> string -> record m
end

module MakeWriter (M : IO_WRITER) = struct
  type archive = M.file

  let ( let* ) = M.bind

  let raw_write_header f x =
    let buffer = Tar.bytes_of_header x in
    M.write_block f buffer

  let raw_write_body f buffer body =
    let rec loop offset length =
      if length > block_size then (
        Bytes.blit_string body offset buffer 0 block_size;
        let* () = M.write_block f buffer in
        loop (offset + block_size) (length - block_size))
      else if length > 0 then (
        Bytes.blit_string body offset buffer 0 length;
        for i = length to block_size - 1 do
          Bytes.set buffer i '\000'
        done;
        M.write_block f buffer)
      else M.return ()
    in
    loop 0 (String.length body)

  let write_header f header =
    let bel_header = string_of_archive_header header in
    let tar_header =
      let size = String.length bel_header |> Int64.of_int in
      let timestamp = get_timestamp header in
      Tar.{ name = "BELENIOS"; size; timestamp }
    in
    let* () = raw_write_header f tar_header in
    let buffer = Bytes.make block_size '\000' in
    raw_write_body f buffer bel_header

  let write_record f ~timestamp typ payload =
    let location_length = String.length payload |> Int64.of_int in
    let hash = Hash.hash_string payload in
    let tar_header =
      let name =
        let typ = match typ with Data -> "data" | Event _ -> "event" in
        Printf.sprintf "%s.%s.json" (Hash.to_hex hash) typ
      in
      Tar.{ name; size = location_length; timestamp }
    in
    let* () = raw_write_header f tar_header in
    let buffer = Bytes.make block_size '\000' in
    let* location_offset = M.get_pos f in
    let* () = raw_write_body f buffer payload in
    let location = { location_offset; location_length } in
    M.return { typ; hash; location }
end

module type IO_ARCHIVER = sig
  include MONAD

  val get_hash : hash -> string option t
end

module type ARCHIVER = sig
  type 'a m
  type archive

  val write_archive : archive -> archive_header -> event -> unit m
end

module MakeArchiver
    (M : IO_ARCHIVER)
    (W : ARCHIVE_WRITER with type 'a m := 'a M.t) =
struct
  let ( let* ) = M.bind

  let get_hash hash =
    let* x = M.get_hash hash in
    match x with
    | None ->
        let msg = Printf.sprintf "hash %s not found" (Hash.to_hex hash) in
        M.fail (Failure msg)
    | Some x ->
        let actual_hash = Hash.hash_string x in
        if hash = actual_hash then M.return x
        else
          let msg =
            Printf.sprintf "hash %s found instead of %s"
              (Hash.to_hex actual_hash) (Hash.to_hex hash)
          in
          M.fail (Failure msg)

  let get_payload event =
    match event.event_payload with
    | None ->
        let msg =
          Printf.sprintf "missing payload in event %s"
            (string_of_event_type event.event_typ)
        in
        M.fail (Failure msg)
    | Some hash -> get_hash hash

  let write_archive archive header last =
    let timestamp = get_timestamp header in
    let* () = W.write_header archive header in
    let rec loop last accu =
      match last.event_parent with
      | None -> M.return accu
      | Some parent ->
          let* previous = get_hash parent in
          let previous = event_of_string previous in
          loop previous (previous :: accu)
    in
    let* events = loop last [ last ] in
    let rec loop height parent = function
      | [] -> M.return ()
      | event :: events ->
          let event_s = string_of_event event in
          let event_h = Hash.hash_string event_s in
          if event.event_parent = parent && event.event_height = height then
            let* () =
              match event.event_typ with
              | `Ballot | `Result ->
                  let* payload = get_payload event in
                  let* _ = W.write_record archive ~timestamp Data payload in
                  let* _ =
                    W.write_record archive ~timestamp (Event event) event_s
                  in
                  M.return ()
              | `EndBallots | `EndShuffles ->
                  if event.event_payload = None then
                    let* _ =
                      W.write_record archive ~timestamp (Event event) event_s
                    in
                    M.return ()
                  else
                    let msg =
                      Printf.sprintf "extra payload found at height %d" height
                    in
                    M.fail (Failure msg)
              | `Setup ->
                  let* payload = get_payload event in
                  let setup_data = setup_data_of_string payload in
                  let* election = get_hash setup_data.setup_election in
                  let* trustees = get_hash setup_data.setup_trustees in
                  let* credentials = get_hash setup_data.setup_credentials in
                  let* _ = W.write_record archive ~timestamp Data election in
                  let* _ = W.write_record archive ~timestamp Data trustees in
                  let* _ = W.write_record archive ~timestamp Data credentials in
                  let* _ = W.write_record archive ~timestamp Data payload in
                  let* _ =
                    W.write_record archive ~timestamp (Event event) event_s
                  in
                  M.return ()
              | `EncryptedTally ->
                  let* payload = get_payload event in
                  let sized_et =
                    sized_encrypted_tally_of_string read_hash payload
                  in
                  let et_h = sized_et.sized_encrypted_tally in
                  let* et_s = get_hash et_h in
                  let* _ = W.write_record archive ~timestamp Data et_s in
                  let* _ = W.write_record archive ~timestamp Data payload in
                  let* _ =
                    W.write_record archive ~timestamp (Event event) event_s
                  in
                  M.return ()
              | `Shuffle | `PartialDecryption ->
                  let* payload = get_payload event in
                  let owned = owned_of_string read_hash payload in
                  let it_h = owned.owned_payload in
                  let* it_s = get_hash it_h in
                  let* _ = W.write_record archive ~timestamp Data it_s in
                  let* _ = W.write_record archive ~timestamp Data payload in
                  let* _ =
                    W.write_record archive ~timestamp (Event event) event_s
                  in
                  M.return ()
            in
            loop (height + 1) (Some event_h) events
          else
            let msg = Printf.sprintf "inconsistency at height %d" height in
            M.fail (Failure msg)
    in
    loop 0 None events
end
