(**************************************************************************)
(*                                BELENIOS                                *)
(*                                                                        *)
(*  Copyright © 2012-2025 Inria                                           *)
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
open Belenios_server_core

let send_bulk_email = function
  | `Password x -> Send_message.send @@ `Voter_password x
  | `Credential x -> Send_message.send @@ `Voter_credential x

module Bulk_processor = struct
  type t = {
    mutable locked : bool;
    mutable queue : bulk_emails option;
    submitters : unit Lwt.u Queue.t;
    processors : unit Lwt.u Queue.t;
  }

  let create () =
    {
      locked = false;
      queue = None;
      submitters = Queue.create ();
      processors = Queue.create ();
    }

  let lock ~is_submitter m =
    if m.locked then (
      let q = if is_submitter then m.submitters else m.processors in
      let t, u = Lwt.wait () in
      Queue.push u q;
      t)
    else (
      m.locked <- true;
      Lwt.return_unit)

  let unlock m =
    if m.locked then
      match Queue.take_opt m.submitters with
      | None -> (
          match Queue.take_opt m.processors with
          | None -> m.locked <- false
          | Some u -> Lwt.wakeup_later u ())
      | Some u -> Lwt.wakeup_later u ()

  let with_lock ~is_submitter m f =
    let* () = lock ~is_submitter m in
    Lwt.finalize f (fun () ->
        unlock m;
        Lwt.return_unit)
end

module Ocsipersist_bulk = struct
  module F = Ocsipersist.Functorial

  module T =
    F.Table
      (struct
        let name = "belenios_bulk_emails"
      end)
      (F.Column.String)
      (F.Column.String)

  module type SerializableInput = sig
    type t

    val name : string
    val default : t
    val of_string : string -> t
    val to_string : t -> string
  end

  module type SerializableOutput = sig
    type t

    val get : unit -> t Lwt.t
    val set : t -> unit Lwt.t
  end

  module MakeSerializable (I : SerializableInput) :
    SerializableOutput with type t := I.t = struct
    let default = I.to_string I.default
    let var = T.Variable.make ~name:I.name ~default

    let get () =
      let* x = T.Variable.get var in
      Lwt.return (I.of_string x)

    let set x = T.Variable.set var (I.to_string x)
  end

  module PrimaryQueueInput = struct
    type t = bulk_emails

    let name = "primary_queue"
    let default = [||]
    let of_string = bulk_emails_of_string
    let to_string x = string_of_bulk_emails x
  end

  module SecondaryQueueInput = struct
    type t = bulk_emails

    let name = "secondary_queue"
    let default = [||]
    let of_string = bulk_emails_of_string
    let to_string x = string_of_bulk_emails x
  end

  module ProcessedInput = struct
    type t = bulk_processed

    let name = "processed"
    let default = { mode = `Primary; processed = 0 }
    let of_string = bulk_processed_of_string
    let to_string x = string_of_bulk_processed x
  end

  module PrimaryQueue = MakeSerializable (PrimaryQueueInput)
  module SecondaryQueue = MakeSerializable (SecondaryQueueInput)
  module Processed = MakeSerializable (ProcessedInput)

  let m = Bulk_processor.create ()

  let get_queue () =
    let* p = Processed.get () in
    match m.queue with
    | Some x -> Lwt.return (p, x)
    | None ->
        let* x =
          match p.mode with
          | `Primary -> PrimaryQueue.get ()
          | `Secondary -> SecondaryQueue.get ()
        in
        m.queue <- Some x;
        Lwt.return (p, x)

  let submit jobs =
    let jobs = Array.of_list jobs in
    let@ () = Bulk_processor.with_lock ~is_submitter:true m in
    let* p, current = get_queue () in
    let newset, newmode, oldset =
      match p.mode with
      | `Primary -> (SecondaryQueue.set, `Secondary, PrimaryQueue.set)
      | `Secondary -> (PrimaryQueue.set, `Primary, SecondaryQueue.set)
    in
    let current =
      Array.sub current p.processed (Array.length current - p.processed)
    in
    let newqueue = Array.append current jobs in
    let* () = newset newqueue in
    let* () = Processed.set { mode = newmode; processed = 0 } in
    m.queue <- Some newqueue;
    let* () = oldset [||] in
    Lwt.return_unit

  let process_one () =
    let@ () = Bulk_processor.with_lock ~is_submitter:false m in
    let* p, current = get_queue () in
    let i = p.processed in
    if i < Array.length current then
      let* () = send_bulk_email current.(i) in
      let* () = Processed.set { p with processed = i + 1 } in
      Lwt.return_true
    else Lwt.return_false

  let rec process () =
    let* continue = process_one () in
    if continue then process () else submit []
end

let process_bulk_emails = Ocsipersist_bulk.process

let submit_bulk_emails jobs =
  let* () = Ocsipersist_bulk.submit jobs in
  Lwt.async process_bulk_emails;
  Lwt.return_unit
