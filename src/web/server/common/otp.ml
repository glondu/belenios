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

open Belenios
open Belenios_server_core

module type SENDER = sig
  type payload
  type context

  val send :
    context:context ->
    recipient:Belenios_web_api.recipient ->
    code:string ->
    (string, unit) result Lwt.t
end

module type S = sig
  type payload
  type context

  val generate :
    context:context ->
    recipient:Belenios_web_api.recipient ->
    payload:payload ->
    (string, unit) result Lwt.t

  val check : address:string -> code:string -> payload option
end

module Make (I : SENDER) () = struct
  type payload = I.payload
  type context = I.context

  type code = {
    code : string;
    payload : payload;
    expiration_time : float;
    mutable trials_left : int;
  }

  let codes = ref SMap.empty

  let filter_codes_by_time now table =
    SMap.filter (fun _ { expiration_time; _ } -> now <= expiration_time) table

  let generate ~context ~(recipient : Belenios_web_api.recipient) ~payload =
    let now = Unix.gettimeofday () in
    let codes_ = filter_codes_by_time now !codes in
    let code = generate_numeric () in
    let expiration_time = now +. 900. in
    let () =
      let x = { recipient = recipient.address; code; expiration_time } in
      let msg = Printf.sprintf "Sending OTP %s" (string_of_otp_record x) in
      Ocsigen_messages.accesslog msg
    in
    codes :=
      SMap.add recipient.address
        { code; payload; expiration_time; trials_left = 10 }
        codes_;
    I.send ~context ~recipient ~code

  let check ~address ~code =
    let now = Unix.gettimeofday () in
    let codes_ = filter_codes_by_time now !codes in
    codes := codes_;
    match SMap.find_opt address codes_ with
    | None -> None
    | Some x ->
        if x.code = code then (
          codes := SMap.remove address codes_;
          Some x.payload)
        else (
          if x.trials_left > 0 then x.trials_left <- x.trials_left - 1
          else codes := SMap.remove address codes_;
          None)
end
