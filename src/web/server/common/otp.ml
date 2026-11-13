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
    ?lang:lang ->
    context:context ->
    recipient:recipient ->
    ?state:string ->
    code:string ->
    unit ->
    (string, unit) result Lwt.t
end

module type S = sig
  type payload
  type context

  val generate :
    ?lang:lang ->
    context:context ->
    recipient:recipient ->
    ?state:string ->
    payload:payload ->
    unit ->
    (string, unit) result Lwt.t

  val check : address:string -> code:string -> payload option
end

module Make (I : SENDER) () = struct
  type payload = I.payload
  type context = I.context

  type code = {
    code : string;
    mutable payload : payload;
    expiration_time : float;
    mutable trials_left : int;
    sent_time : float;
    sent : (string, unit) result Lwt.t;
  }

  let codes = ref SMap.empty
  let max_trials = 10

  (* minimum delay (in seconds) between two codes sent to the same
     address *)
  let min_delay = 60.

  let filter_codes_by_time now table =
    SMap.filter (fun _ { expiration_time; _ } -> now <= expiration_time) table

  let is_throttled now { sent_time; sent; _ } =
    now < sent_time +. min_delay
    &&
    match Lwt.state sent with
    | Return (Error ()) | Fail _ -> false
    | _ -> true

  let generate ?lang ~context ~(recipient : recipient) ?state ~payload () =
    let now = Unix.gettimeofday () in
    let codes_ = filter_codes_by_time now !codes in
    match SMap.find_opt recipient.address codes_ with
    | Some x when is_throttled now x ->
        (* a code has just been sent: keep it instead of sending a new
           one, so that the address cannot be flooded *)
        codes := codes_;
        x.payload <- payload;
        x.sent
    | previous ->
        let code = generate_numeric 6 in
        let expiration_time = now +. 900. in
        let () =
          let x = { recipient = recipient.address; code; expiration_time } in
          let msg =
            Printf.sprintf "Sending OTP %s" (!+yojson_of_otp_record x)
          in
          Ocsigen.Messages.accesslog msg
        in
        (* requesting a new code must not give more trials, otherwise the
           code could be brute-forced by requesting new codes repeatedly *)
        let trials_left =
          match previous with
          | Some { trials_left; _ } -> trials_left
          | None -> max_trials
        in
        let sent = I.send ?lang ~context ~recipient ?state ~code () in
        codes :=
          SMap.add recipient.address
            {
              code;
              payload;
              expiration_time;
              trials_left;
              sent_time = now;
              sent;
            }
            codes_;
        sent

  let check ~address ~code =
    let now = Unix.gettimeofday () in
    let codes_ = filter_codes_by_time now !codes in
    codes := codes_;
    match SMap.find_opt address codes_ with
    | None -> None
    | Some x ->
        if x.trials_left <= 0 then
          (* the entry is kept until it expires, so that the number of
             trials is not reset by requesting a new code *)
          None
        else if x.code = code then (
          codes := SMap.remove address codes_;
          Some x.payload)
        else (
          x.trials_left <- x.trials_left - 1;
          None)
end
