(**************************************************************************)
(*                                BELENIOS                                *)
(*                                                                        *)
(*  Copyright © 2012-2021 Inria                                           *)
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
open Belenios_core.Common
open Web_common

module type SENDER = sig
  val send : address:string -> code:string -> unit Lwt.t
end

module type S = sig
  val generate : address:string -> unit Lwt.t
  val check : address:string -> code:string -> bool
end

module Make (I : SENDER) () = struct

  type code =
    {
      code : string;
      expiration_time : Datetime.t;
      mutable trials_left : int;
    }

  let codes = ref SMap.empty

  let filter_codes_by_time now table =
    SMap.filter (fun _ {expiration_time; _} ->
        Datetime.compare now expiration_time <= 0
      ) table

  let generate ~address =
    let now = Datetime.now () in
    let codes_ = filter_codes_by_time now !codes in
    let* code = generate_numeric () in
    let expiration_time = Period.add now (Period.second 900) in
    codes := SMap.add address {code; expiration_time; trials_left = 10} codes_;
    I.send ~address ~code

  let check ~address ~code =
    let now = Datetime.now () in
    let codes_ = filter_codes_by_time now !codes in
    codes := codes_;
    match SMap.find_opt address codes_ with
    | None -> false
    | Some x ->
       if x.code = code then (
         codes := SMap.remove address codes_;
         true
       ) else (
         if x.trials_left > 0 then
           x.trials_left <- x.trials_left - 1
         else
           codes := SMap.remove address codes_;
         false
       )

end
