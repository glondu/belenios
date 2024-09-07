(**************************************************************************)
(*                                BELENIOS                                *)
(*                                                                        *)
(*  Copyright © 2024-2024 Inria                                           *)
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

open Js_of_ocaml
open Common

let check_origin =
  let open Regexp in
  let rex = regexp "^(https?://[^/]+)(/.*)?$" in
  let prefix =
    match string_match rex (Js.to_string window##.location##.href) 0 with
    | None -> ""
    | Some m -> ( match matched_group m 1 with None -> "" | Some x -> x)
  in
  fun x -> String.starts_with ~prefix (Js.to_string x##.origin)

let targetOrigin =
  match
    String.split_on_char '#' (Js.to_string Dom_html.window##.location##.href)
  with
  | x :: _ -> Js.string x
  | [] -> Js.string "*"

class type message = object
  method ballot : Js.js_string Js.t Js.optdef Js.readonly_prop
  method ready : bool Js.t Js.optdef Js.readonly_prop
end

class type wrapped_message = object
  method belenios : message Js.t Js.optdef Js.readonly_prop
end

let postBallot (window : window Js.t) ~ballot =
  let message : message Js.t =
    object%js
      val ballot = Js.Optdef.return (Js.string ballot)
      val ready = Js.undefined
    end
  in
  let wrapped_message : wrapped_message Js.t =
    object%js
      val belenios = Js.Optdef.return message
    end
  in
  window##postMessage wrapped_message targetOrigin

let postReady (window : window Js.t) () =
  let message : message Js.t =
    object%js
      val ballot = Js.undefined
      val ready = Js.Optdef.return Js._true
    end
  in
  let wrapped_message : wrapped_message Js.t =
    object%js
      val belenios = Js.Optdef.return message
    end
  in
  window##postMessage wrapped_message targetOrigin

let getMessage x =
  let x : wrapped_message Js.t = Js.Unsafe.coerce x##.data in
  x##.belenios

let getBallot e =
  if check_origin e then
    Js.Optdef.case (getMessage e)
      (fun () -> None)
      (fun x ->
        Js.Optdef.case x##.ballot
          (fun () -> None)
          (fun x -> Some (Js.to_string x)))
  else None

let getReady e =
  if check_origin e then
    Js.Optdef.case (getMessage e)
      (fun () -> false)
      (fun x -> Js.Optdef.case x##.ready (fun () -> false) Js.to_bool)
  else false
