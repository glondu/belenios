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
open Belenios
open Common

type 'a t = { post : window Js.t -> 'a -> unit; wait : unit -> 'a Lwt.t }

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

type message

class type wrapped_message = object
  method belenios : message Js.t Js.optdef Js.readonly_prop
end

let post what cast (window : window Js.t) x =
  let message : message Js.t =
    Js.Unsafe.obj [| (what, Js.Unsafe.inject @@ cast x) |]
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

let get what cast e =
  if check_origin e then
    Js.Optdef.case (getMessage e)
      (fun () -> None)
      (fun x ->
        Js.Optdef.case (Js.Unsafe.get x what)
          (fun () -> None)
          (fun x -> Some (cast x)))
  else None

let wait get =
  let t, u = Lwt.task () in
  let id = ref None in
  let handler =
    let@ event = Dom_html.handler in
    match get event with
    | Some x ->
        Option.iter Dom_html.removeEventListener !id;
        Lwt.wakeup_later u x;
        Js._false
    | None -> Js._false
  in
  id :=
    Some
      (Dom_html.addEventListener Dom_html.window Event.message handler Js._false);
  t

let make what to_js of_js =
  let get e = get what of_js e in
  let post w x = post what to_js w x in
  let wait () = wait get in
  { post; wait }

let post t = t.post
let wait t = t.wait
let ready = make "ready" Js.bool Js.to_bool
let ballot = make "ballot" Js.string Js.to_string
