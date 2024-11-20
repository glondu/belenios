(**************************************************************************)
(*                                BELENIOS                                *)
(*                                                                        *)
(*  Copyright © 2022-2023 Inria, CNRS                                     *)
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
open Belenios_api.Serializable_j
open Belenios
open Belenios_js.Common
open Belenios_js.Session
open Common
open Lwt.Syntax

type (_, _) endpoint =
  | Plain : (Api.admin, 'a, 'b) Api.t -> ('a, unit) endpoint
  | WithUuid : (uuid -> (Api.admin, 'a, 'b) Api.t) -> ('a, uuid) endpoint

type ('a, 'b) u = {
  endpoint : ('a, 'b) endpoint;
  readonly : bool;
  mutable content : ('a * 'b) option;
  mutable ifmatch : string;
  mutable dirty : bool;
}

type 'a t = Cache : ('a, 'b) u -> 'a t

let modified (Cache x) =
  match x.content with None -> false | Some _ -> x.dirty

let invalidate (Cache x) =
  x.content <- None;
  x.dirty <- true;
  x.ifmatch <- "";
  Dom_html.window##.onbeforeunload := Dom_html.no_handler

let do_get (type a b) (x : (a, b) u) :
    ((a * string, xhr_result) result * b) Lwt.t =
  let get x = Api.get x !user in
  match x.endpoint with
  | Plain x ->
      let* x = get x in
      Lwt.return (x, ())
  | WithUuid f ->
      let uuid = get_current_uuid () in
      let* x = get (f uuid) in
      Lwt.return (x, uuid)

let get (Cache x) =
  match x.content with
  | None -> (
      let* content, uuid = do_get x in
      match content with
      | Error e -> (
          match e with
          | BadResult ->
              Lwt.return
              @@ Error (Printf.sprintf "Failed to get data from server")
          | BadStatus (code, _) ->
              Lwt.return
              @@ Error
                   (Printf.sprintf "Failed to get data from server, code %d"
                      code)
          | RequestStatus e ->
              Lwt.return
              @@ Error
                   (Printf.sprintf "Got structured error from server, code %d"
                      e.code))
      | Ok (content, ifmatch) ->
          x.dirty <- false;
          x.content <- Some (content, uuid);
          x.ifmatch <- ifmatch;
          Lwt.return @@ Ok content)
  | Some content -> Lwt.return @@ Ok (fst content)

let rec get_until_success x =
  let* result = get x in
  match result with
  | Error msg ->
      alert msg;
      get_until_success x
  | Ok xx -> Lwt.return xx

let sync_one (Cache x) =
  if x.readonly then Lwt.return @@ Ok ()
  else
    match x.content with
    | None -> assert false
    | Some (content, uuid) -> (
        let ifmatch = x.ifmatch in
        let put x =
          let* y = Api.put ~ifmatch x !user content in
          Lwt.return (y, fun () -> sha256_b64 @@ x.to_string content)
        in
        let* y, ifmatch' =
          match x.endpoint with Plain x -> put x | WithUuid f -> put (f uuid)
        in
        match y.code with
        | 200 ->
            x.dirty <- false;
            x.ifmatch <- ifmatch' ();
            Lwt.return @@ Ok ()
        | code ->
            Lwt.return
            @@ Error
                 (Printf.sprintf "Failed to sync data to server, code %d" code))

(***********************************)

let config =
  Cache
    {
      endpoint = Plain Api.configuration;
      readonly = true;
      content = None;
      dirty = true;
      ifmatch = "";
    }

let draft =
  Cache
    {
      endpoint = WithUuid Api.draft;
      readonly = false;
      content = None;
      dirty = true;
      ifmatch = "";
    }

let voters =
  Cache
    {
      endpoint = WithUuid Api.draft_voters;
      readonly = false;
      content = None;
      dirty = true;
      ifmatch = "";
    }

let status =
  Cache
    {
      endpoint = WithUuid Api.draft_status;
      readonly = true;
      content = None;
      dirty = true;
      ifmatch = "";
    }

let account =
  Cache
    {
      endpoint = Plain Api.account;
      readonly = false;
      content = None;
      dirty = true;
      ifmatch = "";
    }

let e_dates =
  Cache
    {
      endpoint = WithUuid Api.election_auto_dates;
      readonly = false;
      content = None;
      dirty = true;
      ifmatch = "";
    }

let e_voters =
  Cache
    {
      endpoint = WithUuid Api.election_voters;
      readonly = true;
      content = None;
      dirty = true;
      ifmatch = "";
    }

let e_records =
  Cache
    {
      endpoint = WithUuid Api.election_records;
      readonly = true;
      content = None;
      dirty = true;
      ifmatch = "";
    }

let e_status =
  Cache
    {
      endpoint = WithUuid Api.election_status;
      readonly = true;
      content = None;
      dirty = true;
      ifmatch = "";
    }

let e_elec =
  Cache
    {
      endpoint = WithUuid Api.election;
      readonly = true;
      content = None;
      dirty = true;
      ifmatch = "";
    }

(***********************************)

type aa = AA : 'a t -> aa

let list_items =
  [
    AA draft;
    AA voters;
    AA status;
    AA account;
    AA e_voters;
    AA e_status;
    AA e_elec;
    AA e_records;
    AA e_dates;
  ]

let sync () =
  let* list_items =
    Lwt_list.map_s
      (fun (AA x) ->
        let (Cache y) = x in
        if y.dirty && y.content <> None then sync_one x else Lwt.return @@ Ok ())
      list_items
  in
  Lwt_list.fold_left_s
    (fun accu y ->
      match y with Error e -> Lwt.return @@ Error e | Ok _ -> Lwt.return accu)
    (Ok ()) list_items

let invalidate_all () = List.iter (fun (AA x) -> invalidate x) list_items

let get_unit_or_uuid (type a b) (x : (a, b) u) : b =
  match x.endpoint with
  | Plain _ -> ()
  | WithUuid _ -> (
      match x.content with
      | None -> get_current_uuid ()
      | Some (_, uuid) -> uuid)

let set (Cache x) y =
  assert (not x.readonly);
  x.dirty <- true;
  x.content <- Some (y, get_unit_or_uuid x)

let rec sync_until_success () =
  let* result = sync () in
  match result with
  | Error msg ->
      alert msg;
      sync_until_success ()
  | Ok _ -> Lwt.return_unit

let get_prefix () =
  let* x = get config in
  match x with
  | Ok c -> Lwt.return c.uris.home
  | Error _ -> Lwt.return "https://fake_link_please_edit/"
