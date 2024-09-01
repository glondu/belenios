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

type _ endpoint =
  | Plain : (Api.admin, 'a, 'b) Api.t -> 'a endpoint
  | WithUuid : (uuid -> (Api.admin, 'a, 'b) Api.t) -> 'a endpoint

type 'a t = {
  endpoint : 'a endpoint;
  readonly : bool;
  mutable content : 'a option;
  mutable ifmatch : string;
  mutable dirty : bool;
}

let modified x = match x.content with None -> false | Some _ -> x.dirty

let invalidate x =
  x.content <- None;
  x.dirty <- true;
  x.ifmatch <- "";
  Dom_html.window##.onbeforeunload := Dom_html.no_handler

let get x =
  match x.content with
  | None -> (
      let get x = Api.get x !user in
      let* content =
        match x.endpoint with
        | Plain x -> get x
        | WithUuid f -> get (f (get_current_uuid ()))
      in
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
          x.content <- Some content;
          x.ifmatch <- ifmatch;
          Lwt.return @@ Ok content)
  | Some content -> Lwt.return @@ Ok content

let rec get_until_success x =
  let* result = get x in
  match result with
  | Error msg ->
      alert msg;
      get_until_success x
  | Ok xx -> Lwt.return xx

let sync_one x =
  if x.readonly then Lwt.return @@ Ok ()
  else
    match x.content with
    | None -> assert false
    | Some content -> (
        let ifmatch = x.ifmatch in
        let put x =
          let* y = Api.put ~ifmatch x !user content in
          Lwt.return (y, fun () -> sha256_b64 @@ x.to_string content)
        in
        let* y, ifmatch' =
          match x.endpoint with
          | Plain x -> put x
          | WithUuid f -> put (f (get_current_uuid ()))
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
  {
    endpoint = Plain Api.configuration;
    readonly = true;
    content = None;
    dirty = true;
    ifmatch = "";
  }

let draft =
  {
    endpoint = WithUuid Api.draft;
    readonly = false;
    content = None;
    dirty = true;
    ifmatch = "";
  }

let voters =
  {
    endpoint = WithUuid Api.draft_voters;
    readonly = false;
    content = None;
    dirty = true;
    ifmatch = "";
  }

let status =
  {
    endpoint = WithUuid Api.draft_status;
    readonly = true;
    content = None;
    dirty = true;
    ifmatch = "";
  }

let account =
  {
    endpoint = Plain Api.account;
    readonly = false;
    content = None;
    dirty = true;
    ifmatch = "";
  }

let e_dates =
  {
    endpoint = WithUuid Api.election_auto_dates;
    readonly = false;
    content = None;
    dirty = true;
    ifmatch = "";
  }

let e_voters =
  {
    endpoint = WithUuid Api.election_voters;
    readonly = true;
    content = None;
    dirty = true;
    ifmatch = "";
  }

let e_records =
  {
    endpoint = WithUuid Api.election_records;
    readonly = true;
    content = None;
    dirty = true;
    ifmatch = "";
  }

let e_status =
  {
    endpoint = WithUuid Api.election_status;
    readonly = true;
    content = None;
    dirty = true;
    ifmatch = "";
  }

let e_elec =
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
        if x.dirty && x.content <> None then sync_one x else Lwt.return @@ Ok ())
      list_items
  in
  Lwt_list.fold_left_s
    (fun accu y ->
      match y with Error e -> Lwt.return @@ Error e | Ok _ -> Lwt.return accu)
    (Ok ()) list_items

let invalidate_all () = List.iter (fun (AA x) -> invalidate x) list_items

let set x y =
  assert (not x.readonly);
  x.dirty <- true;
  x.content <- Some y

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
