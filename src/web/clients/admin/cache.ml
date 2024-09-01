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
open Belenios_api
open Belenios
open Belenios_js.Common
open Belenios_js.Session
open Common
open Lwt.Syntax

type endpoint =
  | Plain of string
  | WithUuid of { fmt : 'a 'b 'c. (string -> 'a, 'b, 'c, 'a) format4 }

type 'a t = {
  endpoint : endpoint;
  of_string : string -> 'a;
  to_string : 'a -> string;
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
      let url =
        match x.endpoint with
        | Plain x -> x
        | WithUuid { fmt } ->
            Printf.sprintf fmt (Uuid.unwrap (get_current_uuid ()))
      in
      let* content = raw_get_with_token ~token:!token Fun.id "%s" url in
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
      | Ok (content_str, ifmatch) ->
          let content = x.of_string content_str in
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
        let content_str = x.to_string content in
        let ifmatch = x.ifmatch in
        let url =
          match x.endpoint with
          | Plain x -> x
          | WithUuid { fmt } ->
              Printf.sprintf fmt (Uuid.unwrap (get_current_uuid ()))
        in
        let* y =
          raw_put_with_token ~ifmatch ~token:!token content_str "%s" url
        in
        match y.code with
        | 200 ->
            x.dirty <- false;
            x.ifmatch <- sha256_b64 content_str;
            Lwt.return @@ Ok ()
        | code ->
            Lwt.return
            @@ Error
                 (Printf.sprintf "Failed to sync data to server, code %d" code))

(***********************************)

let config =
  {
    endpoint = Plain "configuration";
    of_string = configuration_of_string;
    to_string = string_of_configuration;
    readonly = true;
    content = None;
    dirty = true;
    ifmatch = "";
  }

let draft =
  {
    endpoint = WithUuid { fmt = "drafts/%s" };
    of_string = draft_of_string;
    to_string = string_of_draft;
    readonly = false;
    content = None;
    dirty = true;
    ifmatch = "";
  }

let voters =
  {
    endpoint = WithUuid { fmt = "drafts/%s/voters" };
    of_string = voter_list_of_string;
    to_string = string_of_voter_list;
    readonly = false;
    content = None;
    dirty = true;
    ifmatch = "";
  }

let status =
  {
    endpoint = WithUuid { fmt = "drafts/%s/status" };
    of_string = draft_status_of_string;
    to_string = string_of_draft_status;
    readonly = true;
    content = None;
    dirty = true;
    ifmatch = "";
  }

let account =
  {
    endpoint = Plain "account";
    of_string = api_account_of_string;
    to_string = string_of_api_account;
    readonly = false;
    content = None;
    dirty = true;
    ifmatch = "";
  }

let e_dates =
  {
    endpoint = WithUuid { fmt = "elections/%s/automatic-dates" };
    of_string = election_auto_dates_of_string;
    to_string = string_of_election_auto_dates;
    readonly = false;
    content = None;
    dirty = true;
    ifmatch = "";
  }

let e_voters =
  {
    endpoint = WithUuid { fmt = "elections/%s/voters" };
    of_string = Voter.list_of_string;
    to_string = string_of_voter_list;
    readonly = true;
    content = None;
    dirty = true;
    ifmatch = "";
  }

let e_records =
  {
    endpoint = WithUuid { fmt = "elections/%s/records" };
    of_string = records_of_string;
    to_string = string_of_records;
    readonly = true;
    content = None;
    dirty = true;
    ifmatch = "";
  }

let e_status =
  {
    endpoint = WithUuid { fmt = "elections/%s" };
    of_string = election_status_of_string;
    to_string = string_of_election_status;
    readonly = true;
    content = None;
    dirty = true;
    ifmatch = "";
  }

let e_elec =
  {
    endpoint = WithUuid { fmt = "elections/%s/election" };
    of_string = Belenios.Election.template_of_string;
    to_string = Belenios.Election.string_of_template;
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
