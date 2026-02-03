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

open Lwt.Syntax
open Js_of_ocaml_tyxml
open Tyxml_js.Html
open Belenios
open Belenios_web_api
open Belenios_js.Common
open Belenios_js.Session

type page = {
  title : string;
  contents : Html_types.div_content_fun elt list;
  footer : Html_types.div_content_fun elt list;
}

let error x = { title = "Error"; contents = [ txt x ]; footer = [] }

let make_audit_footer election =
  let open (val !Belenios_js.I18n.gettext) in
  let open (val election : Election.ELECTION) in
  let uuid = Uuid.unwrap uuid in
  let parameters = !/(Printf.sprintf "elections/%s/election" uuid) in
  let public_data = !/(Printf.sprintf "elections/%s/last-event" uuid) in
  let advanced = Printf.sprintf "#%s/advanced" uuid in
  let administer = !!(Printf.sprintf "admin#%s" uuid) in
  div
    ~a:[ a_id "audit-footer" ]
    [
      div
        [
          div
            [
              txt @@ s_ "Audit data: ";
              a ~href:parameters (s_ "parameters");
              txt ", ";
              a ~href:public_data (s_ "public data");
              txt ". ";
              a ~href:advanced (s_ "Advanced mode");
              txt ". ";
              a ~href:administer
                ~a:[ a_id @@ Printf.sprintf "election_admin_%s" uuid ]
                (s_ "Administer this election");
              txt ".";
            ];
        ];
    ]

type cache = {
  uuid : uuid;
  status : election_status option;
  dates : election_auto_dates option;
  election : (module Election.ELECTION) option;
  audit_cache : audit_cache option;
  result : string option;
  sized_encrypted_tally : hash sized_encrypted_tally option;
  mutable ballots : ((hash * weight) list, unit) result option;
}

let cache = ref None
let clear_cache () = cache := None

let update_cache uuid =
  let* election =
    let* x = Api.(get (election uuid) `Nobody) in
    match x with
    | Error _ -> Lwt.return_none
    | Ok (x, _) -> Lwt.return_some (Election.of_string (module Random) x)
  in
  let ( ! ) x =
    let* x = Api.(get (x uuid) `Nobody) in
    match x with Error _ -> Lwt.return_none | Ok (x, _) -> Lwt.return_some x
  in
  let* status = !Api.election_status in
  let* dates = !Api.election_auto_dates in
  let* audit_cache = !Api.election_audit_cache in
  let@ result, sized_encrypted_tally =
   fun cont ->
    let ( let& ) x f =
      let* x = x in
      match x with Error _ -> cont (None, None) | Ok (x, _) -> f x
    in
    let& roots = Api.(get (election_roots uuid) `Nobody) in
    match roots.roots_result with
    | None -> cont (None, None)
    | Some result -> (
        match roots.roots_encrypted_tally with
        | None -> cont (None, None)
        | Some t ->
            let& result = Api.(get (election_object uuid result) `Nobody) in
            let& t = Api.(get (election_object uuid t) `Nobody) in
            let t = sized_encrypted_tally_of_string read_hash t in
            cont (Some result, Some t))
  in
  let it =
    {
      uuid;
      status;
      dates;
      election;
      audit_cache;
      result;
      sized_encrypted_tally;
      ballots = None;
    }
  in
  cache := Some it;
  Lwt.return it

let get getter uuid =
  match !cache with
  | Some x when x.uuid = uuid -> Lwt.return @@ getter x
  | _ ->
      let* x = update_cache uuid in
      Lwt.return @@ getter x

let get_election = get (fun x -> x.election)
let get_status = get (fun x -> x.status)
let get_dates = get (fun x -> x.dates)
let get_audit_cache = get (fun x -> x.audit_cache)
let get_result = get (fun x -> x.result)
let get_sized_encrypted_tally = get (fun x -> x.sized_encrypted_tally)

let get_ballots uuid =
  let* c =
    match !cache with
    | Some x when x.uuid = uuid -> Lwt.return x
    | _ -> update_cache uuid
  in
  match c.ballots with
  | Some (Ok x) -> Lwt.return_some x
  | Some (Error ()) -> Lwt.return_none
  | None -> (
      let* x = Api.(get (election_ballots uuid) `Nobody) in
      match x with
      | Error _ ->
          c.ballots <- Some (Error ());
          Lwt.return_none
      | Ok (ballots, _) ->
          c.ballots <- Some (Ok ballots);
          Lwt.return_some ballots)
