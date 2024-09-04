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
open Js_of_ocaml
open Js_of_ocaml_tyxml
open Tyxml_js.Html
open Belenios
open Belenios_js.Common
open Belenios_js.Session
open Common

let make_ballots_contents uuid show_weights sized_encrypted_tally ballots =
  let open (val !Belenios_js.I18n.gettext) in
  let ballots =
    ballots
    |> List.map (fun (h, w) -> (Hash.to_b64 h, h, w))
    |> List.sort (fun (a, _, _) (b, _, _) -> compare_b64 a b)
    |> List.map (fun (b, h, w) ->
           let href =
             !/Belenios_api.Endpoints.((election_object uuid h).path)
           in
           li
             [
               a ~href b;
               (if show_weights then
                  Printf.ksprintf txt " (%s)" (Weight.to_string w)
                else txt "");
             ])
  in
  let links =
    let href = Printf.sprintf "#%s" (Uuid.unwrap uuid) in
    p [ a ~href (s_ "Go back to election") ]
  in
  let number =
    let n = List.length ballots in
    match sized_encrypted_tally with
    | None ->
        div
          [
            txt @@ string_of_int n;
            txt @@ s_ " ballot(s) have been accepted so far.";
          ]
    | Some x when x.sized_num_tallied = n ->
        div
          [ txt @@ string_of_int n; txt @@ s_ " ballot(s) have been accepted." ]
    | Some x ->
        (* should not happen *)
        div
          [
            txt @@ string_of_int n;
            txt @@ s_ " ballot(s) have been accepted, and ";
            txt @@ string_of_int x.sized_num_tallied;
            txt @@ s_ " have been tallied.";
          ]
  in
  [ number; ul ballots; links ]

let ballots uuid =
  let open (val !Belenios_js.I18n.gettext) in
  let@ election cont =
    let* x = Api.(get (election uuid) `Nobody) in
    match x with
    | Error _ -> Lwt.return @@ error "Could not get election parameters!"
    | Ok (x, _) -> cont @@ Election.of_string (module Random) x
  in
  let@ audit_cache cont =
    let* x = Api.(get (election_audit_cache uuid) `Nobody) in
    match x with
    | Error _ -> Lwt.return @@ error "Could not retrieve audit data!"
    | Ok (audit_cache, _) -> cont audit_cache
  in
  let@ sized_encrypted_tally cont =
    let ( let& ) x f =
      let* x = x in
      match x with Error _ -> cont None | Ok (x, _) -> f x
    in
    let& roots = Api.(get (election_roots uuid) `Nobody) in
    match roots.roots_encrypted_tally with
    | None -> cont None
    | Some t ->
        let& t = Api.(get (election_object uuid t) `Nobody) in
        cont @@ Some (sized_encrypted_tally_of_string read_hash t)
  in
  let show_weights = audit_cache.cache_checksums.ec_weights <> None in
  let container = div [ txt @@ s_ "Loading..." ] in
  let () =
    let@ () = Lwt.async in
    let* x = Api.(get (election_ballots uuid) `Nobody) in
    let contents =
      match x with
      | Error _ -> [ txt @@ s_ "Error while loading ballots!" ]
      | Ok (ballots, _) ->
          make_ballots_contents uuid show_weights sized_encrypted_tally ballots
    in
    let container = Tyxml_js.To_dom.of_div container in
    container##.innerHTML := Js.string "";
    List.iter
      (fun x -> Dom.appendChild container (Tyxml_js.To_dom.of_node x))
      contents;
    Lwt.return_unit
  in
  let module W = (val election) in
  let title = W.template.t_name ^^^ s_ "Accepted ballots" in
  let footer = [ make_audit_footer election ] in
  let contents = [ container ] in
  Lwt.return { title; contents; footer }
