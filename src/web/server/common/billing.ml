(**************************************************************************)
(*                                BELENIOS                                *)
(*                                                                        *)
(*  Copyright © 2023-2023 Inria                                           *)
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
open Belenios
open Belenios_web_api
open Belenios_server_core
open Api_generic
module F = Ocsipersist.Functorial

module Request_table =
  F.Table
    (struct
      let name = "billing_requests"
    end)
    (F.Column.String)
    (F.Column.String)

let create ~admin_id ~uuid ~nb_voters =
  let date = Unix.gettimeofday () in
  let r = { admin_id; date; uuid; nb_voters } |> string_of_billing_request in
  let rec find_id () =
    let id = generate_token ~length:22 () in
    Lwt.catch
      (fun () ->
        let* _ = Request_table.find id in
        find_id ())
      (function Not_found -> Lwt.return id | e -> Lwt.fail e)
  in
  let* id = find_id () in
  let* () = Request_table.add id r in
  Lwt.return id

let remove ~id =
  Lwt.catch
    (fun () -> Request_table.remove id)
    (function Not_found -> Lwt.return_unit | e -> Lwt.fail e)

let check ~url ~id =
  Lwt.catch
    (fun () ->
      let url = Printf.sprintf "%s/check?id=%s" url id in
      let* response, x = Cohttp_lwt_unix.Client.get (Uri.of_string url) in
      let* () = Cohttp_lwt.Body.drain_body x in
      match Cohttp.Code.code_of_status response.status with
      | 200 -> Lwt.return_true
      | _ -> Lwt.return_false)
    (fun _ -> Lwt.return_false)

let lookup id =
  Lwt.catch
    (fun () ->
      let* x = Request_table.find id in
      Lwt.return_some @@ billing_request_of_string x)
    (function Not_found -> Lwt.return_none | e -> Lwt.fail e)

(* Forward reference set in Api_drafts *)
let validate = ref (fun ~admin_id:_ _uuid -> failwith "Billing.validate")

let dispatch ~token:_ ~ifmatch:_ endpoint method_ _body =
  match endpoint with
  | [ "requests"; id ] -> (
      match method_ with
      | `GET -> (
          let@ () = handle_generic_error in
          let* x = lookup id in
          match x with
          | None -> not_found
          | Some x -> return_json 200 (string_of_billing_request x))
      | _ -> method_not_allowed)
  | [ "requests"; id; "validate" ] -> (
      match method_ with
      | `POST -> (
          let@ () = handle_generic_error in
          let* x = lookup id in
          match x with
          | None -> not_found
          | Some { admin_id; uuid; _ } -> !validate ~admin_id uuid)
      | _ -> method_not_allowed)
  | _ -> not_found
