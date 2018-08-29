(**************************************************************************)
(*                                BELENIOS                                *)
(*                                                                        *)
(*  Copyright © 2012-2018 Inria                                           *)
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

open Lwt
open Web_serializable_t

type user = {
  uuid: uuid option;
  service : string;
  name : string;
}

let scope = Eliom_common.default_session_scope

let show_cookie_disclaimer = Eliom_reference.eref ~scope true

let user = Eliom_reference.eref ~scope None

let get_site_user () =
  match%lwt Eliom_reference.get user with
  | None -> return None
  | Some u ->
     match u.uuid with
     | None ->
        return @@ Some {
          user_domain = u.service;
          user_name = u.name;
        }
     | Some _ -> return None

let get_election_user uuid =
  match%lwt Eliom_reference.get user with
  | None -> return None
  | Some u ->
     match u.uuid with
     | None -> return None
     | Some uuid' ->
        if uuid = uuid' then
          return @@ Some {
            user_domain = u.service;
            user_name = u.name
          }
        else
          return None


let cont = Eliom_reference.eref ~scope []

let cont_push f =
  let open Eliom_reference in
  let%lwt fs = get cont in
  set cont (f :: fs)

let cont_pop () =
  let open Eliom_reference in
  let%lwt fs = get cont in
  match fs with
  | f :: fs ->
     let%lwt () = set cont fs in
     return (Some f)
  | [] -> return None


let ballot = Eliom_reference.eref ~scope None
let cast_confirmed = Eliom_reference.eref ~scope None

let get_default_language () =
  let ri = Eliom_request_info.get_ri () in
  let lazy langs = Ocsigen_request_info.accept_language ri in
  match langs with
  | [] -> "en"
  | (lang, _) :: _ ->
     let n =
       try String.index lang '-'
       with Not_found -> String.length lang
     in
     String.sub lang 0 n

let language = Eliom_reference.eref_from_fun ~scope get_default_language

let signup_address = Eliom_reference.eref ~scope None
