(**************************************************************************)
(*                                BELENIOS                                *)
(*                                                                        *)
(*  Copyright © 2012-2016 Inria                                           *)
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
open Web_signatures

type user = {
  uuid: Uuidm.t option;
  service : string;
  name : string;
  logout : unit -> content;
}

let scope = Eliom_common.default_session_scope

let user = Eliom_reference.eref ~scope None

let get_site_user () =
  match_lwt Eliom_reference.get user with
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
  match_lwt Eliom_reference.get user with
  | None -> return None
  | Some u ->
     match u.uuid with
     | None -> return None
     | Some uuid' ->
        if Uuidm.equal uuid uuid' then
          return @@ Some {
            user_domain = u.service;
            user_name = u.name
          }
        else
          return None


let cont = Eliom_reference.eref ~scope []

let cont_push f =
  let open Eliom_reference in
  lwt fs = get cont in
  set cont (f :: fs)

let cont_pop () =
  let open Eliom_reference in
  lwt fs = get cont in
  match fs with
  | f :: fs -> set cont fs >> return (Some f)
  | [] -> return None


let ballot = Eliom_reference.eref ~scope None
let cast_confirmed = Eliom_reference.eref ~scope None
