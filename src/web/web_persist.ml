(**************************************************************************)
(*                                BELENIOS                                *)
(*                                                                        *)
(*  Copyright © 2012-2015 Inria                                           *)
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
open Serializable_t
open Common

type election_state =
  [ `Open
  | `Closed
  | `EncryptedTally of int * int * string
  | `Tallied of plaintext
  ]

let election_states = Ocsipersist.open_table "election_states"

let get_election_state x =
  try_lwt Ocsipersist.find election_states x
  with Not_found -> return `Open

let set_election_state x s =
  Ocsipersist.add election_states x s

let store = Ocsipersist.open_store "site"

lwt main_election =
  Ocsipersist.make_persistent store "main_election" None

lwt featured =
  Ocsipersist.make_persistent store "featured_elections" []

let add_featured_election x =
  lwt the_featured = Ocsipersist.get featured in
  if List.mem x the_featured then (
    return ()
  ) else (
    Ocsipersist.set featured (x :: the_featured)
  )

let rec list_remove x = function
  | [] -> []
  | y :: ys -> if x = y then ys else y :: (list_remove x ys)

let remove_featured_election x =
  lwt the_featured = Ocsipersist.get featured in
  Ocsipersist.set featured (list_remove x the_featured)

let is_featured_election x =
  lwt the_featured = Ocsipersist.get featured in
  return (List.mem x the_featured)

let get_featured_elections () =
  Ocsipersist.get featured

let get_main_election () =
  Ocsipersist.get main_election

let set_main_election x =
  Ocsipersist.set main_election (Some x)

let unset_main_election () =
  Ocsipersist.set main_election None

let election_pds = Ocsipersist.open_table "election_pds"

let get_partial_decryptions x =
  try_lwt Ocsipersist.find election_pds x
  with Not_found -> return []

let set_partial_decryptions x pds =
  Ocsipersist.add election_pds x pds

let auth_configs = Ocsipersist.open_table "auth_configs"

let get_auth_config x =
  try_lwt Ocsipersist.find auth_configs x
  with Not_found -> return []

let set_auth_config x c =
  Ocsipersist.add auth_configs x c
