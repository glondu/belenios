(**************************************************************************)
(*                                BELENIOS                                *)
(*                                                                        *)
(*  Copyright © 2012-2014 Inria                                           *)
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

open Serializable_builtin_t
open Serializable_t
open Signatures
open Common
open Web_serializable_t

module type AUTH_SERVICES = sig

  val get_auth_systems : unit -> string list Lwt.t
  val get_user : unit -> user option Lwt.t

end

module type AUTH_LINKS = sig

  val login :
    string option ->
    (unit, unit,
     [> `Attached of
          ([> `Internal of [> `Service ] ], [> `Get ])
          Eliom_service.a_s ],
     [ `WithoutSuffix ], unit, unit,
     [< Eliom_service.registrable > `Unregistrable ],
     [> Eliom_service.http_service ])
    Eliom_service.service

  val logout :
    (unit, unit,
     [> `Attached of
          ([> `Internal of [> `Service ] ], [> `Get ])
          Eliom_service.a_s ],
     [ `WithoutSuffix ], unit, unit,
     [< Eliom_service.registrable > `Unregistrable ],
     [> Eliom_service.http_service ])
    Eliom_service.service

end

type content =
    Eliom_registration.browser_content Eliom_registration.kind Lwt.t

module type WEB_BALLOT_BOX = sig
  module Ballots : MONADIC_MAP_RO
    with type 'a m = 'a Lwt.t
    and type elt = string
    and type key = string
  module Records : MONADIC_MAP_RO
    with type 'a m = 'a Lwt.t
    and type elt = datetime * string
    and type key = string

  val cast : string -> string * datetime -> string Lwt.t
  val inject_cred : string -> unit Lwt.t
  val update_files : unit -> unit Lwt.t
  val update_cred : old:string -> new_:string -> unit Lwt.t

  val compute_encrypted_tally : unit -> (int * string * string) Lwt.t
  (** Computes and writes to disk the encrypted tally. Returns the
      number of ballots and the hash of the encrypted tally. *)
end

module type WEB_PARAMS = sig
  val metadata : metadata
  val dir : string
end

module type WEB_ELECTION = sig
  include ELECTION_DATA
  include WEB_PARAMS
  module E : ELECTION with type elt = G.t and type 'a m = 'a Lwt.t
  module B : WEB_BALLOT_BOX
end

type election_files = {
  f_election : string;
  f_metadata : string;
  f_public_keys : string;
  f_public_creds : string;
  f_voters : string;
}

module type REGISTRABLE_ELECTION = sig
  val discard : unit -> unit
  val register : unit -> (module WEB_ELECTION) Lwt.t
end
