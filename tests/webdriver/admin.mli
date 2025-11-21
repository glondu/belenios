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

type admin =
  | Local of { username : string; password : string }
  | Demo of { username : string }

type question =
  | Select of {
      question : string;
      min : int;
      max : int;
      blank : bool;
      answers : string list;
    }

type trustee = { name : string; email : string }
type trustee_mode = Basic | Threshold of int
type trustees = { mode : trustee_mode; trustees : trustee list }
type auth = Password | Email
type external_registrar = { server : string; operator : string }
type registrar = { name : string; ext : external_registrar option }

type config = {
  questions : question list;
  voters : string list;
  trustees : trustees;
  registrar : registrar option;
  auth : auth;
}

module type CONFIG = sig
  val width : int
  val height : int
  val webdriver : string
  val belenios : string
  val headless : bool
  val admin : admin
  val config : config
  val emails : in_channel
end

type private_creds =
  | Creds of Yojson.Safe.t
  | Credop of { server : string; uuid : string; key : string }

type election_params = {
  id : string;
  private_keys : string list;
  private_creds : private_creds option;
}

module Make (_ : CONFIG) : sig
  val setup_election : unit -> election_params Lwt.t
  val regen_password : id:string -> username:string -> string Lwt.t
  val tally_election : (unit -> unit Lwt.t) -> election_params -> unit Lwt.t
end
