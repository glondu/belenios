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

type auth = Password | Email
type config = { questions : question list; voters : string list; auth : auth }

module type CONFIG = sig
  val webdriver : string
  val belenios : string
  val headless : bool
  val admin : admin
  val config : config
  val emails : in_channel
end

module Make (Config : CONFIG) : sig
  val setup_election : unit -> string Lwt.t
  val regen_password : election_id:string -> username:string -> string Lwt.t
  val tally_election : election_id:string -> unit Lwt.t
end
