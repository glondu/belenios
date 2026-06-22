(**************************************************************************)
(*                                BELENIOS                                *)
(*                                                                        *)
(*  Copyright © 2012-2021 Inria                                           *)
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

type raw_ballots = int array array [@@deriving yojson]
type processed_ballots = int list list [@@deriving yojson]

type event =
  [ `Win of int list
  | `Lose of int
  | `TieWin of int list
  | `TieLose of int list ]
[@@deriving yojson]

type events = event list [@@deriving yojson]

type result = {
  ballots : processed_ballots;
  invalid : raw_ballots;
  events : events;
  winners : int list;
}
[@@deriving yojson]

val compute : nseats:int -> raw_ballots -> result
