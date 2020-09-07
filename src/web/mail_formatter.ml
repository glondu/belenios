(**************************************************************************)
(*                                BELENIOS                                *)
(*                                                                        *)
(*  Copyright © 2012-2020 Inria                                           *)
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

let width = 72

type t =
  {
    buffer : Buffer.t;
    mutable current : int;
    mutable pending : bool;
  }

let create () =
  {
    buffer = Buffer.create 1000;
    current = 0;
    pending = false;
  }

let add_newline t =
  Buffer.add_string t.buffer "\n";
  t.current <- 0;
  t.pending <- false

let add_string t s =
  Buffer.add_string t.buffer s;
  t.current <- t.current + String.length s;
  t.pending <- false

let add_word t s =
  let length = String.length s in
  let pending = if t.pending then 1 else 0 in
  if t.current + pending + length > width then (
    add_newline t;
    add_string t s;
    t.pending <- true
  ) else (
    if t.pending then add_string t " ";
    add_string t s;
    t.pending <- true
  )

let add_sentence t s =
  let n = String.length s in
  let rec loop i =
    if i < n then (
      let j = try String.index_from s i ' ' with Not_found -> n in
      add_word t (String.sub s i (j - i));
      loop (j + 1)
    )
  in
  loop 0

let contents t =
  Buffer.contents t.buffer
