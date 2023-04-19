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

open Belenios_core
open Serializable_j

let rec input_lines ic lines =
  match input_line ic with
  | line -> input_lines ic (line :: lines)
  | exception End_of_file -> lines

let tally_txt = input_lines stdin []

let nchoices =
  let rex = Pcre.regexp "^V: ([-1-9]+)\\s" in
  match tally_txt with
  | [] -> failwith "No lines in input"
  | line :: _ -> (
      match Pcre.exec ~rex line with
      | s -> String.length (Pcre.get_substring s 1)
      | exception Not_found -> failwith "Could not parse last line as a ballot")

let rex =
  let buf = Buffer.create 32 in
  Buffer.add_string buf "^V: ";
  for _ = 1 to nchoices do
    Buffer.add_string buf "(.)"
  done;
  Buffer.add_string buf "\\s";
  Pcre.regexp (Buffer.contents buf)

let rec convert accu = function
  | [] -> accu
  | line :: lines ->
      let accu =
        match Pcre.exec ~rex line with
        | s ->
            let get i =
              let x = Pcre.get_substring s (i + 1) in
              if x = "-" then 0 else int_of_string x
            in
            Array.init nchoices get :: accu
        | exception Not_found -> accu
      in
      convert accu lines

let tally = convert [] tally_txt |> Array.of_list
let () = print_endline (string_of_condorcet_ballots tally)
