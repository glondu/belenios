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

module SMap = Map.Make (String)

type t = string list list SMap.t

let parse_upto_To ic =
  let rec loop accu =
    match input_line ic with
    | exception End_of_file -> (None, accu)
    | line ->
        if String.starts_with ~prefix:"To: " line then
          let i = String.index line '<' + 1 in
          let j = String.index line '>' in
          (Some (String.sub line i (j - i)), accu)
        else loop (line :: accu)
  in
  loop []

let add address lines accu =
  let current = SMap.find_opt address accu in
  let current = Option.value ~default:[] current in
  SMap.add address (lines :: current) accu

let parse ic =
  let rec loop address accu =
    let address', lines = parse_upto_To ic in
    let accu = add address lines accu in
    match address' with None -> accu | Some address' -> loop address' accu
  in
  loop "" SMap.empty

let extract ~prefix emails voter =
  let nprefix = String.length prefix in
  SMap.find voter emails
  |> List.find_map (fun email ->
      List.find_map
        (fun line ->
          let n = String.length line in
          if String.starts_with ~prefix line then
            Some (String.sub line nprefix (n - nprefix))
          else None)
        email)

let extract_password = extract ~prefix:"Password: "
let extract_credential = extract ~prefix:"Your credential: "
let extract_code = extract ~prefix:"  "

let extract_credop x v =
  let server = extract ~prefix:"Election server: " x v in
  let uuid = extract ~prefix:"Election identifier: " x v in
  let key = extract ~prefix:"Secret key: " x v in
  match (server, uuid, key) with
  | Some server, Some uuid, Some key -> Some (server, uuid, key)
  | _ -> None
