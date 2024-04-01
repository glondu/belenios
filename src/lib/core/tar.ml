(**************************************************************************)
(*                                BELENIOS                                *)
(*                                                                        *)
(*  Copyright © 2012-2024 Inria                                           *)
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

let block_size = 512

type header = { name : string; size : int64; timestamp : int64 }

let int64_of_octal x = Int64.of_string ("0o" ^ x)

let compute_checksum x =
  let sum = ref 0 in
  for i = 0 to Bytes.length x - 1 do
    sum := !sum + int_of_char (Bytes.get x i)
  done;
  Printf.sprintf "%06o\000 " !sum

let write_to_bytes buffer pos str =
  Bytes.blit_string str 0 buffer pos (String.length str)

let bytes_of_header { name; size; timestamp } =
  if String.length name >= 100 then
    failwith "Tar.bytes_of_header: name too long";
  let b = Bytes.make block_size '\000' in
  write_to_bytes b 0 name;
  write_to_bytes b 100 "0000644";
  write_to_bytes b 108 "0000000";
  write_to_bytes b 116 "0000000";
  write_to_bytes b 124 (Printf.sprintf "%011Lo" size);
  write_to_bytes b 136 (Printf.sprintf "%011Lo" timestamp);
  write_to_bytes b 148 "        ";
  write_to_bytes b 156 "0";
  write_to_bytes b 148 (compute_checksum b);
  b

let header_of_bytes b =
  let name =
    let i = 0 in
    let j = Bytes.index_from b i '\000' in
    Bytes.sub_string b i (j - i)
  in
  let size = Bytes.sub_string b 124 11 |> int64_of_octal in
  let timestamp = Bytes.sub_string b 136 11 |> int64_of_octal in
  let r = { name; size; timestamp } in
  let reproducible = b = bytes_of_header r in
  if reproducible then { name; size; timestamp }
  else failwith "Tar.header_of_bytes: not reproducible"
