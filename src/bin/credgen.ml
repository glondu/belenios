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

open Util

let remove_dashes x =
  let n = String.length x in
  let res = Buffer.create n in
  for i = 0 to n-1 do
    let c = x.[i] in
    if c <> '-' then Buffer.add_char res c;
  done;
  Buffer.contents res

let derive uuid x =
  let open Cryptokit in
  let uuid = remove_dashes (Uuidm.to_string uuid) in
  let salt = transform_string (Hexa.decode ()) uuid in
  pbkdf2 ~prf:MAC.hmac_sha256 ~iterations:1000 ~size:1 ~salt x |>
  transform_string (Hexa.encode ())

module RunCredgen (X : sig end) = struct

  (* Setup group *)

  module G = Election.DefaultGroup;;
  assert (Election.check_finite_field G.group);;

  (* Some helpers *)

  (* Beware: the following must be changed in accordance with the booth! *)
  let digits = "123456789ABCDEFGHJKLMNPQRSTUVWXYZabcdefghijkmnopqrstuvwxyz"
  let token_length = 14
  let n58 = Z.of_int 58
  let n53 = Z.of_int 53

  let public_key_of_token uuid x =
    let hex = derive uuid x in
    let x = Z.(of_string_base 16 hex mod G.q) in
    let y = G.(g **~ x) in
    Z.to_string y

  (* Argument parsing *)

  let dir = ref (Sys.getcwd ())
  let uuid = ref None
  let count = ref None
  let file = ref None
  let derive = ref None

  let speclist = Arg.([
    "--dir", String (fun s -> dir := s), "directory where output will be written";
    "--uuid", String (fun s -> uuid := Some s), "UUID of the election";
    "--count", Int (fun i -> count := Some i), "number of credentials to generate";
    "--file", String (fun s -> file := Some s), "file with list of identities";
    "--derive", String (fun s -> derive := Some s), "derive public credential from given private one";
  ])

  let usage_msg =
    Printf.sprintf "Usage: %s credgen [--dir <dir>] --uuid <uuid> {--count <n> | --file <file> | --derive <privcred>}" Sys.argv.(0)

  let anon_fun x =
    Printf.eprintf "I do not know what to do with %s!\n" x;
    exit 1

  let () = Arg.parse speclist anon_fun usage_msg

  let uuid = match !uuid with
    | None ->
      Printf.eprintf "UUID is missing!\n";
      exit 1
    | Some u ->
      match Uuidm.of_string u with
        | Some u -> u
        | None ->
          Printf.eprintf "UUID is invalid!\n";
          exit 1

  let count, ids =
    match !count, !file, !derive with
      | Some i, None, None ->
        if i < 1 then (
          Printf.eprintf "You must generate at least one credential!\n";
          exit 1
        ); i, None
      | None, Some f, None ->
        let ic = open_in f in
        let rec loop accu =
          match (try Some (input_line ic) with End_of_file -> None) with
            | Some "" -> loop accu
            | Some x -> loop (x::accu)
            | None -> List.rev accu
        in
        let res = loop [] in
        close_in ic;
        List.length res, Some res
      | None, None, Some d ->
        print_endline (public_key_of_token uuid d);
        exit 0
      | None, None, None ->
        Printf.eprintf "Nothing to do: use --count, --file or --derive!\n";
        exit 1
      | _, _, _ ->
        Printf.eprintf "Conflicting options!\n";
        exit 1
  ;;

  (* The generation itself, if requested *)

  let prng = Cryptokit.Random.(pseudo_rng (string secure_rng 16))
  let random_char () = int_of_char (Cryptokit.Random.string prng 1).[0]

  let generate_raw_token () =
    let res = String.create token_length in
    let rec loop i accu =
      if i < token_length then (
        let digit = random_char () mod 58 in
        res.[i] <- digits.[digit];
        loop (i+1) Z.(n58 * accu + of_int digit)
      ) else (res, accu)
    in loop 0 Z.zero

  let generate_token () =
    let (raw, value) = generate_raw_token () in
    let checksum = 53 - Z.(to_int (value mod n53)) in
    raw ^ String.make 1 digits.[checksum]

  let private_credentials =
    let rec loop i accu =
      if i > 0 then loop (i-1) (generate_token () :: accu)
      else accu
    in loop count []

  let public_credentials =
    List.map (public_key_of_token uuid) private_credentials

  let hashed_credentials = option_map (fun ids ->
    List.map2 (fun id cred ->
      Printf.sprintf "%s %s" (sha256_hex cred) id
    ) ids public_credentials
  ) ids

  (* Save to files *)

  let timestamp = Printf.sprintf "%.0f" (Unix.time ())

  let pub =
    "public credentials",
    timestamp ^ ".pubcreds",
    0o444,
    List.sort compare public_credentials

  let priv =
    let kind, creds = match ids with
      | None -> "private credentials", private_credentials
      | Some ids -> "private credentials with ids",
        List.map2 (fun id cred ->
          Printf.sprintf "%s %s" cred id
        ) ids private_credentials
    in
    kind,
    timestamp ^ ".privcreds",
    0o400,
    List.sort compare creds

  let hashed = option_map (fun h ->
    "hashed credentials with ids",
    timestamp ^ ".hashcreds",
    0o400,
    List.sort compare h
  ) hashed_credentials

  let output_endline oc x =
    output_string oc x;
    output_char oc '\n'

  let save (kind, filename, perm, thing) =
    let full_filename = Filename.concat !dir filename in
    let oc = open_out_gen [
      Open_wronly; Open_creat; Open_excl
    ] perm full_filename in
    List.iter (output_endline oc) thing;
    close_out oc;
    Printf.printf "%d %s saved to %s\n%!" count kind full_filename;;

  save pub;;
  save priv;;
  ignore (option_map save hashed);;

end


let main () =
  let module X = RunCredgen (struct end) in
  ()
