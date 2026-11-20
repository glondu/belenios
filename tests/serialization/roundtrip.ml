(**************************************************************************)
(*                                BELENIOS                                *)
(*                                                                        *)
(*  Copyright © 2026-2026 Inria                                           *)
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

(* Decode every file of an extracted archive with its description, encode it
   again and check that the bytes are the same.

   Usage: roundtrip.exe DIR, where DIR contains the <hash>.<data|event>.json
   files of a .bel archive. *)

open Belenios

let failures = ref 0
let checked = ref 0

let check what raw desc =
  incr checked;
  match Desc.Json.to_string desc (Desc.Json.of_string desc raw) with
  | raw' when raw' = raw -> ()
  | raw' ->
      incr failures;
      Printf.printf "DIFF %s\n  original:   %s\n  re-encoded: %s\n" what raw
        raw'
  | exception e ->
      incr failures;
      Printf.printf "ERROR %s: %s\n" what (Printexc.to_string e)

let () =
  let dir = Sys.argv.(1) in
  let files = Hashtbl.create 1000 in
  Sys.readdir dir
  |> Array.iter (fun name ->
      match String.split_on_char '.' name with
      | [ h; kind; "json" ] ->
          let contents =
            In_channel.with_open_bin (Filename.concat dir name)
              In_channel.input_all
          in
          if Hash.hash_string contents <> Hash.of_hex h then (
            incr failures;
            Printf.printf "BAD HASH %s\n" name);
          Hashtbl.replace files h (kind, contents)
      | _ -> ());
  let get h = snd (Hashtbl.find files (Hash.to_hex h)) in
  let events =
    Hashtbl.fold
      (fun h (kind, raw) acc ->
        if kind = "event" then (
          check ("event " ^ h) raw Events.event_d;
          Desc.Json.of_string Events.event_d raw :: acc)
        else acc)
      files []
    |> List.sort (fun (a : Events.event) b -> compare a.height b.height)
  in
  let setup =
    List.find (fun (e : Events.event) -> e.typ = `Setup) events |> fun e ->
    Option.get e.payload |> get
  in
  check "setup" setup setup_data_d;
  let setup = Desc.Json.of_string setup_data_d setup in
  let election = get setup.election in
  let { Belenios_v2.version; group; _ } =
    Desc.Json.of_string (Belenios_v2.params_d Desc.Core.json) election
  in
  let module G = (val Group.make { version; group }) in
  let g = Desc.Core.map Desc.Core.string G.of_string G.to_string in
  let zq = Desc.Core.map Desc.Core.string G.Zq.of_string G.Zq.to_string in
  check "election" election (Belenios_v2.params_d g);
  check "trustees" (get setup.trustees) (trustees_d g zq);
  check "credentials" (get setup.credentials) (public_credentials_d g);
  Option.iter
    (fun h ->
      check "credentials certificate" (get h) (credentials_certificate_d g zq))
    setup.credentials_certificate;
  List.iter
    (fun (e : Events.event) ->
      let payload what desc =
        Option.iter (fun h -> check what (get h) desc) e.payload
      in
      let owned what desc =
        Option.iter
          (fun h ->
            check (what ^ " (owned)") (get h) (owned_d hash_d);
            let o = Desc.Json.of_string (owned_d hash_d) (get h) in
            check what (get o.payload) desc)
          e.payload
      in
      match e.typ with
      | `Setup | `EndBallots | `EndShuffles -> ()
      | `Ballot -> payload "ballot" (ballot_d g zq)
      | `EncryptedTally ->
          Option.iter
            (fun h ->
              check "sized encrypted tally" (get h)
                (sized_encrypted_tally_d hash_d);
              let t =
                Desc.Json.of_string (sized_encrypted_tally_d hash_d) (get h)
              in
              check "encrypted tally" (get t.encrypted_tally)
                (encrypted_tally_d g))
            e.payload
      | `Shuffle -> owned "shuffle" (shuffle_d g zq)
      | `PartialDecryption ->
          owned "partial decryption" (partial_decryption_d g zq)
      | `Result -> payload "result" (election_result_d Desc.Core.json))
    events;
  Printf.printf "%s: %d objects checked, %d failures\n" dir !checked !failures;
  if !failures > 0 then exit 1
