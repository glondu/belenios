(**************************************************************************)
(*                                BELENIOS                                *)
(*                                                                        *)
(*  Copyright © 2012-2016 Inria                                           *)
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

open Signatures
open Serializable_j
open Common

let stream_to_list s =
  let res = ref [] in
  Stream.iter (fun x -> res := x :: !res) s;
  List.rev !res

let lines_of_file fname =
  let ic = open_in fname in
  Stream.from (fun _ ->
    try Some (input_line ic)
    with End_of_file -> close_in ic; None
  )

let string_of_file f =
  lines_of_file f |> stream_to_list |> String.concat "\n"

let string_of_file_opt filename =
  if Sys.file_exists filename then Some (string_of_file filename) else None

let load_from_file of_string filename =
  if Sys.file_exists filename then (
    Some (lines_of_file filename |> stream_to_list |> List.rev_map of_string)
  ) else None

let ( / ) = Filename.concat

type verifydiff_error =
  | ElectionMismatch
  | PublicKeysMismatch
  | MissingPublicKeys
  | InvalidPublicKeys
  | InvalidThreshold
  | PublicKeyMismatch
  | ThresholdMismatch
  | MissingCredentials
  | InvalidCredential
  | CredentialsMismatch
  | MissingBallots
  | InvalidBallot
  | DuplicateBallot
  | BallotSignedByInvalidKey
  | DecreasingBallots
  | BallotSignedByReplacedKey

exception VerifydiffError of verifydiff_error

let explain_error = function
  | ElectionMismatch -> "election mismatch"
  | PublicKeysMismatch -> "public keys mismatch"
  | MissingPublicKeys -> "missing public keys"
  | InvalidPublicKeys -> "invalid public keys"
  | InvalidThreshold -> "invalid threshold parameters"
  | PublicKeyMismatch -> "public key mismatch"
  | ThresholdMismatch -> "threshold parameters mismatch"
  | MissingCredentials -> "missing credentials"
  | InvalidCredential -> "invalid credential"
  | CredentialsMismatch -> "credentials mismatch"
  | MissingBallots -> "missing ballots"
  | InvalidBallot -> "invalid ballot"
  | DuplicateBallot -> "duplicate ballot"
  | BallotSignedByInvalidKey -> "ballot signed by invalid key"
  | DecreasingBallots -> "decreasing ballots"
  | BallotSignedByReplacedKey -> "ballot signed by replaced key"

let () =
  Printexc.register_printer (function
      | VerifydiffError e -> Some ("verify-diff error: " ^ explain_error e)
      | _ -> None)

let verifydiff dir1 dir2 =
  (* the elections must be the same *)
  let election = string_of_file (dir1 / "election.json") in
  let () =
    let election2 = string_of_file (dir2 / "election.json") in
    if election2 <> election then raise (VerifydiffError ElectionMismatch)
  in
  (* the public keys must be the same *)
  let pks = load_from_file (fun x -> x) (dir1 / "public_keys.jsons") in
  let () =
    let pks2 = load_from_file (fun x -> x) (dir2 / "public_keys.jsons") in
    if pks2 <> pks then raise (VerifydiffError PublicKeysMismatch)
  in
  (* the threshold parameters must be the same *)
  let threshold = string_of_file_opt (dir1 / "threshold.json") in
  let () =
    let t2 = string_of_file_opt (dir2 / "threshold.json") in
    if t2 <> threshold then raise (VerifydiffError ThresholdMismatch)
  in
  (* the public keys / threshold parameters must be valid *)
  let module ED = (val Election.(get_group (of_string election))) in
  let open ED in
  let module E = Election.Make (ED) (DirectRandom) in
  let y =
    match threshold with
    | None ->
       let module K = Trustees.MakeSimple (G) (DirectRandom) in
       let pks = match pks with
         | None -> raise (VerifydiffError MissingPublicKeys)
         | Some pks -> List.map (trustee_public_key_of_string G.read) pks
       in
       if not (List.for_all K.check pks) then
         raise (VerifydiffError InvalidPublicKeys);
       K.combine (Array.of_list pks)
    | Some t ->
       let t = threshold_parameters_of_string G.read t in
       let module P = Trustees.MakePKI (G) (DirectRandom) in
       let module C = Trustees.MakeChannels (G) (DirectRandom) (P) in
       let module K = Trustees.MakePedersen (G) (DirectRandom) (P) (C) in
       if not (K.check t) then
         raise (VerifydiffError InvalidThreshold);
       K.combine t
  in
  (* the public keys must correspond to the public key of election *)
  let () =
    if not G.(election.e_params.e_public_key =~ y) then
      raise (VerifydiffError PublicKeyMismatch)
  in
  (* load both public_creds.txt and check that their contents is valid *)
  let module GSet = Set.Make (G) in
  let creds dir =
    match load_from_file G.of_string (dir / "public_creds.txt") with
    | None -> raise (VerifydiffError MissingCredentials)
    | Some creds ->
       if not (List.for_all G.check creds) then
         raise (VerifydiffError InvalidCredential);
       List.fold_left (fun accu x -> GSet.add x accu) GSet.empty creds
  in
  let creds1 = creds dir1 and creds2 = creds dir2 in
  (* both public_creds.txt have the same cardinal *)
  let () =
    if GSet.cardinal creds1 <> GSet.cardinal creds2 then
      raise (VerifydiffError CredentialsMismatch)
  in
  (* compute credentials that have been replaced *)
  let creds_replaced =
    GSet.fold (fun x accu ->
        if not (GSet.mem x creds2) then GSet.add x accu else accu
      ) creds1 GSet.empty
  in
  (* issue a warning when credentials have changed *)
  let () =
    if not (GSet.is_empty creds_replaced) then
      Printf.eprintf "W: credentials have changed\n%!"
  in
  (* load both ballots.jsons and check that their contents is valid *)
  let module GMap = Map.Make (G) in
  let ballots dir =
    match load_from_file (ballot_of_string G.read) (dir / "ballots.jsons") with
    | None -> raise (VerifydiffError MissingBallots)
    | Some ballots ->
       if not (List.for_all E.check_ballot ballots) then
         raise (VerifydiffError InvalidBallot);
       (* return the set of ballots indexed by the public keys used to sign *)
       List.fold_left (fun accu x ->
           match x.signature with
           | None -> raise (VerifydiffError InvalidBallot)
           | Some s -> if GMap.mem s.s_public_key accu then
                         raise (VerifydiffError DuplicateBallot)
                       else GMap.add s.s_public_key x accu
         ) GMap.empty ballots
  in
  let ballots1 = ballots dir1 and ballots2 = ballots dir2 in
  (* each ballot is signed with a valid key *)
  let check_keys ballots creds =
    GMap.for_all (fun pk _ -> GSet.mem pk creds) ballots
  in
  let () =
    if not (check_keys ballots1 creds1 && check_keys ballots2 creds2) then
      raise (VerifydiffError BallotSignedByInvalidKey)
  in
  (* the set of ballots increases *)
  let () =
    if not (GMap.for_all (fun pk _ -> GMap.mem pk ballots2) ballots1) then
      raise (VerifydiffError DecreasingBallots)
  in
  let () =
    let n = GMap.cardinal ballots2 - GMap.cardinal ballots1 in
    if n > 0 then Printf.eprintf "I: %d new ballot(s)\n%!" n
  in
  (* the keys of modified ballots have not been replaced *)
  let () =
    if not (GMap.for_all (fun pk ballot1 ->
                let ballot2 = GMap.find pk ballots2 in
                ballot1 = ballot2 || not (GSet.mem pk creds_replaced)
              ) ballots1)
    then raise (VerifydiffError BallotSignedByReplacedKey)
  in
  let () =
    let n = GMap.fold (fun pk ballot1 accu ->
                let ballot2 = GMap.find pk ballots2 in
                if ballot1 <> ballot2 then accu + 1 else accu
              ) ballots1 0
    in
    if n > 0 then Printf.eprintf "W: %d ballot(s) have been replaced\n%!" n
  in
  Printf.eprintf "I: all tests passed!\n%!"
