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

open Belenios_platform.Platform
open Belenios_core
open Signatures
open Belenios
open Serializable_builtin_t
open Serializable_j
open Common

let lines_of_file fname =
  let ic = open_in fname in
  let rec loop accu =
    match input_line ic with
    | line -> loop (line :: accu)
    | exception End_of_file -> close_in ic; List.rev accu
  in
  loop []

let string_of_file f =
  lines_of_file f |> String.concat "\n"

let string_of_file_opt filename =
  if Sys.file_exists filename then Some (string_of_file filename) else None

let load_from_file of_string filename =
  if Sys.file_exists filename then (
    Some (lines_of_file filename |> List.rev_map of_string)
  ) else None

let ( / ) = Filename.concat

type verifydiff_error =
  | ElectionMismatch
  | MissingTrustees
  | InvalidTrustees
  | PublicKeyMismatch
  | TrusteesMismatch
  | MissingCredentials
  | InvalidCredential
  | CredentialsMismatch
  | InvalidBallot of string
  | DuplicateBallot
  | BallotSignedByInvalidKey
  | DecreasingBallots
  | CredentialsHaveChanged
  | InvalidRevote of string

exception VerifydiffError of verifydiff_error

let explain_error = function
  | ElectionMismatch -> "election mismatch"
  | MissingTrustees -> "missing trustees"
  | InvalidTrustees -> "invalid trustees"
  | PublicKeyMismatch -> "public key mismatch"
  | TrusteesMismatch -> "trustees mismatch"
  | MissingCredentials -> "missing credentials"
  | InvalidCredential -> "invalid credential"
  | CredentialsMismatch -> "credentials mismatch"
  | InvalidBallot ballot_id -> Printf.sprintf "invalid ballot: %s" ballot_id
  | DuplicateBallot -> "duplicate ballot"
  | BallotSignedByInvalidKey -> "ballot signed by invalid key"
  | DecreasingBallots -> "decreasing ballots"
  | CredentialsHaveChanged -> "credentials have changed"
  | InvalidRevote ballot_id -> Printf.sprintf "invalid revote: %s" ballot_id

let () =
  Printexc.register_printer (function
      | VerifydiffError e -> Some ("verify-diff error: " ^ explain_error e)
      | _ -> None)

let load_trustees dir =
  match string_of_file_opt (dir / "trustees.json") with
  | Some t -> t
  | None -> raise (VerifydiffError MissingTrustees)

let verifydiff dir1 dir2 =
  (* the elections must be the same *)
  let election = string_of_file (dir1 / "election.json") in
  let () =
    let election2 = string_of_file (dir2 / "election.json") in
    if election2 <> election then raise (VerifydiffError ElectionMismatch)
  in
  (* the trustees must be the same *)
  let trustees = load_trustees dir1 in
  let () =
    let trustees2 = load_trustees dir2 in
    if trustees2 <> trustees then raise (VerifydiffError TrusteesMismatch)
  in
  let module ED = Election.Make (struct let raw_election = election end) (Random) () in
  let open ED in
  let trustees = trustees_of_string G.read trustees in
  let module Trustees = (val Trustees.get_by_version election.e_version) in
  let module K = Trustees.MakeCombinator (G) in
  if not (K.check trustees) then
    raise (VerifydiffError InvalidTrustees);
  let y = K.combine_keys trustees in
  (* the public keys must correspond to the public key of election *)
  let () =
    if not G.(public_key =~ y) then
      raise (VerifydiffError PublicKeyMismatch)
  in
  (* load both public_creds.txt and check that their contents is valid *)
  let parse x =
    let cred, w = extract_weight x in
    G.of_string cred, w
  in
  let creds dir =
    match load_from_file parse (dir / "public_creds.txt") with
    | None -> raise (VerifydiffError MissingCredentials)
    | Some creds ->
       if not (List.for_all (fun (x, _) -> G.check x) creds) then
         raise (VerifydiffError InvalidCredential);
       List.fold_left (fun accu (x, w) -> SMap.add (G.to_string x) (ref None, w) accu) SMap.empty creds
  in
  let creds1 = creds dir1 and creds2 = creds dir2 in
  (* both public_creds.txt have the same cardinal *)
  let () =
    if SMap.cardinal creds1 <> SMap.cardinal creds2 then
      raise (VerifydiffError CredentialsMismatch)
  in
  (* compute credentials that have been replaced *)
  let creds_replaced =
    SMap.fold (fun x _ accu ->
        if not (SMap.mem x creds2) then SSet.add x accu else accu
      ) creds1 SSet.empty
  in
  (* fail if credentials have changed *)
  let () =
    if not (SSet.is_empty creds_replaced) then
      raise (VerifydiffError CredentialsHaveChanged)
  in
  (* instantiate CastBallot *)
  let module BboxOps =
    struct
      type user = string
      let get_credential_record cred =
        match SMap.find_opt cred creds1 with
        | None -> None
        | Some (id, cr_weight) -> Some {cr_weight; cr_ballot = !id}
      let get_user_record user =
        match SMap.find_opt user creds1 with
        | None -> None
        | Some (id, _) ->
           match !id with
           | None -> None
           | Some _ -> Some user
    end
  in
  let module CastBallot = E.CastBallot (BboxOps) in
  (* load first ballots.jsons *)
  let nballots1 =
    match load_from_file (fun x -> x) (dir1 / "ballots.jsons") with
    | None -> 0
    | Some ballots ->
       List.iter
         (fun b ->
           let ballot_id = sha256_b64 b in
           let user =
             match get_credential @@ ballot_of_string b with
             | None -> raise (VerifydiffError BallotSignedByInvalidKey)
             | Some x -> G.to_string x
           in
           match CastBallot.cast ~user b with
           | Ok (_, _, None) ->
              (match SMap.find_opt user creds1 with
               | None -> assert false
               | Some (id, _) -> id := Some ballot_id
              )
           | Ok (_, _, Some _) -> raise (VerifydiffError DuplicateBallot)
           | Error _ -> raise (VerifydiffError (InvalidBallot ballot_id))
         ) ballots;
       List.length ballots
  in
  (* load second ballots.jsons *)
  let nballots2 =
    match load_from_file (fun x -> x) (dir2 / "ballots.jsons") with
    | None -> 0
    | Some ballots ->
       let replaced_ballots = ref 0 in
       List.iter
         (fun b ->
           let ballot_id = sha256_b64 b in
           let user =
             match get_credential @@ ballot_of_string b with
             | None -> raise (VerifydiffError BallotSignedByInvalidKey)
             | Some x -> G.to_string x
           in
           match SMap.find_opt user creds1 with
           | None -> raise (VerifydiffError BallotSignedByInvalidKey)
           | Some (id, _) when !id = Some ballot_id -> ()
           | Some (id, _) ->
              match CastBallot.cast ~user b with
              | Ok (_, _, i) ->
                 let () = match i with
                   | None -> ()
                   | Some x -> assert (x <> ballot_id); incr replaced_ballots
                 in
                 id := Some ballot_id
              | Error _ -> raise (VerifydiffError (InvalidRevote ballot_id))
         ) ballots;
       if !replaced_ballots > 0 then
         Printf.eprintf "W: %d ballot(s) have been replaced\n%!" !replaced_ballots;
       List.length ballots
  in
  (* the set of ballots increases *)
  let () =
    if nballots2 < nballots1 then
      raise (VerifydiffError DecreasingBallots)
  in
  let () =
    let n = nballots2 - nballots1 in
    if n > 0 then Printf.eprintf "I: %d new ballot(s)\n%!" n
  in
  Printf.eprintf "I: all tests passed!\n%!"
