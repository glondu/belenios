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

open Belenios_platform
open Belenios_core
open Belenios
open Platform
open Serializable_builtin_t
open Serializable_j
open Signatures
open Common

module type PARAMS = sig
  val raw_election : string
  val get_trustees : unit -> string option
  val get_public_creds : unit -> string list option
  val get_ballots : unit -> string list option
  val get_shuffles : unit -> string list option
  val get_result : unit -> string option
  val print_msg : string -> unit
end

module type S = sig
  type 'a m
  val vote : string option -> int array array -> string m
  val decrypt : string -> string m
  val tdecrypt : string -> string -> string m
  val validate : string list -> string m
  val verify : unit -> unit m
  val shuffle_ciphertexts : unit -> string m
  val checksums : unit -> string
  val compute_voters : string list -> string list
end

module PTrustees = Trustees

module Make (P : PARAMS) (M : RANDOM) () = struct

  include P
  include Election.Make (P) (M) ()
  module Trustees = (val Trustees.get_by_version election.e_version)
  let ( let* ) = M.bind

  module P = Trustees.MakePKI (G) (M)
  module C = Trustees.MakeChannels (G) (M) (P)

  module K = Trustees.MakeCombinator (G)

  (* Load and check trustee keys, if present *)

  let trustees_as_string =
    get_trustees ()

  let trustees =
    Option.map (trustees_of_string G.read) trustees_as_string

  let public_keys_with_pok =
    trustees
    |> Option.map
         (fun x ->
           x
           |> List.map
                (function
                 | `Single x -> [x]
                 | `Pedersen t -> Array.to_list t.t_verification_keys
                )
           |> List.flatten
           |> Array.of_list
         )

  let () =
    match trustees with
    | Some trustees ->
      assert (K.check trustees);
      let y' = K.combine_keys trustees in
      assert G.(public_key =~ y')
    | None -> failwith "missing trustees"

  let public_keys =
    Option.map (
      Array.map (fun pk -> pk.trustee_public_key)
    ) public_keys_with_pok

  let pks = lazy (match public_keys with
                  | Some pks -> pks
                  | None -> failwith "missing public keys")

  (* Load ballots, if present *)

  module PPC = Credential.MakeParsePublicCredential (G)

  let public_creds =
    lazy (
        get_public_creds ()
        |> Option.map
             (List.fold_left
                (fun accu x ->
                  match PPC.parse_public_credential x with
                  | Some (w, y) -> SMap.add (G.to_string y) (w, ref None) accu
                  | None -> Printf.ksprintf failwith "%s is not a valid public credential" x;
                ) SMap.empty
             )
      )

  let rawballots = lazy (get_ballots ())

  let string_of_cast_error = function
    | `SerializationError e -> Printf.sprintf "ill-formed ballot: %s" (Printexc.to_string e)
    | `NonCanonical -> "ballot not in canonical form"
    | `InvalidBallot -> "invalid ballot"
    | `InvalidCredential -> "invalid credential"
    | `WrongCredential -> "wrong credential"
    | `WrongWeight -> "wrong weight"
    | `UsedCredential -> "used credential"
    | `RevoteNotAllowed -> "revote not allowed"

  let cast rawballot =
    match Lazy.force public_creds with
    | None -> failwith "missing public credentials"
    | Some creds ->
       let ballot_id = sha256_b64 rawballot in
       let module X =
         struct
           type user
           let get_credential_record c =
             match SMap.find_opt c creds with
             | None -> M.return None
             | Some (cr_weight, old) -> M.return (Some {cr_ballot = !old; cr_weight})
           let get_user_record _ =
             M.return None
         end
       in
       let module B = E.CastBallot (X) in
       let* x = B.cast rawballot in
       match x with
       | Error e ->
          Printf.ksprintf failwith "error while casting ballot %s: %s"
            ballot_id (string_of_cast_error e)
       | Ok (credential, ballot, old) ->
          match SMap.find_opt credential creds with
          | None ->
             Printf.ksprintf failwith "invalid credential for ballot %s"
               ballot_id
          | Some (w, used) ->
             assert (!used = old);
             let () =
               match old with
               | None -> used := Some ballot_id
               | Some _ ->
                  Printf.ksprintf failwith
                    "credential of ballot %s used multiple times"
                    ballot_id
             in
             M.return (w, ballot)

  let ballots =
    let rec loop accu = function
      | [] -> M.return (Array.of_list accu)
      | b :: bs ->
         let* x = cast b in
         loop (x :: accu) bs
    in
    lazy (Lazy.force rawballots |> Option.map (loop []))

  let raw_encrypted_tally =
    lazy (
        match Lazy.force ballots with
        | None -> M.return (E.process_ballots [||], Weight.zero)
        | Some x ->
           let* ballots = x in
           let total_weight =
             let open Weight in
             Array.fold_left (fun accu (w, _) -> accu + w) zero ballots
           in
           M.return (E.process_ballots ballots, total_weight)
      )

  let result_as_string = lazy (get_result ())

  let result =
    lazy (
        Lazy.force result_as_string
        |> Option.map (election_result_of_string G.read read_result)
      )

  let shuffles_as_text =
    lazy (
        match Lazy.force result, get_shuffles () with
        | Some _, Some _ -> failwith "both shuffles.jsons and result.json exist"
        | None, None -> None
        | Some result, None ->
           result.shuffles
           |> Option.map (List.map (string_of_shuffle G.write))
        | None, (Some _ as s) -> s
      )

  let shuffles =
    lazy
      (
        Lazy.force shuffles_as_text
        |> Option.map (List.map (shuffle_of_string G.read))
      )

  let shuffles_hash =
    lazy
      (
        Lazy.force shuffles_as_text
        |> Option.map (List.map sha256_b64)
      )

  let shuffles_check =
    lazy (
        let* rtally, _ = Lazy.force raw_encrypted_tally in
        let cc = E.extract_nh_ciphertexts rtally in
        let rec loop i cc ss =
          match ss with
          | s :: ss ->
             if E.check_shuffle cc s then
               loop (i+1) s.shuffle_ciphertexts ss
             else
               Printf.ksprintf failwith "shuffle #%d failed tests" i
          | [] -> M.return true
        in
        match Lazy.force shuffles with
        | Some ss -> loop 0 cc ss
        | None -> M.return true
      )

  let encrypted_tally =
    lazy (
        let* raw_encrypted_tally, ntally = Lazy.force raw_encrypted_tally in
        match Option.map List.rev (Lazy.force shuffles) with
        | Some (s :: _) -> M.return (E.merge_nh_ciphertexts s.shuffle_ciphertexts raw_encrypted_tally, ntally)
        | _ -> M.return (raw_encrypted_tally, ntally)
      )

  let vote privcred ballot =
    let sk =
      match privcred with
      | None -> failwith "missing private credential"
      | Some cred ->
         let module CD = Credential.MakeDerive (G) in
         CD.derive election.e_uuid cred
    in
    let* b = E.create_ballot ~sk ballot in
    assert (E.check_ballot b);
    M.return (string_of_ballot b)

  let decrypt privkey =
    let sk = number_of_string privkey in
    let pk = G.(g **~ sk) in
    if Array.forall (fun x -> not G.(x =~ pk)) (Lazy.force pks) then (
      print_msg "W: your key is not present in trustees.jsons";
    );
    (match Lazy.force shuffles_hash with
     | None | Some [] ->
        if Election.has_nh_questions election then
          failwith "the election has non-homomorphic questions and no shuffles were found";
     | Some shuffles ->
        shuffles
        |> List.iter
             (fun s ->
               Printf.ksprintf print_msg "I: shuffle %s has been applied" s)
    );
    if Election.has_nh_questions election then
      print_msg "W: you should check that your shuffle appears in the list of applied shuffles";
    let* tally, _ = Lazy.force encrypted_tally in
    let* factor = E.compute_factor tally sk in
    assert (E.check_factor tally pk factor);
    M.return (string_of_partial_decryption G.write factor)

  let tdecrypt key pdk =
    let sk = P.derive_sk key and dk = P.derive_dk key in
    let vk = G.(g **~ sk) in
    let pdk = C.recv dk vk (encrypted_msg_of_string G.read pdk) in
    let pdk = (partial_decryption_key_of_string pdk).pdk_decryption_key in
    let pvk = G.(g **~ pdk) in
    (match trustees with
     | None -> print_msg "W: trustees are missing"
     | Some ts ->
        if not @@ List.exists
                    (function
                     | `Single _ -> false
                     | `Pedersen t ->
                        Array.exists
                          (fun x -> G.(x.trustee_public_key =~ pvk))
                          t.t_verification_keys
                    ) ts
        then print_msg "W: your key is not present in threshold parameters"
    );
    let* tally, _ = Lazy.force encrypted_tally in
    let* factor = E.compute_factor tally pdk in
    assert (E.check_factor tally pvk factor);
    M.return (string_of_partial_decryption G.write factor)

  let validate factors =
    let factors = List.map (partial_decryption_of_string G.read) factors in
    let* tally, nballots = Lazy.force encrypted_tally in
    let shuffles = Lazy.force shuffles in
    match trustees with
    | Some trustees ->
       (match E.compute_result ?shuffles nballots tally factors trustees with
        | Ok result ->
           assert (E.check_result trustees result);
           M.return (string_of_election_result G.write write_result result)
        | Error e -> failwith (PTrustees.string_of_combination_error e)
       )
    | None -> failwith "missing trustees"

  let verify () =
    (match trustees with
     | Some trustees ->
        assert (K.check trustees);
        assert G.(public_key =~ K.combine_keys trustees)
     | None -> failwith "missing trustees"
    );
    let* () =
      match Lazy.force ballots with
      | Some _ -> let* b = Lazy.force shuffles_check in assert b; M.return ()
      | None -> print_msg "W: no ballots to check"; M.return ()
    in
    let* () =
      match Lazy.force result, trustees with
      | Some result, Some trustees ->
         let* et = Lazy.force encrypted_tally in
         assert (fst et = result.encrypted_tally);
         assert (E.check_result trustees result);
         M.return ()
      | Some _, None -> failwith "missing trustees"
      | None, _ -> print_msg "W: no result to check"; M.return ()
    in
    print_msg "I: all checks passed";
    M.return ()

  let shuffle_ciphertexts () =
    let* cc, _ = Lazy.force encrypted_tally in
    let cc = E.extract_nh_ciphertexts cc in
    let* s = E.shuffle_ciphertexts cc in
    M.return (string_of_shuffle G.write s)

  let checksums () =
    let election = raw_election in
    let result_or_shuffles =
      match Lazy.force result_as_string with
      | Some r -> `Result r
      | None ->
         match Lazy.force shuffles_as_text with
         | None -> `Nothing
         | Some shuffles -> `Shuffles (shuffles, None)
    in
    let trustees =
      match trustees_as_string with
      | None -> failwith "missing trustees"
      | Some x -> x
    in
    let public_credentials =
      match Lazy.force public_creds with
      | None -> failwith "missing public credentials"
      | Some public_creds ->
         public_creds
         |> SMap.bindings
         |> List.map fst
         |> List.map (fun x -> x ^ "\n")
         |> String.concat ""
    in
    Election.compute_checksums ~election result_or_shuffles ~trustees ~public_credentials
    |> string_of_election_checksums

  let split line =
    match String.index_opt line ' ' with
    | None -> Printf.ksprintf failwith "Invalid private credential line (%S)" line
    | Some i -> String.sub line 0 i, String.sub line (i + 1) (String.length line - i - 1)

  let compute_voters privcreds =
    let module D = Credential.MakeDerive (G) in
    let map =
      List.fold_left
        (fun accu line ->
          let id, cred = split line in
          SMap.add G.(g **~ D.derive election.e_uuid cred |> to_string) id accu
        ) SMap.empty privcreds
    in
    match Lazy.force public_creds with
    | None -> []
    | Some creds ->
       SMap.fold
         (fun cred (_, ballot) accu ->
           match !ballot with
           | None -> accu
           | Some h ->
              match SMap.find_opt cred map with
              | None -> Printf.ksprintf failwith "Unknown public key in ballot %s" h
              | Some id -> id :: accu
         ) creds []

end
