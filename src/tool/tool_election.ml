(**************************************************************************)
(*                                BELENIOS                                *)
(*                                                                        *)
(*  Copyright © 2012-2022 Inria                                           *)
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

module B = Belenios
open Belenios_core.Serializable_j
open Belenios_core.Signatures
open Belenios_core.Common
open Common

module type FILES = sig
  val fsck : unit -> unit
  val setup_data : setup_data
  val raw_election : string
  val get_trustees : unit -> string option
  val get_public_creds : unit -> string list option
  val get_ballots : unit -> string list option
  val get_shuffles : unit -> (hash * hash owned * string) list option
  val get_pds : unit -> (hash * hash owned * string) list option
  val get_result : unit -> string option
  val print_msg : string -> unit
end

module type PARAMS = sig
  val file : string
end

module type S = sig
  type 'a m

  val vote : string option -> int array array -> string m
  val decrypt : int -> string -> (string * string) m
  val tdecrypt : int -> string -> string -> (string * string) m
  val compute_result : unit -> string m
  val verify : unit -> unit m
  val shuffle_ciphertexts : int -> (string * string) m
  val checksums : unit -> string
  val compute_voters : (string * string) list -> string list
  val compute_ballot_summary : unit -> string
  val compute_encrypted_tally : unit -> string * string
end

module M = Random

module MakeGetters (X : PARAMS) : FILES = struct
  let index = Tool_events.get_index ~file:X.file
  let roots = Tool_events.get_roots index
  let get_data x = Tool_events.get_data index x
  let fsck () = Tool_events.fsck index

  let setup_data =
    match roots.roots_setup_data with
    | None -> failcmd "setup data are missing"
    | Some x -> (
        match get_data x with
        | None -> failcmd "could not get setup data"
        | Some x -> setup_data_of_string x)

  let raw_election =
    match get_data setup_data.setup_election with
    | None -> failcmd "could not get election"
    | Some x -> x

  let get_public_creds () =
    get_data setup_data.setup_credentials
    |> Option.map public_credentials_of_string

  let get_trustees () = get_data setup_data.setup_trustees

  let get_ballots () =
    match roots.roots_last_ballot_event with
    | None -> Some []
    | Some x ->
        Some
          (Tool_events.fold_on_event_payloads index `Ballot x
             (fun x accu -> x :: accu)
             [])

  let get_shuffles () =
    let& x = roots.roots_last_shuffle_event in
    Tool_events.fold_on_event_payload_hashes index `Shuffle x
      (fun x accu ->
        match get_data x with
        | None -> failwith "could not get shuffle"
        | Some y -> (
            let owned = owned_of_string read_hash y in
            match get_data owned.owned_payload with
            | None -> failwith "could not get shuffle payload"
            | Some z -> (x, owned, z) :: accu))
      []
    |> fun x -> Some x

  let get_pds () =
    let& x = roots.roots_last_pd_event in
    Tool_events.fold_on_event_payload_hashes index `PartialDecryption x
      (fun x accu ->
        match get_data x with
        | None -> failwith "could not get partial decryption"
        | Some y -> (
            let owned = owned_of_string read_hash y in
            match get_data owned.owned_payload with
            | None -> failwith "could not get partial decryption payload"
            | Some z -> (x, owned, z) :: accu))
      []
    |> fun x -> Some x

  let get_result () =
    let& x = roots.roots_result in
    get_data x

  let print_msg = prerr_endline
end

module Make (P : PARAMS) () = struct
  include MakeGetters (P)

  include
    B.Election.Make
      (struct
        let raw_election = raw_election
      end)
      (M)
      ()

  module Trustees = (val B.Trustees.get_by_version election.e_version)
  module P = Trustees.MakePKI (G) (M)
  module C = Trustees.MakeChannels (G) (M) (P)
  module K = Trustees.MakeCombinator (G)

  (* Load and check trustee keys, if present *)

  let trustees_as_string = get_trustees ()

  let trustees =
    Option.map (trustees_of_string (sread G.of_string)) trustees_as_string

  let public_keys_with_pok =
    trustees
    |> Option.map (fun x ->
           x
           |> List.map (function
                | `Single x -> [ x ]
                | `Pedersen t -> Array.to_list t.t_verification_keys)
           |> List.flatten |> Array.of_list)

  let () =
    match trustees with
    | Some trustees ->
        assert (K.check trustees);
        let y' = K.combine_keys trustees in
        assert (G.(public_key =~ y'))
    | None -> failwith "missing trustees"

  let public_keys =
    Option.map
      (Array.map (fun pk -> pk.trustee_public_key))
      public_keys_with_pok

  let pks =
    lazy
      (match public_keys with
      | Some pks -> pks
      | None -> failwith "missing public keys")

  (* Load ballots, if present *)

  module PPC = Belenios_core.Credential.MakeParsePublicCredential (G)

  let raw_public_creds = lazy (get_public_creds ())

  let public_creds_weights =
    lazy
      (Lazy.force raw_public_creds
      |> Option.map
           (List.fold_left
              (fun (has_weights, accu) x ->
                let has_weights =
                  has_weights || String.index_opt x ',' <> None
                in
                match PPC.parse_public_credential x with
                | Some (w, y) ->
                    let y = G.to_string y in
                    if SMap.mem y accu then
                      Printf.ksprintf failwith "duplicate credential: %s" y
                    else (has_weights, SMap.add y (w, ref None) accu)
                | None ->
                    Printf.ksprintf failwith
                      "%s is not a valid public credential" x)
              (false, SMap.empty)))

  let public_creds = lazy (Lazy.force public_creds_weights |> Option.map snd)
  let rawballots = lazy (get_ballots ())

  let string_of_cast_error = function
    | `SerializationError e ->
        Printf.sprintf "ill-formed ballot: %s" (Printexc.to_string e)
    | `NonCanonical -> "ballot not in canonical form"
    | `InvalidBallot -> "invalid ballot"
    | `InvalidCredential -> "invalid credential"
    | `WrongCredential -> "wrong credential"
    | `WrongWeight -> "wrong weight"
    | `UsedCredential -> "used credential"
    | `RevoteNotAllowed -> "revote not allowed"
    | `DuplicateBallot -> "duplicate ballot"
    | `ExpiredBallot -> "expired ballot"
    | `WrongUsername -> "wrong username"

  let pre_cast ballot_box rawballot =
    let hash = Hash.hash_string rawballot in
    let ballot_id = Hash.to_b64 hash in
    let@ creds cont =
      match Lazy.force public_creds with
      | None -> failwith "missing public credentials"
      | Some creds -> cont creds
    in
    let is_duplicate = SSet.mem ballot_id ballot_box in
    let@ rc cont =
      match (is_duplicate, E.check_rawballot rawballot) with
      | true, _ -> Error `DuplicateBallot
      | _, (Error _ as e) -> e
      | _, Ok rc -> cont rc
    in
    match SMap.find_opt rc.rc_credential creds with
    | None -> Error `InvalidCredential
    | Some (w, ballot_for_creds) when rc.rc_check () ->
        ballot_for_creds := Some ballot_id;
        Ok (hash, (rc.rc_credential, w, rawballot))
    | Some _ -> Error `InvalidBallot

  let final_ballots =
    lazy
      (let ballot_box =
         let rec cast_all accu seen = function
           | [] -> accu
           | b :: bs -> (
               match pre_cast seen b with
               | Error e ->
                   Printf.ksprintf failwith "error while casting ballot %s: %s"
                     (sha256_b64 b) (string_of_cast_error e)
               | Ok (hash, x) ->
                   let ballot_id = Hash.to_b64 hash in
                   cast_all ((hash, x) :: accu) (SSet.add ballot_id seen) bs)
         in
         match Lazy.force rawballots with
         | None -> []
         | Some bs -> cast_all [] SSet.empty bs
       in
       List.fold_left
         (fun ((seen, bs) as accu) (h, (credential, w, b)) ->
           if SSet.mem credential seen then accu
           else (SSet.add credential seen, (h, w, b) :: bs))
         (SSet.empty, []) ballot_box
       |> snd)

  let raw_encrypted_tally =
    lazy
      (let ballots =
         Lazy.force final_ballots
         |> List.rev_map (fun (_, w, b) -> (w, ballot_of_string b))
       in
       let sized_total_weight =
         let open Weight in
         List.fold_left (fun accu (w, _) -> accu + w) zero ballots
       in
       let encrypted_tally = E.process_ballots ballots in
       ( encrypted_tally,
         {
           sized_num_tallied = List.length ballots;
           sized_total_weight;
           sized_encrypted_tally =
             Hash.hash_string
               (string_of_encrypted_tally (swrite G.to_string) encrypted_tally);
         } ))

  let result_as_string = lazy (get_result ())

  let result =
    lazy
      (Lazy.force result_as_string
      |> Option.map (election_result_of_string read_result))

  let lazy_shuffles = lazy (get_shuffles ())

  let shuffles_as_text =
    lazy (Lazy.force lazy_shuffles |> Option.map (List.map (fun (_, _, x) -> x)))

  let shuffles =
    lazy
      (Lazy.force shuffles_as_text
      |> Option.map (List.map (shuffle_of_string (sread G.of_string))))

  let shuffles_hash =
    lazy (Lazy.force shuffles_as_text |> Option.map (List.map sha256_b64))

  let shuffles_check =
    lazy
      (let rtally, _ = Lazy.force raw_encrypted_tally in
       let cc = E.extract_nh_ciphertexts rtally in
       let rec loop i cc ss =
         match ss with
         | s :: ss ->
             if E.check_shuffle cc s then loop (i + 1) s.shuffle_ciphertexts ss
             else Printf.ksprintf failwith "shuffle #%d failed tests" i
         | [] -> true
       in
       match Lazy.force shuffles with Some ss -> loop 0 cc ss | None -> true)

  let encrypted_tally =
    lazy
      (let raw_encrypted_tally, ntally = Lazy.force raw_encrypted_tally in
       match Option.map List.rev (Lazy.force shuffles) with
       | Some (s :: _) ->
           ( E.merge_nh_ciphertexts s.shuffle_ciphertexts raw_encrypted_tally,
             ntally )
       | _ -> (raw_encrypted_tally, ntally))

  let vote privcred choice =
    let sk =
      match privcred with
      | None -> failwith "missing private credential"
      | Some cred ->
          let module CD = Belenios_core.Credential.MakeDerive (G) in
          CD.derive election.e_uuid cred
    in
    let b = E.create_ballot ~sk choice in
    assert (E.check_ballot b);
    string_of_ballot b

  let decrypt owned_owner privkey =
    let sk = number_of_string privkey in
    let pk = G.(g **~ sk) in
    if Array.for_all (fun x -> not G.(x =~ pk)) (Lazy.force pks) then
      failwith "your key is not present in trustees";
    (match Lazy.force shuffles_hash with
    | None | Some [] ->
        if B.Election.has_nh_questions election then
          failwith
            "the election has non-homomorphic questions and no shuffles were \
             found"
    | Some shuffles ->
        shuffles
        |> List.iter (fun s ->
               Printf.ksprintf print_msg "I: shuffle %s has been applied" s));
    if B.Election.has_nh_questions election then
      print_msg
        "I: you should check that your shuffle appears in the list of applied \
         shuffles";
    let tally, _ = Lazy.force encrypted_tally in
    let factor = E.compute_factor tally sk in
    assert (E.check_factor tally pk factor);
    let pd = string_of_partial_decryption (swrite G.to_string) factor in
    let opd = { owned_owner; owned_payload = Hash.hash_string pd } in
    (pd, string_of_owned write_hash opd)

  let tdecrypt owned_owner key pdk =
    let sk = P.derive_sk key and dk = P.derive_dk key in
    let vk = G.(g **~ sk) in
    let pdk = C.recv dk vk (encrypted_msg_of_string (sread G.of_string) pdk) in
    let pdk = (partial_decryption_key_of_string pdk).pdk_decryption_key in
    let pvk = G.(g **~ pdk) in
    (match trustees with
    | None -> failwith "trustees are missing"
    | Some ts ->
        if
          not
          @@ List.exists
               (function
                 | `Single _ -> false
                 | `Pedersen t ->
                     Array.exists
                       (fun x -> G.(x.trustee_public_key =~ pvk))
                       t.t_verification_keys)
               ts
        then failwith "your key is not present in threshold parameters");
    let tally, _ = Lazy.force encrypted_tally in
    let factor = E.compute_factor tally pdk in
    assert (E.check_factor tally pvk factor);
    let pd = string_of_partial_decryption (swrite G.to_string) factor in
    let opd = { owned_owner; owned_payload = Hash.hash_string pd } in
    (pd, string_of_owned write_hash opd)

  let pds = lazy (get_pds ())

  let compute_result () =
    let pds =
      match Lazy.force pds with
      | None -> failwith "missing partial decryptions"
      | Some x -> x
    in
    let fill of_string (_, owned, x) =
      { owned with owned_payload = of_string x }
    in
    let factors =
      List.map (fill (partial_decryption_of_string (sread G.of_string))) pds
    in
    let tally, sized = Lazy.force encrypted_tally in
    let sized = { sized with sized_encrypted_tally = tally } in
    match trustees with
    | Some trustees -> (
        match E.compute_result sized factors trustees with
        | Ok result -> string_of_election_result write_result result
        | Error e -> failwith (B.Trustees.string_of_combination_error e))
    | None -> failwith "missing trustees"

  let verify () =
    let () = fsck () in
    (match trustees with
    | Some trustees ->
        assert (K.check trustees);
        assert (G.(public_key =~ K.combine_keys trustees))
    | None -> failwith "missing trustees");
    let () =
      match Lazy.force rawballots with
      | Some _ ->
          let b = Lazy.force shuffles_check in
          assert b
      | None -> print_msg "I: no ballots to check"
    in
    let () =
      match (Lazy.force result, trustees, Lazy.force pds) with
      | None, _, _ -> print_msg "I: no result to check"
      | _, None, _ -> failwith "missing trustees"
      | _, _, None -> failwith "no partial decryptions"
      | Some result, Some trustees, Some pds ->
          let fill of_string (_, owned, x) =
            { owned with owned_payload = of_string x }
          in
          let factors =
            List.map
              (fill (partial_decryption_of_string (sread G.of_string)))
              pds
          in
          let tally, sized = Lazy.force encrypted_tally in
          let sized = { sized with sized_encrypted_tally = tally } in
          if not (E.check_result sized factors trustees result) then
            failwith "check_result failed"
    in
    print_msg "I: all checks passed"

  let shuffle_ciphertexts owned_owner =
    let cc, _ = Lazy.force encrypted_tally in
    let cc = E.extract_nh_ciphertexts cc in
    let shuffle = E.shuffle_ciphertexts cc in
    let shuffle_s = string_of_shuffle (swrite G.to_string) shuffle in
    let owned = { owned_owner; owned_payload = Hash.hash_string shuffle_s } in
    (shuffle_s, string_of_owned write_hash owned)

  let checksums () =
    let election = setup_data.setup_election in
    let shuffles =
      let& x = Lazy.force lazy_shuffles in
      Some (List.map (fun (_, x, _) -> x) x)
    in
    let encrypted_tally =
      let _, x = Lazy.force encrypted_tally in
      Some x.sized_encrypted_tally
    in
    let trustees =
      match trustees_as_string with
      | None -> failwith "missing trustees"
      | Some x -> x
    in
    let public_credentials =
      match Lazy.force raw_public_creds with
      | None -> failwith "missing credentials"
      | Some x -> x
    in
    B.Election.compute_checksums ~election ~shuffles ~encrypted_tally ~trustees
      ~public_credentials
    |> string_of_election_checksums

  let compute_voters privcreds =
    let module D = Belenios_core.Credential.MakeDerive (G) in
    let map =
      List.fold_left
        (fun accu (id, cred) ->
          SMap.add G.(g **~ D.derive election.e_uuid cred |> to_string) id accu)
        SMap.empty privcreds
    in
    ignore (Lazy.force final_ballots);
    match Lazy.force public_creds with
    | None -> []
    | Some creds ->
        SMap.fold
          (fun cred (_, ballot) accu ->
            match !ballot with
            | None -> accu
            | Some h -> (
                match SMap.find_opt cred map with
                | None ->
                    Printf.ksprintf failwith "Unknown public key in ballot %s" h
                | Some id -> id :: accu))
          creds []

  let compute_ballot_summary () =
    let has_weights =
      match Lazy.force public_creds_weights with
      | None -> false
      | Some (b, _) -> b
    in
    Lazy.force final_ballots
    |> List.rev_map (fun (bs_hash, w, _) ->
           let bs_weight =
             if has_weights then Some w
             else (
               assert (Weight.is_int w 1);
               None)
           in
           { bs_hash; bs_weight })
    |> string_of_ballot_summary

  let compute_encrypted_tally () =
    let et, sized = Lazy.force raw_encrypted_tally in
    ( string_of_encrypted_tally (swrite G.to_string) et,
      string_of_sized_encrypted_tally write_hash sized )
end
