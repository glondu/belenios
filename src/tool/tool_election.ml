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
open Belenios

module type PARAMS = sig
  val file : string
  val salts_file : string option
end

module type S = sig
  type 'a m

  val vote : string option -> int array array -> string m
  val decrypt : int -> string -> (string * string) m
  val tdecrypt : int -> string -> string -> (string * string) m
  val compute_result : unit -> string m
  val verify_ballot : string -> unit m
  val verify : ?skip_ballot_check:bool -> unit -> unit m
  val shuffle_ciphertexts : int -> (string * string) m
  val checksums : unit -> string
  val compute_voters : (string * string) list -> string list
  val compute_ballot_summary : unit -> string
  val compute_encrypted_tally : unit -> string * string
end

module Make (P : PARAMS) () = struct
  module Getters = Tool_election_data.MakeGetters (P)
  module R = Random

  module Election =
    B.Election.Make
      (struct
        let raw_election = Getters.raw_election
      end)
      (R)
      ()

  include Election
  include Tool_election_data.Make (Getters) (Election)

  let print_msg = prerr_endline

  module Trustees = (val B.Trustees.get_by_version version)
  module P = Trustees.MakePKI (G) (R)
  module C = Trustees.MakeChannels (G) (R) (P)
  module K = Trustees.MakeCombinator (G)

  (* Check trustee keys, if present *)
  let () =
    match trustees with
    | Some trustees ->
        assert (K.check trustees);
        let y' = K.combine_keys trustees in
        assert (G.(public_key =~ y'))
    | None -> failwith "missing trustees"

  let vote privcred choice =
    let sk =
      match privcred with
      | None -> failwith "missing private credential"
      | Some cred -> (
          match Cred.derive cred with
          | Ok x -> x
          | Error _ -> failwith "invalid private credential")
    in
    let b = E.create_ballot ~sk choice in
    assert (E.check_ballot b);
    write_ballot -- b

  let decrypt owned_owner privkey =
    let sk = sread G.Zq.of_string ++ privkey in
    let pk = G.(g **~ sk) in
    if Array.for_all (fun x -> not G.(x =~ pk)) (Lazy.force pks) then
      failwith "your key is not present in trustees";
    (match Lazy.force shuffles_hash with
    | None | Some [] ->
        if Election.has_nh_questions then
          failwith
            "the election has non-homomorphic questions and no shuffles were \
             found"
    | Some shuffles ->
        shuffles
        |> List.iter (fun s ->
               Printf.ksprintf print_msg "I: shuffle %s has been applied" s));
    if Election.has_nh_questions then
      print_msg
        "I: you should check that your shuffle appears in the list of applied \
         shuffles";
    let tally, _ = Lazy.force encrypted_tally in
    let factor = E.compute_factor tally sk in
    assert (E.check_factor tally pk factor);
    let pd =
      string_of_partial_decryption (swrite G.to_string) (swrite G.Zq.to_string)
        factor
    in
    let opd = { owned_owner; owned_payload = Hash.hash_string pd } in
    (pd, string_of_owned write_hash opd)

  let tdecrypt owned_owner key pdk =
    let sk = P.derive_sk key and dk = P.derive_dk key in
    let vk = G.(g **~ sk) in
    let pdk = C.recv dk vk (encrypted_msg_of_string (sread G.of_string) pdk) in
    let pdk =
      (partial_decryption_key_of_string (sread G.Zq.of_string) pdk)
        .pdk_decryption_key
    in
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
    let pd =
      string_of_partial_decryption (swrite G.to_string) (swrite G.Zq.to_string)
        factor
    in
    let opd = { owned_owner; owned_payload = Hash.hash_string pd } in
    (pd, string_of_owned write_hash opd)

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
      List.map
        (fill
           (partial_decryption_of_string (sread G.of_string)
              (sread G.Zq.of_string)))
        pds
    in
    let tally, sized = Lazy.force encrypted_tally in
    let sized = { sized with sized_encrypted_tally = tally } in
    match trustees with
    | Some trustees -> (
        match E.compute_result sized factors trustees with
        | Ok result -> string_of_election_result write_result result
        | Error e -> failwith (B.Trustees.string_of_combination_error e))
    | None -> failwith "missing trustees"

  let verify_ballot raw_ballot =
    let ballot_box =
      Lazy.force unverified_ballots
      |> List.map (fun (h, _, _, _) -> Hash.to_b64 h)
      |> SSet.of_list
    in
    match pre_cast ballot_box raw_ballot with
    | Error e ->
        Printf.ksprintf failwith "error: %s in ballot %s"
          (string_of_cast_error e) raw_ballot
    | Ok _ -> print_msg "I: ballot is valid"

  let shuffles_check shuffles =
    let rtally, _ = Lazy.force raw_encrypted_tally in
    let cc = E.extract_nh_ciphertexts rtally in
    let rec loop i cc ss =
      match ss with
      | s :: ss ->
          if E.check_shuffle cc s then loop (i + 1) s.shuffle_ciphertexts ss
          else Printf.ksprintf failwith "shuffle #%d failed tests" i
      | [] -> true
    in
    loop 0 cc shuffles

  let verify ?(skip_ballot_check = false) () =
    let () = fsck () in
    (match trustees with
    | Some trustees ->
        assert (K.check trustees);
        assert (G.(public_key =~ K.combine_keys trustees))
    | None -> failwith "missing trustees");
    let () =
      match Lazy.force raw_ballots with
      | Some _ -> (
          match skip_ballot_check with
          | false -> ignore (Lazy.force verified_ballots)
          | true -> ignore (Lazy.force unverified_ballots))
      | None -> print_msg "I: no ballots to check"
    in
    let () =
      match Lazy.force shuffles with
      | Some shuffles ->
          let b = shuffles_check shuffles in
          assert b
      | None -> print_msg "I: no shuffles to check"
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
              (fill
                 (partial_decryption_of_string (sread G.of_string)
                    (sread G.Zq.of_string)))
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
    let shuffle_s =
      string_of_shuffle (swrite G.to_string) (swrite G.Zq.to_string) shuffle
    in
    let owned = { owned_owner; owned_payload = Hash.hash_string shuffle_s } in
    (shuffle_s, string_of_owned write_hash owned)

  let checksums () =
    let election = election_hash in
    let shuffles =
      let& x = Lazy.force raw_shuffles in
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
    let map =
      List.fold_left
        (fun accu (id, cred) ->
          match Cred.derive cred with
          | Error _ -> Printf.ksprintf failwith "invalid credential %s" cred
          | Ok x -> SMap.add G.(g **~ x |> to_string) id accu)
        SMap.empty privcreds
    in
    let ballots = Lazy.force verified_ballots in
    List.fold_left
      (fun accu (h, cred, _, _) ->
        match SMap.find_opt cred map with
        | None ->
            Printf.ksprintf failwith "Unknown public key in ballot %s"
              (Hash.to_b64 h)
        | Some id -> id :: accu)
      [] ballots

  let compute_ballot_summary () =
    let has_weights =
      match Lazy.force public_creds_weights with
      | None -> false
      | Some (b, _) -> b
    in
    Lazy.force verified_ballots
    |> List.rev_map (fun (bs_hash, _, w, _) ->
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
