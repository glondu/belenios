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

open Lwt.Syntax
open Belenios

module type PARAMS = sig
  val file : string
end

module type S = sig
  val vote : string option -> int Shape.t array -> string Lwt.t
  val decrypt : int -> string -> (string * string) Lwt.t
  val tdecrypt : int -> string -> string -> (string * string) Lwt.t
  val compute_result : unit -> string Lwt.t
  val verify_ballot : string -> unit Lwt.t
  val verify : ?skip_ballot_check:bool -> unit -> unit Lwt.t
  val shuffle_ciphertexts : int -> (string * string) Lwt.t
  val checksums : unit -> string Lwt.t
  val compute_voters : (string * string) list -> string list Lwt.t
  val compute_ballot_summary : unit -> string Lwt.t
  val compute_encrypted_tally : unit -> (string * string) Lwt.t
end

let make file =
  let module Getters = Tool_election_data.MakeGetters (struct
    let file = file
  end) in
  let module R = Random in
  let* raw_election = Getters.raw_election in
  let module Election = (val Election.of_string (module R) raw_election) in
  let open Election in
  let open Tool_election_data.Make (Getters) (Election) in
  let module Trustees = (val Belenios.Trustees.get_by_version version) in
  let module P = Trustees.MakePKI (G) (R) in
  let module C = Trustees.MakeChannels (G) (R) (P) in
  let module K = Trustees.MakeCombinator (G) in
  (* Check trustee keys, if present *)
  let* () =
    let* trustees = trustees in
    match trustees with
    | Some trustees ->
        assert (K.check trustees);
        let y' = K.combine_keys trustees in
        assert (G.(public_key =~ y'));
        Lwt.return_unit
    | None -> failwith "missing trustees"
  in
  let module X = struct
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
      Lwt.return (write_ballot -- b)

    let decrypt owned_owner privkey =
      let sk = sread G.Zq.of_string ++ privkey in
      let pk = G.(g **~ sk) in
      let* pks = Lazy.force pks in
      if Array.for_all (fun x -> not G.(x =~ pk)) pks then
        failwith "your key is not present in trustees";
      let* () =
        let* x = Lazy.force shuffles_hash in
        match x with
        | None | Some [] ->
            if Election.has_nh_questions then
              failwith
                "the election has non-homomorphic questions and no shuffles \
                 were found"
            else Lwt.return_unit
        | Some shuffles ->
            shuffles
            |> Lwt_list.iter_s (fun s ->
                   Lwt_io.eprintlf "I: shuffle %s has been applied" s)
      in
      let* () =
        if Election.has_nh_questions then
          Lwt_io.eprintl
            "I: you should check that your shuffle appears in the list of \
             applied shuffles"
        else Lwt.return_unit
      in
      let* tally, _ = Lazy.force encrypted_tally in
      let factor = E.compute_factor tally sk in
      assert (E.check_factor tally pk factor);
      let pd =
        string_of_partial_decryption (swrite G.to_string)
          (swrite G.Zq.to_string) factor
      in
      let opd = { owned_owner; owned_payload = Hash.hash_string pd } in
      Lwt.return (pd, string_of_owned write_hash opd)

    let tdecrypt owned_owner key pdk =
      let sk = P.derive_sk key and dk = P.derive_dk key in
      let vk = G.(g **~ sk) in
      let* pdk =
        C.recv dk vk (encrypted_msg_of_string (sread G.of_string) pdk)
      in
      let pdk =
        (partial_decryption_key_of_string (sread G.Zq.of_string) pdk)
          .pdk_decryption_key
      in
      let pvk = G.(g **~ pdk) in
      let* trustees = trustees in
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
      let* tally, _ = Lazy.force encrypted_tally in
      let factor = E.compute_factor tally pdk in
      assert (E.check_factor tally pvk factor);
      let pd =
        string_of_partial_decryption (swrite G.to_string)
          (swrite G.Zq.to_string) factor
      in
      let opd = { owned_owner; owned_payload = Hash.hash_string pd } in
      Lwt.return (pd, string_of_owned write_hash opd)

    let compute_result () =
      let* pds =
        let* x = Lazy.force pds in
        match x with
        | None -> failwith "missing partial decryptions"
        | Some x -> Lwt.return x
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
      let* tally, sized = Lazy.force encrypted_tally in
      let sized = { sized with sized_encrypted_tally = tally } in
      let* trustees = trustees in
      match trustees with
      | Some trustees -> (
          match E.compute_result sized factors trustees with
          | Ok result ->
              Lwt.return @@ string_of_election_result write_result result
          | Error e ->
              failwith (Belenios.Trustees.string_of_combination_error e))
      | None -> failwith "missing trustees"

    let verify_ballot raw_ballot =
      let* ballot_box =
        let* x = Lazy.force unverified_ballots in
        x
        |> List.map (fun (h, _, _, _) -> Hash.to_b64 h)
        |> SSet.of_list |> Lwt.return
      in
      let* x = pre_cast ballot_box raw_ballot in
      match x with
      | Error e ->
          Printf.ksprintf failwith "error: %s in ballot %s"
            (string_of_cast_error e) raw_ballot
      | Ok _ -> Lwt_io.eprintl "I: ballot is valid"

    let shuffles_check shuffles =
      let* rtally, _ = Lazy.force raw_encrypted_tally in
      let cc = E.extract_nh_ciphertexts rtally in
      let rec loop i cc ss =
        match ss with
        | s :: ss ->
            if E.check_shuffle cc s then loop (i + 1) s.shuffle_ciphertexts ss
            else Printf.ksprintf failwith "shuffle #%d failed tests" i
        | [] -> Lwt.return_true
      in
      loop 0 cc shuffles

    let verify ?(skip_ballot_check = false) () =
      let* () = fsck () in
      let* trustees = trustees in
      (match trustees with
      | Some trustees ->
          assert (K.check trustees);
          assert (G.(public_key =~ K.combine_keys trustees))
      | None -> failwith "missing trustees");
      let* () =
        let* x = Lazy.force raw_ballots in
        match x with
        | Some _ -> (
            match skip_ballot_check with
            | false ->
                ignore (Lazy.force verified_ballots);
                Lwt.return_unit
            | true ->
                ignore (Lazy.force unverified_ballots);
                Lwt.return_unit)
        | None -> Lwt_io.eprintl "I: no ballots to check"
      in
      let* () =
        let* x = Getters.get_encrypted_tally () in
        match x with
        | Some (_, x) ->
            let* _, y = Lazy.force raw_encrypted_tally in
            if x.sized_encrypted_tally = y.sized_encrypted_tally then
              Lwt_io.eprintlf "I: fingerprint of encrypted tally is %s"
                (Hash.to_b64 x.sized_encrypted_tally)
            else failwith "encrypted tally failed verification"
        | None -> Lwt.return_unit
      in
      let* () =
        let* x = Lazy.force shuffles in
        match x with
        | Some shuffles ->
            let* b = shuffles_check shuffles in
            assert b;
            Lwt.return_unit
        | None -> Lwt_io.eprintl "I: no shuffles to check"
      in
      let* () =
        let* x = Lazy.force result in
        let* y = Lazy.force pds in
        match (x, trustees, y) with
        | None, _, _ -> Lwt_io.eprintl "I: no result to check"
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
            let* tally, sized = Lazy.force encrypted_tally in
            let sized = { sized with sized_encrypted_tally = tally } in
            if not (E.check_result sized factors trustees result) then
              failwith "check_result failed"
            else Lwt.return_unit
      in
      Lwt_io.eprintl "I: all checks passed"

    let shuffle_ciphertexts owned_owner =
      let* cc, _ = Lazy.force encrypted_tally in
      let cc = E.extract_nh_ciphertexts cc in
      let shuffle = E.shuffle_ciphertexts cc in
      let shuffle_s =
        string_of_shuffle (swrite G.to_string) (swrite G.Zq.to_string) shuffle
      in
      let owned = { owned_owner; owned_payload = Hash.hash_string shuffle_s } in
      Lwt.return (shuffle_s, string_of_owned write_hash owned)

    let checksums () =
      let* election = election_hash in
      let* shuffles =
        let* x = Lazy.force raw_shuffles in
        match x with
        | None -> Lwt.return_none
        | Some x -> Lwt.return_some @@ List.map (fun (_, x, _) -> x) x
      in
      let* encrypted_tally =
        try
          let* _, x = Lazy.force encrypted_tally in
          Lwt.return_some x.sized_encrypted_tally
        with _ -> Lwt.return_none
      in
      let* trustees =
        let* trustees_as_string = trustees_as_string in
        match trustees_as_string with
        | None -> failwith "missing trustees"
        | Some x -> Lwt.return x
      in
      let* public_credentials =
        let* x = Lazy.force raw_public_creds in
        match x with
        | None -> failwith "missing credentials"
        | Some x -> Lwt.return x
      in
      let* final = Getters.get_final () in
      Belenios.Election.compute_checksums ~election ~shuffles ~encrypted_tally
        ~trustees ~public_credentials ~final
      |> string_of_election_checksums |> Lwt.return

    let compute_voters privcreds =
      let map =
        List.fold_left
          (fun accu (id, cred) ->
            match Cred.derive cred with
            | Error _ -> Printf.ksprintf failwith "invalid credential %s" cred
            | Ok x -> SMap.add G.(g **~ x |> to_string) id accu)
          SMap.empty privcreds
      in
      let* ballots = Lazy.force unverified_ballots in
      List.fold_left
        (fun accu (h, cred, _, _) ->
          match SMap.find_opt cred map with
          | None ->
              Printf.ksprintf failwith "Unknown public key in ballot %s"
                (Hash.to_b64 h)
          | Some id -> id :: accu)
        [] ballots
      |> Lwt.return

    let compute_ballot_summary () =
      let* has_weights =
        let* x = Lazy.force public_creds_weights in
        match x with None -> Lwt.return_false | Some (b, _) -> Lwt.return b
      in
      let* x = Lazy.force unverified_ballots in
      x
      |> List.rev_map (fun (bs_hash, _, w, _) ->
             let bs_weight =
               if has_weights then Some w
               else (
                 assert (Weight.is_int w 1);
                 None)
             in
             { bs_hash; bs_weight })
      |> string_of_ballot_summary |> Lwt.return

    let compute_encrypted_tally () =
      let* et, sized = Lazy.force raw_encrypted_tally in
      Lwt.return
        ( string_of_encrypted_tally (swrite G.to_string) et,
          string_of_sized_encrypted_tally write_hash sized )
  end in
  Lwt.return (module X : S)
