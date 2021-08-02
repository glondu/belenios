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
open Belenios
open Platform
open Serializable_builtin_t
open Serializable_j
open Signatures
open Common

module type PARAMS = sig
  val raw_election : string
  val get_trustees : unit -> string option
  val get_public_creds : unit -> string Stream.t option
  val get_ballots : unit -> string Stream.t option
  val get_shuffles : unit -> string Stream.t option
  val get_result : unit -> string option
  val print_msg : string -> unit
end

module type S = sig
  val vote : string option -> int array array -> string
  val decrypt : string -> string
  val tdecrypt : string -> string -> string
  val validate : string list -> string
  val verify : unit -> unit
  val shuffle_ciphertexts : unit -> string
  val checksums : unit -> string
  val compute_voters : string list -> string list
end

module type PARSED_PARAMS = sig
  include PARAMS
  include ELECTION with type 'a m = 'a
end

let parse_params p =
  let module P = (val p : PARAMS) in
  let module R = struct
    include P
    include Election.ParseMake (P) (DirectRandom) ()
  end in
  (module R : PARSED_PARAMS)

module Make (P : PARSED_PARAMS) : S = struct

  open P

  module P = Trustees.MakePKI (G) (DirectRandom)
  module C = Trustees.MakeChannels (G) (DirectRandom) (P)

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

  module GSet = Map.Make (G)
  module PPC = Credential.MakeParsePublicCredential (G)

  let public_creds = lazy (
    get_public_creds () |> Option.map (fun creds ->
      let res = ref GSet.empty in
      Stream.iter
        (fun x ->
          match PPC.parse_public_credential x with
          | Some (w, y) ->
             res := GSet.add y (w, ref false) !res
          | None ->
            Printf.ksprintf failwith "%s is not a valid public credential" x;
        ) creds;
      res
    )
  )

  let ballots = lazy (
    get_ballots () |> Option.map (fun ballots ->
      let res = ref [] in
      Stream.iter (fun x ->
        res := (ballot_of_string G.read x, sha256_b64 x) :: !res
      ) ballots;
      List.rev !res
    )
  )

  let check_signature_present = lazy (
    match Lazy.force public_creds with
    | Some creds -> (fun b ->
      match b.signature with
      | Some s ->
         (match GSet.find_opt s.s_public_key !creds with
          | Some (w, used) -> if !used then None else (used := true; Some w)
          | None -> None)
      | None -> None
    )
    | None -> (fun _ -> Some Weight.one)
  )

  let cast (b, hash) =
    match
      match Lazy.force check_signature_present b with
      | Some w -> if E.check_ballot b then Some w else None
      | None -> None
    with
    | Some w -> w
    | None -> Printf.ksprintf failwith "ballot %s failed tests" hash

  let ballots_weights = lazy (
    Lazy.force ballots |> Option.map (List.map cast)
  )

  let raw_encrypted_tally =
    lazy (
        match Lazy.force ballots with
        | None -> E.process_ballots [||], Weight.zero
        | Some ballots ->
           match Lazy.force ballots_weights with
           | None -> failwith "ballots or public credentials are missing"
           | Some weights ->
              let ballots = List.map fst ballots in
              let ballots = List.combine weights ballots |> Array.of_list in
              let nballots =
                let open Weight in
                Array.fold_left (fun accu (w, _) -> accu + w) zero ballots
              in
              E.process_ballots ballots, nballots
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
        | None, Some s ->
           let rec loop accu =
             if (try Stream.empty s; true with Stream.Failure -> false) then
               Some (List.rev accu)
             else
               loop (Stream.next s :: accu)
           in
           loop []
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
        let rtally, _ = Lazy.force raw_encrypted_tally in
        let cc = E.extract_nh_ciphertexts rtally in
        let rec loop i cc ss =
          match ss with
          | s :: ss ->
             if E.check_shuffle cc s then
               loop (i+1) s.shuffle_ciphertexts ss
             else
               Printf.ksprintf failwith "shuffle #%d failed tests" i
          | [] -> true
        in
        match Lazy.force shuffles with
        | Some ss -> loop 0 cc ss
        | None -> true
      )

  let encrypted_tally =
    lazy (
        let raw_encrypted_tally, ntally = Lazy.force raw_encrypted_tally in
        match Option.map List.rev (Lazy.force shuffles) with
        | Some (s :: _) -> E.merge_nh_ciphertexts s.shuffle_ciphertexts raw_encrypted_tally, ntally
        | _ -> raw_encrypted_tally, ntally
      )

  let vote privcred ballot =
    let sk =
      privcred |> Option.map (fun cred ->
        let module CD = Credential.MakeDerive (G) in
        CD.derive election.e_uuid cred
      )
    in
    let b = E.create_ballot ?sk ballot in
    assert (E.check_ballot b);
    string_of_ballot G.write b

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
    let tally, _ = Lazy.force encrypted_tally in
    let factor = E.compute_factor tally sk in
    assert (E.check_factor tally pk factor);
    string_of_partial_decryption G.write factor

  let tdecrypt key pdk =
    let sk = P.derive_sk key and dk = P.derive_dk key in
    let vk = G.(g **~ sk) in
    let pdk = C.recv dk vk pdk in
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
    let tally, _ = Lazy.force encrypted_tally in
    let factor = E.compute_factor tally pdk in
    assert (E.check_factor tally pvk factor);
    string_of_partial_decryption G.write factor

  let validate factors =
    let factors = List.map (partial_decryption_of_string G.read) factors in
    let tally, nballots = Lazy.force encrypted_tally in
    let shuffles = Lazy.force shuffles in
    match trustees with
    | Some trustees ->
       (match E.compute_result ?shuffles nballots tally factors trustees with
        | Ok result ->
           assert (E.check_result trustees result);
           string_of_election_result G.write write_result result
        | Error e -> failwith (Trustees.string_of_combination_error e)
       )
    | None -> failwith "missing trustees"

  let verify () =
    (match trustees with
     | Some trustees ->
        assert (K.check trustees);
        assert G.(public_key =~ K.combine_keys trustees)
     | None -> failwith "missing trustees"
    );
    (match Lazy.force ballots_weights with
    | Some _ -> assert (Lazy.force shuffles_check)
    | None -> print_msg "W: no ballots to check"
    );
    (match Lazy.force result, trustees with
    | Some result, Some trustees ->
       assert (fst (Lazy.force encrypted_tally) = result.encrypted_tally);
       assert (E.check_result trustees result)
    | Some _, None -> failwith "missing trustees"
    | None, _ -> print_msg "W: no result to check"
    );
    print_msg "I: all checks passed"

  let shuffle_ciphertexts () =
    let cc, _ = Lazy.force encrypted_tally in
    let cc = E.extract_nh_ciphertexts cc in
    let s = E.shuffle_ciphertexts cc in
    string_of_shuffle G.write s

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
         !public_creds
         |> GSet.bindings
         |> List.map fst
         |> List.map (fun x -> G.to_string x ^ "\n")
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
    let module GMap = Map.Make (G) in
    let map =
      List.fold_left
        (fun accu line ->
          let id, cred = split line in
          GMap.add G.(g **~ D.derive election.e_uuid cred) id accu
        ) GMap.empty privcreds
    in
    (match Lazy.force ballots with
     | None -> []
     | Some bs -> bs)
    |> List.map
         (fun (b, h) ->
           match b.signature with
           | None -> Printf.ksprintf failwith "Missing signature in ballot %s" h
           | Some s ->
              match GMap.find_opt s.s_public_key map with
              | None -> Printf.ksprintf failwith "Unknown public key in ballot %s" h
              | Some id -> id
         )

end

let make params =
  let module P = (val parse_params params : PARSED_PARAMS) in
  let module R = Make (P) in
  (module R : S)
