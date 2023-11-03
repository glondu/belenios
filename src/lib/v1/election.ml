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

module PSerializable_j = Serializable_j
open Belenios_core
open Serializable_core_j
open Serializable_j
open PSerializable_j
open Signatures
open Common
open Belenios_question

let template_of_string x =
  let open PSerializable_j in
  let params = params_of_string Yojson.Safe.read_json x in
  {
    t_name = params.e_name;
    t_description = params.e_description;
    t_questions = params.e_questions;
    t_administrator = params.e_administrator;
    t_credential_authority = params.e_credential_authority;
  }

let make_raw_election template ~uuid ~group ~public_key =
  let module G = (val Group.of_string group) in
  let e_public_key = G.of_string public_key in
  let open PSerializable_j in
  let params =
    {
      e_version = 1;
      e_description = template.t_description;
      e_name = template.t_name;
      e_questions = template.t_questions;
      e_uuid = uuid;
      e_administrator = template.t_administrator;
      e_credential_authority = template.t_credential_authority;
      e_group = group;
      e_public_key;
    }
  in
  string_of_params (swrite G.to_string) params

module Parse (R : RAW_ELECTION) () = struct
  let read_question a b = Question.of_concrete (read_question a b)
  let write_question b x = write_question b (Question.to_concrete x)

  let erase_question x =
    x |> Question.to_concrete |> erase_question |> Question.of_concrete

  let j = params_of_string Yojson.Safe.read_json R.raw_election

  module G = (val Group.of_string j.e_group)

  let version = 1
  let params = params_of_string (sread G.of_string) R.raw_election
  let uuid = params.e_uuid

  let template =
    {
      t_name = params.e_name;
      t_description = params.e_description;
      t_questions = params.e_questions;
      t_administrator = params.e_administrator;
      t_credential_authority = params.e_credential_authority;
    }

  let has_nh_questions = Array.exists Question.is_nh_question params.e_questions
  let fingerprint = sha256_b64 R.raw_election
  let public_key = params.e_public_key

  type nonrec ballot = (G.t, G.Zq.t) ballot

  let string_of_ballot x =
    string_of_ballot (swrite G.to_string) (swrite G.Zq.to_string) x

  let ballot_of_string x =
    ballot_of_string (sread G.of_string) (sread G.Zq.of_string) x

  let get_credential x = Some x.credential

  type result = Yojson.Safe.t

  let of_generic_result x =
    `List (x |> Array.map Yojson.Safe.from_string |> Array.to_list)

  let to_generic_result = function
    | `List x -> x |> Array.of_list |> Array.map Yojson.Safe.to_string
    | _ -> invalid_arg "to_generirc_result: list expected"

  let write_result = Yojson.Safe.write_json
  let read_result = Yojson.Safe.read_json
end

module MakeElection
    (W : ELECTION_DATA with type question := Question.t)
    (M : RANDOM) =
struct
  type element = W.G.t
  type scalar = W.G.Zq.t

  module G = W.G
  module Q = Question.Make (M) (G)
  module Mix = Mixnet.Make (W) (M)
  open G

  type private_key = scalar
  type public_key = element

  let random () = M.random Zq.q |> Zq.coerce
  let ( / ) x y = x *~ invert y

  type plaintext = int array array
  type nonrec ballot = (element, scalar) ballot
  type weighted_ballot = Weight.t * ballot

  (** Fiat-Shamir non-interactive zero-knowledge proofs of
      knowledge *)

  let fs_prove gs x oracle =
    let w = random () in
    let commitments = Array.map (fun g -> g **~ w) gs in
    let challenge = oracle commitments in
    let response = Zq.(w - (x * challenge)) in
    { challenge; response }

  (** Ballot creation *)

  let swap xs =
    let rec loop i accu =
      if i >= 0 then
        let x = xs.(i) in
        loop (pred i) (x :: accu)
      else Array.of_list accu
    in
    loop (pred (Array.length xs)) []

  let create_answer y zkp q m = Q.create_answer q ~public_key:y ~prefix:zkp m
  let make_sig_prefix hash = "sig|" ^ hash ^ "|"

  let create_ballot ~sk m =
    let election_uuid = W.uuid in
    let election_hash = W.fingerprint in
    let credential = G.(g **~ sk) in
    let zkp = W.fingerprint ^ "|" ^ G.to_string credential in
    let answers =
      swap
        (Array.map2 (create_answer W.public_key zkp) W.template.t_questions m)
    in
    let ballot_without_signature =
      { election_uuid; election_hash; credential; answers; signature = None }
    in
    let s_hash =
      sha256_b64
        (string_of_ballot (swrite G.to_string) (swrite G.Zq.to_string)
           ballot_without_signature)
    in
    let signature =
      let w = random () in
      let commitment = g **~ w in
      let prefix = make_sig_prefix s_hash in
      let challenge = G.hash prefix [| commitment |] in
      let response = Zq.(w - (sk * challenge)) in
      let s_proof = { challenge; response } in
      Some { s_hash; s_proof }
    in
    { election_uuid; election_hash; credential; answers; signature }

  (** Ballot verification *)

  let verify_answer y zkp q a = Q.verify_answer q ~public_key:y ~prefix:zkp a

  let check_ballot
      { election_uuid; election_hash; credential; answers; signature } =
    let ballot_without_signature =
      { election_uuid; election_hash; credential; answers; signature = None }
    in
    let expected_hash =
      sha256_b64
        (string_of_ballot (swrite G.to_string) (swrite G.Zq.to_string)
           ballot_without_signature)
    in
    let zkp = W.fingerprint ^ "|" ^ G.to_string credential in
    election_uuid = W.uuid
    && election_hash = W.fingerprint
    && (match signature with
       | Some { s_hash; s_proof = { challenge; response } } ->
           s_hash = expected_hash && G.check credential
           &&
           let commitment = (g **~ response) *~ (credential **~ challenge) in
           let prefix = make_sig_prefix s_hash in
           Zq.(challenge =% G.hash prefix [| commitment |])
       | None -> false)
    && Array.for_all2
         (verify_answer W.public_key zkp)
         W.template.t_questions answers

  let check_rawballot rawballot =
    match
      ballot_of_string (sread G.of_string) (sread G.Zq.of_string) rawballot
    with
    | exception e -> Error (`SerializationError e)
    | ballot ->
        if
          string_of_ballot (swrite G.to_string) (swrite G.Zq.to_string) ballot
          = rawballot
        then
          Ok
            {
              rc_credential = G.to_string ballot.credential;
              rc_check = (fun () -> check_ballot ballot);
            }
        else Error `NonCanonical

  let process_ballots bs =
    `Array
      (Array.mapi
         (fun i q ->
           Q.process_ciphertexts q
             (List.map
                (fun (w, b) -> (w, Q.extract_ciphertexts q b.answers.(i)))
                bs))
         W.template.t_questions)

  let extract_nh_ciphertexts x =
    let x = Shape.to_shape_array x in
    let rec loop i accu =
      if i >= 0 then
        match Question.is_nh_question W.template.t_questions.(i) with
        | false -> loop (i - 1) accu
        | true -> loop (i - 1) (Shape.to_array x.(i) :: accu)
      else Array.of_list accu
    in
    loop (Array.length x - 1) []

  let merge_nh_ciphertexts cc x =
    let x = Array.copy (Shape.to_shape_array x) in
    let n = Array.length x and m = Array.length cc in
    let rec loop i j =
      if i < n && j < m then (
        match Question.is_nh_question W.template.t_questions.(i) with
        | false -> loop (i + 1) j
        | true ->
            x.(i) <- Shape.of_array cc.(j);
            loop (i + 1) (j + 1))
      else (
        assert (j = m);
        `Array x)
    in
    loop 0 0

  let shuffle_ciphertexts cc =
    let rec loop i accu =
      if i >= 0 then
        let c = cc.(i) in
        let c', r', psi = Mix.gen_shuffle W.public_key c in
        let pi = Mix.gen_shuffle_proof W.public_key c c' r' psi in
        loop (i - 1) ((c', pi) :: accu)
      else
        let shuffle_ciphertexts, shuffle_proofs =
          Array.(split (of_list accu))
        in
        { shuffle_ciphertexts; shuffle_proofs }
    in
    loop (Array.length cc - 1) []

  let check_shuffle cc s =
    Array.for_all3
      (Mix.check_shuffle_proof W.public_key)
      cc s.shuffle_ciphertexts s.shuffle_proofs

  type factor = (element, scalar) partial_decryption

  let eg_factor x { alpha; _ } =
    let zkp = "decrypt|" ^ W.fingerprint ^ "|" ^ G.to_string (g **~ x) ^ "|" in
    (alpha **~ x, fs_prove [| g; alpha |] x (hash zkp))

  let check_ciphertext c =
    Shape.forall (fun { alpha; beta } -> G.check alpha && G.check beta) c

  let compute_factor c x =
    if check_ciphertext c then
      let res = Shape.map (eg_factor x) c in
      let decryption_factors, decryption_proofs = Shape.split res in
      { decryption_factors; decryption_proofs }
    else invalid_arg "Invalid ciphertext"

  let check_factor c y f =
    let zkp = "decrypt|" ^ W.fingerprint ^ "|" ^ G.to_string y ^ "|" in
    Shape.forall3
      (fun { alpha; _ } f { challenge; response } ->
        G.check f
        &&
        let commitments =
          [|
            (g **~ response) *~ (y **~ challenge);
            (alpha **~ response) *~ (f **~ challenge);
          |]
        in
        Zq.(hash zkp commitments =% challenge))
      c f.decryption_factors f.decryption_proofs

  type result_type = W.result
  type result = result_type Serializable_t.election_result

  module Combinator = Trustees.MakeCombinator (G)

  let compute_result encrypted_tally partial_decryptions trustees =
    let total_weight = encrypted_tally.sized_total_weight in
    let et = encrypted_tally.sized_encrypted_tally in
    let check = check_factor et in
    match Combinator.combine_factors trustees check partial_decryptions with
    | Ok factors -> (
        match Shape.map2 (fun { beta; _ } f -> beta / f) et factors with
        | `Array rs ->
            Array.map2
              (Q.compute_result ~total_weight)
              W.template.t_questions rs
            |> W.of_generic_result
            |> fun result -> Ok { result }
        | `Atomic _ -> failwith "compute_result: invalid shape")
    | Error e -> Error e

  let check_result encrypted_tally partial_decryptions trustees { result } =
    let total_weight = encrypted_tally.sized_total_weight in
    let encrypted_tally = encrypted_tally.sized_encrypted_tally in
    check_ciphertext encrypted_tally
    &&
    let check = check_factor encrypted_tally in
    match Combinator.combine_factors trustees check partial_decryptions with
    | Error _ -> false
    | Ok factors -> (
        match
          Shape.map2 (fun { beta; _ } f -> beta / f) encrypted_tally factors
        with
        | `Array rs ->
            Array.for_all3
              (Q.check_result ~total_weight)
              W.template.t_questions rs
              (W.to_generic_result result)
        | `Atomic _ -> failwith "check_result: invalid shape")
end

module Make (R : RAW_ELECTION) (M : RANDOM) () = struct
  module X = Parse (R) ()
  include X
  module E = MakeElection (X) (M)
end
