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

open Belenios_core
open Serializable
open Belenios_question

let get_complexity qs =
  Array.fold_left
    (fun accu q ->
      let { nb_ciphertexts; nb_zkps } = Question.get_complexity q in
      {
        nb_ciphertexts = accu.nb_ciphertexts + nb_ciphertexts;
        nb_zkps = accu.nb_zkps + nb_zkps;
      })
    { nb_ciphertexts = 0; nb_zkps = 0 }
    qs

let template_of_yojson x =
  let params = params_of_yojson Fun.id x in
  {
    name = params.name;
    description = params.description;
    questions = params.questions;
    administrator = params.administrator;
    credential_authority = params.credential_authority;
    language = params.language;
  }

let make_raw_election (template : _ template) ~uuid ~group ~public_key =
  let module G = (val Group.of_string group) in
  let public_key = G.of_string public_key in
  let params =
    {
      version = 2;
      description = template.description;
      name = template.name;
      questions = template.questions;
      uuid;
      administrator = template.administrator;
      credential_authority = template.credential_authority;
      language = template.language;
      group;
      public_key;
    }
  in
  yojson_of_params !&G.to_string params

module Parse (R : RAW_ELECTION) () = struct
  let yojson_of_question = yojson_of_question
  let question_of_yojson = question_of_yojson

  let erase_question =
    Question.to_concrete >> erase_question >> Question.of_concrete

  let json_election = Json.of_string R.raw_election
  let j = params_of_yojson Fun.id json_election

  module G = (val Group.of_string j.group)

  let params = params_of_yojson !$G.of_string json_election
  let version = params.version
  let uuid = params.uuid

  let template =
    {
      name = params.name;
      description = params.description;
      questions = params.questions;
      administrator = params.administrator;
      credential_authority = params.credential_authority;
      language = params.language;
    }

  let has_nh_questions = Array.exists Question.is_nh_question params.questions
  let fingerprint = Hash.hash_string R.raw_election
  let public_key = params.public_key

  type nonrec ballot = (G.t, G.Zq.t) ballot

  let yojson_of_ballot = yojson_of_ballot !&G.to_string !&G.Zq.to_string
  let ballot_of_yojson = ballot_of_yojson !$G.of_string !$G.Zq.of_string
  let get_credential x = Some x.message.credential

  type result = json

  let yojson_of_result = Fun.id
  let result_of_yojson = Fun.id
  let of_generic_result x = `List (x |> Array.to_list)

  let to_generic_result = function
    | `List x -> x |> Array.of_list
    | _ -> invalid_arg "to_generirc_result: list expected"
end

module MakeElection (W : ELECTION_DATA with type question := Question.t) =
struct
  type element = W.G.t
  type scalar = W.G.Zq.t

  module G = W.G
  module Q = Question.Make (G)
  module Mix = Mixnet.Make (W)
  module P = Pki.Make (G)
  open G

  type private_key = scalar
  type public_key = element

  let random () = Zq.random (Crypto_primitives.get_rng ())
  let ( / ) x y = x *~ invert y

  type plaintext = int Shape.t array
  type nonrec ballot = (element, scalar) ballot
  type weighted_ballot = Weight.t * ballot

  (** Fiat-Shamir non-interactive zero-knowledge proofs of knowledge *)

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

  let xch_ballot =
    {
      dst = dst_prefix ^ "-ballot";
      of_yojson = raw_ballot_of_yojson !$G.of_string !$G.Zq.of_string;
      to_yojson = yojson_of_raw_ballot !&G.to_string !&G.Zq.to_string;
    }

  let create_ballot ~sk m =
    let election_uuid = W.uuid in
    let election_hash = W.fingerprint in
    let credential = G.(g **~ sk) in
    let zkp = Hash.to_hex W.fingerprint ^ "|" ^ G.to_string credential in
    let answers =
      swap (Array.map2 (create_answer W.public_key zkp) W.template.questions m)
    in
    let raw_ballot = { election_uuid; election_hash; credential; answers } in
    P.sign xch_ballot sk raw_ballot

  (** Ballot verification *)

  let verify_answer y zkp q a = Q.verify_answer q ~public_key:y ~prefix:zkp a

  let check_ballot (ballot : ballot) =
    let { election_uuid; election_hash; credential; answers } =
      ballot.message
    in
    let zkp = Hash.to_hex W.fingerprint ^ "|" ^ G.to_string credential in
    election_uuid = W.uuid
    && election_hash = W.fingerprint
    && P.verify xch_ballot credential ballot
    && Array.for_all2
         (verify_answer W.public_key zkp)
         W.template.questions answers

  let check_rawballot rawballot =
    match
      rawballot |> Json.of_string
      |> ballot_of_yojson !$G.of_string !$G.Zq.of_string
    with
    | exception e -> Error (`SerializationError (Printexc.to_string e))
    | ballot ->
        if
          ballot
          |> yojson_of_ballot !&G.to_string !&G.Zq.to_string
          |> Json.to_string = rawballot
        then
          Ok
            {
              rc_credential = G.to_string ballot.message.credential;
              rc_check = (fun () -> check_ballot ballot);
            }
        else Error `NonCanonical

  let process_ballots bs =
    `Array
      (Array.mapi
         (fun i q ->
           Q.process_ciphertexts q
             (List.map
                (fun (w, b) ->
                  (w, Q.extract_ciphertexts q b.message.answers.(i)))
                bs))
         W.template.questions)

  let extract_nh_ciphertexts x =
    let x = Shape.to_shape_array x in
    let rec loop i accu =
      if i >= 0 then
        match Question.is_nh_question W.template.questions.(i) with
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
        match Question.is_nh_question W.template.questions.(i) with
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
        let ciphertexts, proofs = Array.(split (of_list accu)) in
        { ciphertexts; proofs }
    in
    loop (Array.length cc - 1) []

  let check_shuffle cc s =
    Array.for_all3
      (Mix.check_shuffle_proof W.public_key)
      cc s.ciphertexts s.proofs

  type factor = (element, scalar) partial_decryption

  let eg_factor x ({ alpha; _ } : _ ciphertext) =
    let zkp = Hash.to_hex W.fingerprint ^ "|" ^ G.to_string (g **~ x) ^ "|" in
    let dst = dst_prefix ^ "-decrypt" in
    (alpha **~ x, fs_prove [| g; alpha |] x (hash ~dst zkp))

  let check_ciphertext c =
    Shape.forall
      (fun ({ alpha; beta } : _ ciphertext) -> G.check alpha && G.check beta)
      c

  let compute_factor c x =
    if check_ciphertext c then
      let res = Shape.map (eg_factor x) c in
      let decryption_factors, decryption_proofs = Shape.split res in
      { decryption_factors; decryption_proofs }
    else invalid_arg "Invalid ciphertext"

  let check_factor c y f =
    let zkp = Hash.to_hex W.fingerprint ^ "|" ^ G.to_string y ^ "|" in
    let dst = dst_prefix ^ "-decrypt" in
    Shape.forall3
      (fun ({ alpha; _ } : _ ciphertext) f { challenge; response } ->
        G.check f
        &&
        let commitments =
          [|
            (g **~ response) *~ (y **~ challenge);
            (alpha **~ response) *~ (f **~ challenge);
          |]
        in
        Zq.(G.hash ~dst zkp commitments =% challenge))
      c f.decryption_factors f.decryption_proofs

  type result_type = W.result
  type result = result_type election_result

  module Combinator = Trustees.MakeCombinator (G)

  let compute_result encrypted_tally partial_decryptions trustees =
    let total_weight = encrypted_tally.total_weight in
    let et = encrypted_tally.encrypted_tally in
    let check = check_factor et in
    match Combinator.combine_factors trustees check partial_decryptions with
    | Ok factors -> (
        match
          Shape.map2 (fun ({ beta; _ } : _ ciphertext) f -> beta / f) et factors
        with
        | `Array rs ->
            Array.map2 (Q.compute_result ~total_weight) W.template.questions rs
            |> W.of_generic_result
            |> fun result -> Ok { result }
        | `Atomic _ -> failwith "compute_result: invalid shape")
    | Error e -> Error e

  let check_result encrypted_tally partial_decryptions trustees
      ({ result } : result) =
    let total_weight = encrypted_tally.total_weight in
    let encrypted_tally = encrypted_tally.encrypted_tally in
    check_ciphertext encrypted_tally
    &&
    let check = check_factor encrypted_tally in
    match Combinator.combine_factors trustees check partial_decryptions with
    | Error _ -> false
    | Ok factors -> (
        match
          Shape.map2
            (fun ({ beta; _ } : _ ciphertext) f -> beta / f)
            encrypted_tally factors
        with
        | `Array rs ->
            Array.for_all3
              (Q.check_result ~total_weight)
              W.template.questions rs
              (W.to_generic_result result)
        | `Atomic _ -> failwith "check_result: invalid shape")
end

module Make (R : RAW_ELECTION) () = struct
  module X = Parse (R) ()
  include X
  module E = MakeElection (X)
end
