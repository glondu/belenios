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

module PSerializable_j = Serializable_j
open Belenios_platform
open Belenios_core
open Platform
open Serializable_builtin_t
open Serializable_core_j
open Serializable_j
open PSerializable_j
open Signatures
open Common

let of_string x =
  let open PSerializable_j in
  let params = params_of_string Yojson.Safe.read_json x in
  let {
      e_description; e_name; e_questions; e_uuid;
      e_administrator; e_credential_authority;
      _
    } = params
  in
  let open Serializable_j in
  {
    e_version = 0;
    e_description; e_name; e_questions; e_uuid;
    e_administrator; e_credential_authority;
  }

let to_string params ~group ~public_key =
  let open Serializable_j in
  let {
      e_description; e_name; e_questions; e_uuid;
      e_administrator; e_credential_authority;
      _
    } = params
  in
  let wpk_group = ff_params_of_string group in
  let module G = (val Group.of_string group) in
  let wpk_y = G.of_string public_key in
  let e_public_key = {wpk_group; wpk_y} in
  let open PSerializable_j in
  let params = {
      e_description; e_name; e_questions; e_uuid;
      e_administrator; e_credential_authority;
      e_public_key;
    }
  in
  string_of_params (write_wrapped_pubkey write_ff_params G.write) params

module Parse (R : RAW_ELECTION) () = struct
  let params = params_of_string (read_wrapped_pubkey Yojson.Safe.read_json Yojson.Safe.read_json) R.raw_election
  let group = Yojson.Safe.to_string params.e_public_key.wpk_group
  let y = Yojson.Safe.Util.to_string params.e_public_key.wpk_y

  module G = (val Group.of_string group)

  let election =
    let {
        e_description; e_name; e_questions; e_uuid;
        e_administrator; e_credential_authority;
        _
      } = params
    in
    let open Serializable_j in
    {
      e_version = 0;
      e_description; e_name; e_questions; e_uuid;
      e_administrator; e_credential_authority;
    }
  let fingerprint = sha256_b64 R.raw_election
  let public_key = G.of_string y

  type nonrec ballot = G.t ballot
  let string_of_ballot x = string_of_ballot G.write x
  let ballot_of_string x = ballot_of_string G.read x
  let get_credential x =
    match x.signature with
    | None -> None
    | Some s -> Some s.s_public_key

end

module MakeElection (W : ELECTION_DATA) (M : RANDOM) = struct
  type 'a m = 'a M.t
  let ( let* ) = M.bind

  type elt = W.G.t

  module G = W.G
  module Q = Question.Make (M) (G) (Question_h.Make (M) (G)) (Question_nh.Make (M) (G))
  module Mix = Mixnet.Make (W) (M)
  open G
  let election = W.election
  let y = W.public_key

  type private_key = Z.t
  type public_key = elt

  let ( / ) x y = x *~ invert y

  type plaintext = int array array
  type nonrec ballot = elt ballot
  type weighted_ballot = Weight.t * ballot

  (** Fiat-Shamir non-interactive zero-knowledge proofs of
      knowledge *)

  let fs_prove gs x oracle =
    let* w = M.random q in
    let commitments = Array.map (fun g -> g **~ w) gs in
    let* () = M.yield () in
    let challenge = oracle commitments in
    let response = Z.((w + x * challenge) mod q) in
    M.return {challenge; response}

  (** Ballot creation *)

  let swap xs =
    let rec loop i accu =
      if i >= 0
      then let* x = xs.(i) in loop (pred i) (x::accu)
      else M.return (Array.of_list accu)
    in loop (pred (Array.length xs)) []

  let create_answer y zkp q m =
    Q.create_answer q ~public_key:y ~prefix:zkp m

  let make_sig_prefix zkp commitment =
    "sig|" ^ zkp ^ "|" ^ G.to_string commitment ^ "|"

  let make_sig_contents answers =
    Array.map2 Q.extract_ciphertexts election.e_questions answers
    |> Array.map Shape.flatten
    |> Array.to_list
    |> List.flatten
    |> List.map (fun {alpha; beta} -> [alpha; beta])
    |> List.flatten
    |> Array.of_list

  let create_ballot ~sk m =
    let y = G.(g **~ sk) in
    let zkp = G.to_string y in
    let* answers = swap (Array.map2 (create_answer W.public_key zkp) election.e_questions m) in
    let* signature =
      let* w = M.random q in
      let commitment = g **~ w in
      let prefix = make_sig_prefix zkp commitment in
      let contents = make_sig_contents answers in
      let s_challenge = G.hash prefix contents in
      let s_response = Z.(erem (w - sk * s_challenge) q) in
      M.return (Some {s_public_key = y; s_challenge; s_response})
    in
    M.return {
        answers;
        election_hash = W.fingerprint;
        election_uuid = election.e_uuid;
        signature;
      }

  (** Ballot verification *)

  let verify_answer y zkp q a =
    Q.verify_answer q ~public_key:y ~prefix:zkp a

  let check_ballot b =
    b.election_uuid = election.e_uuid &&
      b.election_hash = W.fingerprint &&
        let ok, zkp = match b.signature with
          | Some {s_public_key = y; s_challenge; s_response} ->
             let zkp = G.to_string y in
             let ok =
               G.check y &&
                 check_modulo q s_challenge &&
                   check_modulo q s_response &&
                     let commitment = g **~ s_response *~ y **~ s_challenge in
                     let prefix = make_sig_prefix zkp commitment in
                     let contents = make_sig_contents b.answers in
                     Z.(s_challenge =% G.hash prefix contents)
             in ok, zkp
          | None -> true, ""
        in ok &&
             Array.forall2 (verify_answer W.public_key zkp) election.e_questions b.answers

  module CastBallot (B : BBOX_OPS with type 'a m := 'a m) = struct

    let check_credential ?user ?weight credential =
      let* x = B.get_credential_record credential in
      match x with
      | None -> M.return (Error `InvalidCredential)
      | Some cr ->
         let weight_ok =
           match weight with
           | None -> true
           | Some weight -> Weight.compare weight cr.cr_weight = 0
         in
         if weight_ok then (
           let* user_record =
             match user with
             | None -> M.return None
             | Some user -> B.get_user_record user
           in
           match user_record, cr.cr_ballot with
           | None, (None as x) -> M.return (Ok x)
           | None, Some _ -> M.return (Error `UsedCredential)
           | Some _, None -> M.return (Error `RevoteNotAllowed)
           | Some old_credential, (Some _ as x) ->
              if credential = old_credential then
                M.return (Ok x)
              else
                M.return (Error `WrongCredential)
         ) else M.return (Error `WrongWeight)

    let cast ?user ?weight rawballot =
      match ballot_of_string G.read rawballot with
      | exception e -> M.return (Error (`SerializationError e))
      | ballot ->
         if string_of_ballot G.write ballot <> rawballot then (
           M.return (Error `NonCanonical)
         ) else (
           match ballot.signature with
           | None -> M.return (Error `InvalidBallot)
           | Some s ->
              let credential = G.to_string s.s_public_key in
              let* x = check_credential ?user ?weight credential in
              match x with
              | Error _ as e -> M.return e
              | Ok old ->
                 let* () = M.yield () in
                 if check_ballot ballot then
                   M.return (Ok (credential, ballot, old))
                 else
                   M.return (Error `InvalidBallot)
         )

  end

  let process_ballots bs =
    SArray (
        Array.mapi (fun i q ->
            Q.process_ciphertexts q
              (List.map
                 (fun (w, b) ->
                   w, Q.extract_ciphertexts q b.answers.(i)
                 ) bs
              )
          ) election.e_questions
      )

  let extract_nh_ciphertexts x =
    let x = Shape.to_shape_array x in
    let rec loop i accu =
      if i >= 0 then (
        match election.e_questions.(i) with
        | Question.Homomorphic _ -> loop (i-1) accu
        | Question.NonHomomorphic _ -> loop (i-1) (Shape.to_array x.(i) :: accu)
      ) else Array.of_list accu
    in
    loop (Array.length x - 1) []

  let merge_nh_ciphertexts cc x =
    let x = Array.copy (Shape.to_shape_array x) in
    let n = Array.length x and m = Array.length cc in
    let rec loop i j =
      if i < n && j < m then (
        match election.e_questions.(i) with
        | Question.Homomorphic _ -> loop (i+1) j
        | Question.NonHomomorphic _ ->
           x.(i) <- Shape.of_array cc.(j);
           loop (i+1) (j+1)
      ) else (
        assert (j = m);
        SArray x
      )
    in
    loop 0 0

  let shuffle_ciphertexts cc =
    let rec loop i accu =
      if i >= 0 then (
        let c = cc.(i) in
        let* (c', r', psi) = Mix.gen_shuffle y c in
        let* pi = Mix.gen_shuffle_proof y c c' r' psi in
        loop (i-1) ((c', pi) :: accu)
      ) else (
        let shuffle_ciphertexts, shuffle_proofs = Array.(split (of_list accu)) in
        M.return {shuffle_ciphertexts; shuffle_proofs}
      )
    in
    loop (Array.length cc - 1) []

  let check_shuffle cc s =
    Array.forall3 (Mix.check_shuffle_proof y) cc s.shuffle_ciphertexts s.shuffle_proofs

  type factor = elt partial_decryption

  let eg_factor x {alpha; _} =
    let zkp = "decrypt|" ^ G.to_string (g **~ x) ^ "|" in
    alpha **~ x,
    fs_prove [| g; alpha |] x (hash zkp)

  let check_ciphertext c =
    Shape.forall (fun {alpha; beta} -> G.check alpha && G.check beta) c

  let rec swaps = function
    | SAtomic x -> let* x = x in M.return (SAtomic x)
    | SArray x ->
       let rec loop i accu =
         if i >= 0
         then let* x = swaps x.(i) in loop (pred i) (x::accu)
         else M.return (SArray (Array.of_list accu))
       in
       loop (pred (Array.length x)) []

  let compute_factor c x =
    if check_ciphertext c then (
      let res = Shape.map (eg_factor x) c in
      let decryption_factors, decryption_proofs = Shape.split res in
      let* decryption_proofs = swaps decryption_proofs in
      M.return {decryption_factors; decryption_proofs}
    ) else (
      M.fail (Invalid_argument "Invalid ciphertext")
    )

  let check_factor c y f =
    let zkp = "decrypt|" ^ G.to_string y ^ "|" in
    Shape.forall3 (fun {alpha; _} f {challenge; response} ->
        check_modulo q challenge &&
          check_modulo q response &&
            let commitments =
              [|
                g **~ response / (y **~ challenge);
                alpha **~ response / (f **~ challenge);
              |]
            in Z.(hash zkp commitments =% challenge)
      ) c f.decryption_factors f.decryption_proofs

  type result_type = W.result
  type result = (result_type, elt encrypted_tally, elt partial_decryption, elt shuffle) Serializable_t.election_result

  module Combinator = Trustees.MakeCombinator (G)

  let compute_result ?shuffles ?shufflers num_tallied encrypted_tally partial_decryptions trustees =
    let check = check_factor encrypted_tally in
    match Combinator.combine_factors trustees check partial_decryptions with
    | Ok factors ->
       let results =
         Shape.map2 (fun {beta; _} f ->
             beta / f
           ) encrypted_tally factors
       in
       let raw_result =
         match results with
         | SAtomic _ ->
            invalid_arg "Election.compute_result: cannot compute result"
         | SArray xs ->
            Array.map2 (Q.compute_result ~num_tallied) election.e_questions xs
       in
       let result = W.cast_result raw_result in
       Ok {num_tallied; encrypted_tally; shuffles; shufflers; partial_decryptions; result}
    | Error e -> Error e

  let check_result trustees r =
    let {num_tallied; encrypted_tally; partial_decryptions; result; _} = r in
    check_ciphertext encrypted_tally &&
      let check = check_factor encrypted_tally in
      match Combinator.combine_factors trustees check partial_decryptions with
      | Error _ -> false
      | Ok factors ->
         let results =
           Shape.map2 (fun {beta; _} f ->
               beta / f
             ) encrypted_tally factors
         in
         match results with
         | SArray xs ->
            Array.forall3 (Q.check_result ~num_tallied) election.e_questions xs (result : W.result :> raw_result)
         | _ -> false
end

module Make (MakeResult : MAKE_RESULT) (R : RAW_ELECTION) (M : RANDOM) () = struct
  module X = Parse (R) ()
  module Y = struct
    include X
    include MakeResult (X)
  end
  include Y
  type 'a m = 'a M.t
  module E = MakeElection (Y) (M)
end
