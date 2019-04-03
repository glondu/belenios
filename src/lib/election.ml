(**************************************************************************)
(*                                BELENIOS                                *)
(*                                                                        *)
(*  Copyright © 2012-2018 Inria                                           *)
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

open Platform
open Serializable_builtin_t
open Serializable_core_j
open Serializable_j
open Signatures
open Common

(** Parsing helpers *)

let of_string x =
  let params = params_of_string Yojson.Safe.read_json x in
  {
    e_params = params;
    e_fingerprint = sha256_b64 x;
  }

let get_group x =
  let w = Yojson.Safe.to_string x.e_params.e_public_key in
  let module W = (val Group.wrapped_pubkey_of_string w) in
  let module X =
    struct
      module G = W.G
      let election = {x with e_params = {x.e_params with e_public_key = W.y}}
    end
  in (module X : ELECTION_DATA)

(** Helper functions *)

let check_modulo p x = Z.(geq x zero && lt x p)

(** Homomorphic elections *)

module Make (W : ELECTION_DATA) (M : RANDOM) = struct
  type 'a m = 'a M.t
  open M
  let ( >>= ) = bind

  type elt = W.G.t

  module G = W.G
  module Q = Question.Make (M) (G)
  module Mix = Mixnet.Make (M) (G)
  open G
  let election = W.election

  type private_key = Z.t
  type public_key = elt

  let ( / ) x y = x *~ invert y

  type plaintext = int array array
  type ballot = elt Serializable_t.ballot

  (** Fiat-Shamir non-interactive zero-knowledge proofs of
      knowledge *)

  let fs_prove gs x oracle =
    random q >>= fun w ->
    let commitments = Array.map (fun g -> g **~ w) gs in
    let challenge = oracle commitments in
    let response = Z.((w + x * challenge) mod q) in
    return {challenge; response}

  (** Ballot creation *)

  let swap xs =
    let rec loop i accu =
      if i >= 0
      then xs.(i) >>= fun x -> loop (pred i) (x::accu)
      else return (Array.of_list accu)
    in loop (pred (Array.length xs)) []

  let create_answer y zkp q m =
    Q.create_answer q ~public_key:y ~prefix:zkp m

  let make_sig_prefix zkp commitment =
    "sig|" ^ zkp ^ "|" ^ G.to_string commitment ^ "|"

  let make_sig_contents answers =
    Array.map2 Q.extract_ciphertexts election.e_params.e_questions answers
    |> Array.map Shape.flatten
    |> Array.to_list
    |> List.flatten
    |> List.map (fun {alpha; beta} -> [alpha; beta])
    |> List.flatten
    |> Array.of_list

  let create_ballot ?sk m =
    let p = election.e_params in
    let sk, zkp =
      match sk with
      | None -> None, ""
      | Some x -> let y = G.(g **~ x) in Some (x, y), G.to_string y
    in
    swap (Array.map2 (create_answer p.e_public_key zkp) p.e_questions m) >>= fun answers ->
    (
      match sk with
      | None -> return None
      | Some (x, y) ->
        random q >>= fun w ->
        let commitment = g **~ w in
        let prefix = make_sig_prefix zkp commitment in
        let contents = make_sig_contents answers in
        let s_challenge = G.hash prefix contents in
        let s_response = Z.(erem (w - x * s_challenge) q) in
        return (Some {s_public_key = y; s_challenge; s_response})
    ) >>= fun signature ->
    return {
      answers;
      election_hash = election.e_fingerprint;
      election_uuid = p.e_uuid;
      signature;
    }

  (** Ballot verification *)

  let verify_answer y zkp q a =
    Q.verify_answer q ~public_key:y ~prefix:zkp a

  let check_ballot b =
    let p = election.e_params in
    b.election_uuid = p.e_uuid &&
    b.election_hash = election.e_fingerprint &&
    let ok, zkp = match b.signature with
      | Some {s_public_key = y; s_challenge; s_response} ->
        let zkp = G.to_string y in
        let ok =
          check_modulo q s_challenge &&
          check_modulo q s_response &&
          let commitment = g **~ s_response *~ y **~ s_challenge in
          let prefix = make_sig_prefix zkp commitment in
          let contents = make_sig_contents b.answers in
          Z.(s_challenge =% G.hash prefix contents)
        in ok, zkp
      | None -> true, ""
    in ok &&
    Array.forall2 (verify_answer p.e_public_key zkp) p.e_questions b.answers

  let process_ballots bs =
    SArray (
        Array.mapi (fun i q ->
            Q.process_ciphertexts q (Array.map (fun b -> Q.extract_ciphertexts q b.answers.(i)) bs)
          ) election.e_params.e_questions
      )

  let extract_nh_ciphertexts x =
    let x = Shape.to_shape_array x in
    let rec loop i accu =
      if i >= 0 then (
        match election.e_params.e_questions.(i) with
        | Question.Standard _ -> loop (i-1) accu
        | Question.Open _ -> loop (i-1) (Shape.to_array x.(i) :: accu)
      ) else Array.of_list accu
    in
    loop (Array.length x - 1) []

  let merge_nh_ciphertexts cc x =
    let x = Array.copy (Shape.to_shape_array x) in
    let n = Array.length x and m = Array.length cc in
    let rec loop i j =
      if i < n then (
        assert (j < m);
        match election.e_params.e_questions.(i) with
        | Question.Standard _ -> loop (i+1) j
        | Question.Open _ ->
           x.(i) <- Shape.of_array cc.(j);
           loop (i+1) (j+1)
      ) else (
        assert (j = m);
        SArray x
      )
    in
    loop 0 0

  let shuffle_ciphertexts cc =
    let y = election.e_params.e_public_key in
    let rec loop i accu =
      if i >= 0 then (
        let c = cc.(i) in
        Mix.gen_shuffle y c >>= fun (c', r', psi) ->
        Mix.gen_shuffle_proof y c c' r' psi >>= fun pi ->
        loop (i-1) ((c', pi) :: accu)
      ) else
        M.return Array.(split (of_list accu))
    in
    loop (Array.length cc - 1) []

  let check_shuffle cc cc' p =
    let y = election.e_params.e_public_key in
    Array.forall3 (Mix.check_shuffle_proof y) cc cc' p

  type factor = elt partial_decryption

  let eg_factor x {alpha; _} =
    let zkp = "decrypt|" ^ G.to_string (g **~ x) ^ "|" in
    alpha **~ x,
    fs_prove [| g; alpha |] x (hash zkp)

  let check_ciphertext c =
    Shape.forall (fun {alpha; beta} -> G.check alpha && G.check beta) c

  let rec swaps = function
    | SAtomic x -> x >>= fun x -> return (SAtomic x)
    | SArray x ->
       let rec loop i accu =
         if i >= 0
         then swaps x.(i) >>= fun x -> loop (pred i) (x::accu)
         else return (SArray (Array.of_list accu))
       in
       loop (pred (Array.length x)) []

  let compute_factor c x =
    if check_ciphertext c then (
      let res = Shape.map (eg_factor x) c in
      let decryption_factors, decryption_proofs = Shape.split res in
      swaps decryption_proofs >>= fun decryption_proofs ->
      return {decryption_factors; decryption_proofs}
    ) else (
      fail (Invalid_argument "Invalid ciphertext")
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

  type result = elt Serializable_t.election_result

  type combinator = factor list -> elt shape

  let compute_result num_tallied encrypted_tally partial_decryptions combinator =
    let factors = combinator partial_decryptions in
    let results = Shape.map2 (fun {beta; _} f ->
      beta / f
    ) encrypted_tally factors in
    let result =
      match results with
      | SAtomic _ ->
         invalid_arg "Election.compute_result: cannot compute result"
      | SArray xs ->
         SArray (Array.map2 (Q.compute_result ~num_tallied) election.e_params.e_questions xs)
    in
    {num_tallied; encrypted_tally; partial_decryptions; result}

  let check_result combinator r =
    let {encrypted_tally; partial_decryptions; result; _} = r in
    check_ciphertext encrypted_tally &&
    let factors = combinator partial_decryptions in
    let results = Shape.map2 (fun {beta; _} f ->
      beta / f
    ) encrypted_tally factors in
    match results, result with
    | SArray xs, SArray rs ->
       Array.forall3 Q.check_result election.e_params.e_questions xs rs
    | _, _ -> false

  let extract_tally r = r.result
end
