(**************************************************************************)
(*                                BELENIOS                                *)
(*                                                                        *)
(*  Copyright © 2012-2016 Inria                                           *)
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
open Serializable_t
open Signatures
open Common

(** Helper functions *)

let check_modulo p x = Z.(geq x zero && lt x p)

(** Simple monad *)

module MakeSimpleMonad (G : GROUP) = struct
  type 'a t = unit -> 'a
  let ballots = ref []
  let return x () = x
  let bind x f = f (x ())
  let fail e = raise e

  let prng = lazy (pseudo_rng (random_string secure_rng 16))

  let random q =
    let size = Z.bit_length q / 8 + 1 in
    fun () ->
      let r = random_string (Lazy.force prng) size in
      Z.(of_bits r mod q)

  type elt = G.t ballot
  let cast x () = ballots := x :: !ballots
  let fold f x () = List.fold_left (fun accu b -> f () b accu ()) x !ballots
  let cardinal () = List.length !ballots
end

(** Distributed key generation *)

module MakeSimpleDistKeyGen (G : GROUP) (M : RANDOM) = struct
  open G
  open M

  let ( >>= ) = bind
  let ( / ) x y = x *~ invert y

  (** Fiat-Shamir non-interactive zero-knowledge proofs of
      knowledge *)

  let fs_prove gs x oracle =
    random q >>= fun w ->
    let commitments = Array.map (fun g -> g **~ w) gs in
    let challenge = oracle commitments in
    let response = Z.((w + x * challenge) mod q) in
    return {challenge; response}

  let generate_and_prove () =
    random q >>= fun x ->
    let trustee_public_key = g **~ x in
    let zkp = "pok|" ^ G.to_string trustee_public_key ^ "|" in
    fs_prove [| g |] x (G.hash zkp) >>= fun trustee_pok ->
    return (x, {trustee_pok; trustee_public_key})

  let check {trustee_pok; trustee_public_key = y} =
    G.check y &&
    let {challenge; response} = trustee_pok in
    check_modulo q challenge &&
    check_modulo q response &&
    let commitment = g **~ response / (y **~ challenge) in
    let zkp = "pok|" ^ G.to_string y ^ "|" in
    Z.(challenge =% G.hash zkp [| commitment |])

  let combine pks =
    Array.fold_left (fun y {trustee_public_key; _} ->
      y *~ trustee_public_key
    ) G.one pks

end

(** Homomorphic elections *)

module MakeElection (G : GROUP) (M : RANDOM) = struct
  open G

  type 'a m = 'a M.t
  open M
  let ( >>= ) = bind

  type elt = G.t

  type t = elt election
  type private_key = Z.t
  type public_key = elt

  let ( / ) x y = x *~ invert y

  type ciphertext = elt Serializable_t.ciphertext array array

  let dummy_ciphertext =
    {
      alpha = G.one;
      beta = G.one;
    }

  (** Multiply two ElGamal ciphertexts. *)
  let eg_combine c1 c2 =
    {
      alpha = c1.alpha *~ c2.alpha;
      beta = c1.beta *~ c2.beta;
    }

  let neutral_ciphertext e = Array.map (fun q ->
    Array.make (Array.length q.q_answers) dummy_ciphertext
  ) e.e_params.e_questions

  let combine_ciphertexts = Array.mmap2 eg_combine

  type plaintext = int array array
  type ballot = elt Serializable_t.ballot
  type randomness = Z.t array array

  (** ElGamal encryption. *)
  let eg_encrypt y r x =
    {
      alpha = g **~ r;
      beta = y **~ r *~ g **~ Z.of_int x;
    }

  let dummy_proof =
    {
      challenge = Z.zero;
      response = Z.zero;
    }

  (** Fiat-Shamir non-interactive zero-knowledge proofs of
      knowledge *)

  let fs_prove gs x oracle =
    random q >>= fun w ->
    let commitments = Array.map (fun g -> g **~ w) gs in
    let challenge = oracle commitments in
    let response = Z.((w + x * challenge) mod q) in
    return {challenge; response}

  (** ZKPs for disjunctions *)

  let eg_disj_prove y d zkp x r {alpha; beta} =
    (* prove that alpha = g^r and beta = y^r/d_x *)
    (* the size of d is the number of disjuncts *)
    let n = Array.length d in
    assert (0 <= x && x < n);
    let proofs = Array.make n dummy_proof
    and commitments = Array.make (2*n) g
    and total_challenges = ref Z.zero in
    (* compute fake proofs *)
    let f i =
      let challenge = random q
      and response = random q in
      challenge >>= fun challenge ->
      response >>= fun response ->
      proofs.(i) <- {challenge; response};
      commitments.(2*i) <- g **~ response / alpha **~ challenge;
      commitments.(2*i+1) <- y **~ response / (beta *~ d.(i)) **~ challenge;
      total_challenges := Z.(!total_challenges + challenge);
      return ()
    in
    let rec loop i =
      if i < x then f i >>= fun () -> loop (succ i)
      else if i = x then loop (succ i)
      else if i < n then f i >>= fun () -> loop (succ i)
      else return ()
    in loop 0 >>= fun () ->
    total_challenges := Z.(q - !total_challenges mod q);
    (* compute genuine proof *)
    fs_prove [| g; y |] r (fun commitx ->
      Array.blit commitx 0 commitments (2*x) 2;
      let prefix = Printf.sprintf "prove|%s|%s,%s|"
        zkp (G.to_string alpha) (G.to_string beta)
      in
      Z.((G.hash prefix commitments + !total_challenges) mod q)
    ) >>= fun p ->
    proofs.(x) <- p;
    return proofs

  let eg_disj_verify y d zkp proofs {alpha; beta} =
    G.check alpha && G.check beta &&
    let n = Array.length d in
    n = Array.length proofs &&
    let commitments = Array.make (2*n) g
    and total_challenges = ref Z.zero in
    try
      for i = 0 to n-1 do
        let {challenge; response} = proofs.(i) in
        if check_modulo q challenge && check_modulo q response then (
          commitments.(2*i) <- g **~ response / alpha **~ challenge;
          commitments.(2*i+1) <- y **~ response / (beta *~ d.(i)) **~ challenge;
          total_challenges := Z.(!total_challenges + challenge);
        ) else raise Exit
      done;
      total_challenges := Z.(!total_challenges mod q);
      let prefix = Printf.sprintf "prove|%s|%s,%s|"
        zkp (G.to_string alpha) (G.to_string beta)
      in
      Z.(hash prefix commitments =% !total_challenges)
    with Exit -> false

  (** Ballot creation *)

  let invg = invert g
  let d01 = [| G.one; invg |]

  let make_d min max =
    let n = max - min + 1 in
    let d = Array.make n (invert (g **~ Z.of_int min)) in
    for i = 1 to n-1 do
      d.(i) <- d.(i-1) *~ invg
    done;
    d

  let swap xs =
    let rec loop i accu =
      if i >= 0
      then xs.(i) >>= fun x -> loop (pred i) (x::accu)
      else return (Array.of_list accu)
    in loop (pred (Array.length xs)) []

  let sswap xs =
    let rec loop_outer i accu =
      if i >= 0 then (
        let x = xs.(i) in
        let rec loop_inner j accu =
          if j >= 0
          then x.(j) >>= fun r -> loop_inner (pred j) (r::accu)
          else return (Array.of_list accu)
        in
        loop_inner (Array.length x - 1) [] >>= fun ys ->
        loop_outer (pred i) (ys::accu)
      ) else return (Array.of_list accu)
    in loop_outer (Array.length xs - 1) []

  let create_answer y zkp q r m =
    let choices = Array.map2 (eg_encrypt y) r m in
    let individual_proofs = Array.map3 (eg_disj_prove y d01 zkp) m r choices in
    (* create overall_proof from homomorphic combination of individual
       weights *)
    let sumr = Array.fold_left Z.(+) Z.zero r in
    let summ = Array.fold_left (+) 0 m in
    let sumc = Array.fold_left eg_combine dummy_ciphertext choices in
    assert (q.q_min <= summ && summ <= q.q_max);
    let d = make_d q.q_min q.q_max in
    let overall_proof = eg_disj_prove y d zkp (summ - q.q_min) sumr sumc in
    swap individual_proofs >>= fun individual_proofs ->
    overall_proof >>= fun overall_proof ->
    return {choices; individual_proofs; overall_proof}

  let make_randomness e =
    sswap (Array.map (fun q ->
      Array.init (Array.length q.q_answers) (fun _ -> random G.q)
    ) e.e_params.e_questions)

  let make_sig_prefix zkp commitment =
    "sig|" ^ zkp ^ "|" ^ G.to_string commitment ^ "|"

  let make_sig_contents answers =
    List.flatten (
      List.map (fun a ->
        List.flatten (
          List.map (fun {alpha; beta} ->
            [alpha; beta]
          ) (Array.to_list a.choices)
        )
      ) (Array.to_list answers)
    ) |> Array.of_list

  let create_ballot e ?sk r m =
    let p = e.e_params in
    let sk, zkp =
      match sk with
      | None -> None, ""
      | Some x -> let y = G.(g **~ x) in Some (x, y), G.to_string y
    in
    swap (Array.map3 (create_answer p.e_public_key zkp) p.e_questions r m) >>= fun answers ->
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
      election_hash = e.e_fingerprint;
      election_uuid = p.e_uuid;
      signature;
    }

  (** Ballot verification *)

  let verify_answer y zkp q a =
    Array.forall2 (eg_disj_verify y d01 zkp) a.individual_proofs a.choices &&
    let sumc = Array.fold_left eg_combine dummy_ciphertext a.choices in
    let d = make_d q.q_min q.q_max in
    eg_disj_verify y d zkp a.overall_proof sumc

  let check_ballot e b =
    let p = e.e_params in
    b.election_uuid = p.e_uuid &&
    b.election_hash = e.e_fingerprint &&
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

  let extract_ciphertext b = Array.map (fun x -> x.choices) b.answers

  type factor = elt partial_decryption

  let eg_factor x {alpha; _} =
    let zkp = "decrypt|" ^ G.to_string (g **~ x) ^ "|" in
    alpha **~ x,
    fs_prove [| g; alpha |] x (hash zkp)

  let check_ciphertext c =
    Array.fforall (fun {alpha; beta} -> G.check alpha && G.check beta) c

  let compute_factor c x =
    if check_ciphertext c then (
      let res = Array.mmap (eg_factor x) c in
      let decryption_factors, decryption_proofs = Array.ssplit res in
      sswap decryption_proofs >>= fun decryption_proofs ->
      return {decryption_factors; decryption_proofs}
    ) else (
      fail (Invalid_argument "Invalid ciphertext")
    )

  let check_factor c y f =
    let zkp = "decrypt|" ^ G.to_string y ^ "|" in
    Array.fforall3 (fun {alpha; _} f {challenge; response} ->
      check_modulo q challenge &&
      check_modulo q response &&
      let commitments =
        [|
          g **~ response / (y **~ challenge);
          alpha **~ response / (f **~ challenge);
        |]
      in Z.(hash zkp commitments =% challenge)
    ) c f.decryption_factors f.decryption_proofs

  type result = elt Serializable_t.result

  let combine_factors num_tallied encrypted_tally partial_decryptions =
    let dummy = Array.mmap (fun _ -> G.one) encrypted_tally in
    let factors = Array.fold_left (fun a b ->
      Array.mmap2 ( *~ ) a b.decryption_factors
    ) dummy partial_decryptions in
    let results = Array.mmap2 (fun {beta; _} f ->
      beta / f
    ) encrypted_tally factors in
    let log =
      let module GMap = Map.Make(G) in
      let rec loop i cur accu =
        if i <= num_tallied
        then loop (succ i) (cur *~ g) (GMap.add cur i accu)
        else accu
      in
      let map = loop 0 G.one GMap.empty in
      fun x ->
        try
          GMap.find x map
        with Not_found ->
          invalid_arg "Cannot compute result"
    in
    let result = Array.mmap log results in
    {num_tallied; encrypted_tally; partial_decryptions; result}

  let check_result pks r =
    let {encrypted_tally; partial_decryptions; result; _} = r in
    check_ciphertext encrypted_tally &&
    Array.forall2 (check_factor encrypted_tally) pks partial_decryptions &&
    let dummy = Array.mmap (fun _ -> G.one) encrypted_tally in
    let factors = Array.fold_left (fun a b ->
      Array.mmap2 ( *~ ) a b.decryption_factors
    ) dummy partial_decryptions in
    let results = Array.mmap2 (fun {beta; _} f ->
      beta / f
    ) encrypted_tally factors in
    Array.fforall2 (fun r1 r2 -> r1 =~ g **~ Z.of_int r2) results result

  let extract_tally r = r.result
end
