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
open Platform
open Common
open Signatures_core
open Serializable_builtin_t
open Serializable_core_t
open Question_h_t

(** Helper functions *)

let question_length q =
  Array.length q.q_answers + match q.q_blank with
                             | Some true -> 1
                             | _ -> 0


module Make (M : RANDOM) (G : GROUP) = struct
  open G

  let ( let* ) = M.bind
  let ( / ) x y = x *~ invert y

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

  (** ElGamal encryption. *)
  let eg_encrypt y r x =
    {
      alpha = g **~ r;
      beta = (y **~ r) *~ (g **~ Z.of_int x);
    }

  let dummy_proof =
    {
      challenge = Z.zero;
      response = Z.zero;
    }

  (** Fiat-Shamir non-interactive zero-knowledge proofs of
      knowledge *)

  let fs_prove gs x oracle =
    let* w = M.random q in
    let commitments = Array.map (fun g -> g **~ w) gs in
    let* () = M.yield () in
    let challenge = oracle commitments in
    let response = Z.((w + x * challenge) mod q) in
    M.return {challenge; response}

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
      let* challenge = M.random q in
      let* response = M.random q in
      proofs.(i) <- {challenge; response};
      commitments.(2*i) <- g **~ response / alpha **~ challenge;
      let* () = M.yield () in
      commitments.(2*i+1) <- y **~ response / (beta *~ d.(i)) **~ challenge;
      let* () = M.yield () in
      total_challenges := Z.(!total_challenges + challenge);
      M.return ()
    in
    let rec loop i =
      if i < x then let* () = f i in loop (succ i)
      else if i = x then loop (succ i)
      else if i < n then let* () = f i in loop (succ i)
      else M.return ()
    in
    let* () = loop 0 in
    total_challenges := Z.(q - !total_challenges mod q);
    (* compute genuine proof *)
    let* p =
      fs_prove [| g; y |] r (fun commitx ->
          Array.blit commitx 0 commitments (2*x) 2;
          let prefix = Printf.sprintf "prove|%s|%s,%s|"
                         zkp (G.to_string alpha) (G.to_string beta)
          in
          Z.((G.hash prefix commitments + !total_challenges) mod q)
        )
    in
    proofs.(x) <- p;
    M.return proofs

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

  (** ZKPs for blank ballots *)

  let make_blank_proof y zkp min max m0 c0 r0 mS cS rS =
    if m0 = 0 then (
      let* blank_proof =
        (* proof of m0 = 0 \/ mS = 0 (first is true) *)
        let* challenge1 = M.random q in
        let* response1 = M.random q in
        let commitmentA1 = g **~ response1 *~ cS.alpha **~ challenge1 in
        let* () = M.yield () in
        let commitmentB1 = y **~ response1 *~ cS.beta **~ challenge1 in
        let* w = M.random q in
        let commitmentA0 = g **~ w and commitmentB0 = y **~ w in
        let* () = M.yield () in
        let prefix = Printf.sprintf "bproof0|%s|" zkp in
        let h = G.hash prefix [|commitmentA0; commitmentB0; commitmentA1; commitmentB1|] in
        let challenge0 = Z.(erem (h - challenge1) q) in
        let response0 = Z.(erem (w - r0 * challenge0) q) in
        M.return [|
            {challenge=challenge0; response=response0};
            {challenge=challenge1; response=response1};
          |]
      in
      let* overall_proof =
        (* proof of m0 = 1 \/ min <= mS <= max (second is true) *)
        assert (min <= mS && mS <= max);
        let* challenge0 = M.random q in
        let* response0 = M.random q in
        let proof0 = {challenge=challenge0; response=response0} in
        let overall_proof = Array.make (max-min+2) proof0 in
        let commitments = Array.make (2*(max-min+2)) g in
        let total_challenges = ref challenge0 in
        commitments.(0) <- g **~ response0 *~ c0.alpha **~ challenge0;
        let* () = M.yield () in
        commitments.(1) <- y **~ response0 *~ (c0.beta / g) **~ challenge0;
        let* () = M.yield () in
        let index_true = mS-min+1 in
        let rec loop i =
          if i < max-min+2 then (
            if i <> index_true then (
              let* challenge = M.random q in
              let* response = M.random q in
              let g' = if min+i-1 = 0 then G.one else g **~ Z.of_int (min+i-1) in
              let nbeta = cS.beta / g' in
              let j = 2*i in
              overall_proof.(i) <- {challenge; response};
              commitments.(j) <- g **~ response *~ cS.alpha **~ challenge;
              let* () = M.yield () in
              commitments.(j+1) <- y **~ response *~ nbeta **~ challenge;
              let* () = M.yield () in
              total_challenges := Z.(!total_challenges + challenge);
              loop (i+1)
            ) else loop (i+1)
          ) else M.return ()
        in
        let* () = loop 1 in
        let* w = M.random q in
        let j = 2 * index_true in
        commitments.(j) <- g **~ w;
        commitments.(j+1) <- y **~ w;
        let* () = M.yield () in
        let prefix = Printf.sprintf "bproof1|%s|" zkp in
        let h = G.hash prefix commitments in
        let challenge = Z.(erem (h - !total_challenges) q) in
        let response = Z.(erem (w - rS * challenge) q) in
        overall_proof.(index_true) <- {challenge; response};
        M.return overall_proof
      in
      M.return (overall_proof, blank_proof)
    ) else (
      let* blank_proof =
        (* proof of m0 = 0 \/ mS = 0 (second is true) *)
        assert (mS = 0);
        let* challenge0 = M.random q in
        let* response0 = M.random q in
        let commitmentA0 = g **~ response0 *~ c0.alpha **~ challenge0 in
        let* () = M.yield () in
        let commitmentB0 = y **~ response0 *~ c0.beta **~ challenge0 in
        let* w = M.random q in
        let commitmentA1 = g **~ w and commitmentB1 = y **~ w in
        let* () = M.yield () in
        let prefix = Printf.sprintf "bproof0|%s|" zkp in
        let h = G.hash prefix [|commitmentA0; commitmentB0; commitmentA1; commitmentB1|] in
        let challenge1 = Z.(erem (h - challenge0) q) in
        let response1 = Z.(erem (w - rS * challenge1) q) in
        M.return [|
            {challenge=challenge0; response=response0};
            {challenge=challenge1; response=response1}
          |]
      in
      let* overall_proof =
        (* proof of m0 = 1 \/ min <= mS <= max (first is true) *)
        assert (m0 = 1);
        let nil_proof = {challenge=Z.zero; response=Z.zero} in
        let overall_proof = Array.make (max-min+2) nil_proof in
        let commitments = Array.make (2*(max-min+2)) g in
        let total_challenges = ref Z.zero in
        let rec loop i =
          if i < max-min+2 then (
            let* challenge = M.random q in
            let* response = M.random q in
            let g' = if min+i-1 = 0 then G.one else g **~ Z.of_int (min+i-1) in
            let nbeta = cS.beta / g' in
            let j = 2*i in
            overall_proof.(i) <- {challenge; response};
            commitments.(j) <- g **~ response *~ cS.alpha **~ challenge;
            let* () = M.yield () in
            commitments.(j+1) <- y **~ response *~ nbeta **~ challenge;
            let* () = M.yield () in
            total_challenges := Z.(!total_challenges + challenge);
            loop (i+1)
          ) else M.return ()
        in
        let* () = loop 1 in
        let* w = M.random q in
        commitments.(0) <- g **~ w;
        commitments.(1) <- y **~ w;
        let* () = M.yield () in
        let prefix = Printf.sprintf "bproof1|%s|" zkp in
        let h = G.hash prefix commitments in
        let challenge = Z.(erem (h - !total_challenges) q) in
        let response = Z.(erem (w - r0 * challenge) q) in
        overall_proof.(0) <- {challenge; response};
        M.return overall_proof
      in
      M.return (overall_proof, blank_proof)
    )

  let verify_blank_proof y zkp min max c0 cS overall_proof blank_proof =
    G.check c0.alpha && G.check c0.beta &&
      G.check cS.alpha && G.check cS.beta &&
        (* check blank_proof, proof of m0 = 0 \/ mS = 0 *)
        Array.length blank_proof = 2 &&
          (
            try
              let commitments = Array.make 4 g in
              let total_challenges = ref Z.zero in
              let {challenge; response} = blank_proof.(0) in
              if not (check_modulo q challenge && check_modulo q response) then
                raise Exit;
              commitments.(0) <- g **~ response *~ c0.alpha **~ challenge;
              commitments.(1) <- y **~ response *~ c0.beta **~ challenge;
              total_challenges := Z.(!total_challenges + challenge);
              let {challenge; response} = blank_proof.(1) in
              if not (check_modulo q challenge && check_modulo q response) then
                raise Exit;
              commitments.(2) <- g **~ response *~ cS.alpha **~ challenge;
              commitments.(3) <- y **~ response *~ cS.beta **~ challenge;
              total_challenges := Z.(!total_challenges + challenge);
              let prefix = Printf.sprintf "bproof0|%s|" zkp in
              let h = G.hash prefix commitments in
              let total_challenges = Z.(!total_challenges mod q) in
              Z.(h =% total_challenges)
            with Exit -> false
          ) &&
            (* check overall_proof, proof of m0 = 1 \/ min <= mS <= max *)
            Array.length overall_proof = max-min+2 &&
              (
                try
                  let commitments = Array.make (2*(max-min+2)) g in
                  let total_challenges = ref Z.zero in
                  let {challenge; response} = overall_proof.(0) in
                  if not (check_modulo q challenge && check_modulo q response) then
                    raise Exit;
                  commitments.(0) <- g **~ response *~ c0.alpha **~ challenge;
                  commitments.(1) <- y **~ response *~ (c0.beta / g) **~ challenge;
                  total_challenges := Z.(!total_challenges + challenge);
                  let rec loop i =
                    if i < max-min+2 then (
                      let {challenge; response} = overall_proof.(i) in
                      if not (check_modulo q challenge && check_modulo q response) then
                        raise Exit;
                      let g' = if min+i-1 = 0 then G.one else g **~ Z.of_int (min+i-1) in
                      let nbeta = cS.beta / g' in
                      let j = 2*i in
                      commitments.(j) <- g **~ response *~ cS.alpha **~ challenge;
                      commitments.(j+1) <- y **~ response *~ nbeta **~ challenge;
                      total_challenges := Z.(!total_challenges + challenge);
                      loop (i+1)
                    ) else ()
                  in
                  loop 1;
                  let prefix = Printf.sprintf "bproof1|%s|" zkp in
                  let h = G.hash prefix commitments in
                  let total_challenges = Z.(!total_challenges mod q) in
                  Z.(h =% total_challenges)
                with Exit -> false
              )

  let invg = invert g
  let d01 = [| G.one; invg |]

  let make_d min max =
    let n = max - min + 1 in
    let g' = if min = 0 then G.one else g **~ Z.of_int min in
    let d = Array.make n (invert g') in
    for i = 1 to n-1 do
      d.(i) <- d.(i-1) *~ invg
    done;
    d

  let swap xs =
    let rec loop i accu =
      if i >= 0
      then let* x = xs.(i) in loop (pred i) (x::accu)
      else M.return (Array.of_list accu)
    in loop (pred (Array.length xs)) []

  let stringify_choices choices =
    choices
    |> Array.map
         (fun {alpha; beta} ->
           Printf.sprintf "%s,%s" (G.to_string alpha) (G.to_string beta)
         )
    |> Array.to_list
    |> String.concat ","

  let create_answer q ~public_key:y ~prefix:zkp m =
    let n = Array.length m in
    let* r = swap (Array.init n (fun _ -> M.random G.q)) in
    let choices = Array.map2 (eg_encrypt y) r m in
    let* () = M.yield () in
    let individual_proofs = Array.map3 (eg_disj_prove y d01 zkp) m r choices in
    let* () = M.yield () in
    let* individual_proofs = swap individual_proofs in
    let zkp = zkp ^ "|" ^ stringify_choices choices in
    match q.q_blank with
    | Some true ->
       (* index 0 is whether the ballot is blank or not,
          indexes 1..n-1 are the actual choices *)
       assert (n = Array.length q.q_answers + 1);
       let choices' = Array.sub choices 1 (n - 1) in
       let r' = Array.sub r 1 (n - 1) in
       let m' = Array.sub m 1 (n - 1) in
       let sumr = Array.fold_left Z.(+) Z.zero r' in
       let summ = Array.fold_left (+) 0 m' in
       let sumc = Array.fold_left eg_combine dummy_ciphertext choices' in
       let* (overall_proof, blank_proof) =
         make_blank_proof y zkp q.q_min q.q_max
           m.(0) choices.(0) r.(0) summ sumc sumr
       in
       let blank_proof = Some blank_proof in
       M.return {choices; individual_proofs; overall_proof; blank_proof}
    | _ ->
       (* indexes 0..n-1 are the actual choices *)
       assert (n = Array.length q.q_answers);
       let sumr = Array.fold_left Z.(+) Z.zero r in
       let summ = Array.fold_left (+) 0 m in
       let sumc = Array.fold_left eg_combine dummy_ciphertext choices in
       assert (q.q_min <= summ && summ <= q.q_max);
       let d = make_d q.q_min q.q_max in
       let* overall_proof = eg_disj_prove y d zkp (summ - q.q_min) sumr sumc in
       let blank_proof = None in
       M.return {choices; individual_proofs; overall_proof; blank_proof}

  let verify_answer q ~public_key:y ~prefix:zkp a =
    let n = Array.length a.choices in
    n = Array.length a.individual_proofs &&
      Array.forall2 (eg_disj_verify y d01 zkp) a.individual_proofs a.choices &&
        let zkp = zkp ^ "|" ^ stringify_choices a.choices in
        match q.q_blank, a.blank_proof with
        | Some true, Some blank_proof ->
           n = Array.length q.q_answers + 1 &&
             let c = Array.sub a.choices 1 (n - 1) in
             let sumc = Array.fold_left eg_combine dummy_ciphertext c in
             verify_blank_proof y zkp q.q_min q.q_max a.choices.(0) sumc a.overall_proof blank_proof
        | _, None ->
           n = Array.length q.q_answers &&
             let sumc = Array.fold_left eg_combine dummy_ciphertext a.choices in
             let d = make_d q.q_min q.q_max in
             eg_disj_verify y d zkp a.overall_proof sumc
        | _, _ -> false

  let extract_ciphertexts _ a =
    SArray (Array.map (fun x -> SAtomic x) a.choices)

  let process_ciphertexts q es =
    let neutral = SArray (Array.make (question_length q) (SAtomic dummy_ciphertext)) in
    let ( * ) = Shape.map2 eg_combine in
    let rec power b n =
      if Z.(compare n zero) > 0 then (
        let x = power b Z.(shift_right n 1) in
        (if Z.(compare (logand n one) one) = 0 then b else neutral) * x * x
      ) else neutral
    in
    let total =
      let open Weight in
      Array.fold_left (fun a (w, _) -> a + w) zero es
    in
    let es = Array.map (fun (w, b) -> power b (Weight.expand ~total w)) es in
    Array.fold_left (Shape.map2 eg_combine) neutral es

  let compute_result ~num_tallied:total =
    let num_tallied = Weight.expand ~total total in
    let log =
      let module X = BabyStepGiantStep (G) in
      let log = X.log ~generator:G.g ~max:num_tallied in
      fun x ->
      match log x with
      | Some x -> x
      | None -> invalid_arg "Cannot compute result"
    in
    fun _ x ->
    Shape.to_array x
    |> Array.map (fun i -> Weight.reduce ~total (log i))
    |> (fun x -> RHomomorphic x)

  let check_result ~num_tallied _ x r =
    match r with
    | RHomomorphic r ->
       Array.forall2
         (fun x r ->
           let r = Weight.expand ~total:num_tallied r in
           let g' = if Z.compare r Z.zero = 0 then G.one else g **~ r in
           x =~ g'
         ) (Shape.to_array x) r
    | _ -> false

end
