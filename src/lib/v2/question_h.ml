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
open Belenios_core.Question
module Q = Homomorphic
open Q

let cast (Q x : Belenios_core.Question.t) : question option =
  let module Q = (val x.type_) in
  match Q.Id with Id -> Some x.value | _ -> None

(** Helper functions *)

let question_length q =
  Array.length q.answers + match q.blank with true -> 1 | false -> 0

let get_complexity q =
  let allowBlank = q.blank in
  let nb_ciphertexts = Array.length q.answers + if allowBlank then 1 else 0 in
  let nb_extra_zkps = q.max - q.min + 1 + if allowBlank then 3 else 0 in
  let nb_zkps = (2 * nb_ciphertexts) + nb_extra_zkps in
  { nb_ciphertexts; nb_zkps }

module Make (G : GROUP) = struct
  module G = G
  open G

  (** Serializers *)

  let question_of_yojson = question_of_yojson
  let yojson_of_question = yojson_of_question
  let answer_of_yojson = [%group_of_yojson: _ answer]
  let yojson_of_answer = [%yojson_of_group: _ answer]
  let result_of_yojson = result_of_yojson
  let yojson_of_result = yojson_of_result

  (** Implementation *)

  let random () = Zq.random (Crypto_primitives.get_rng ())
  let ( / ) x y = x *~ invert y
  let dummy_ciphertext : _ ciphertext = { alpha = G.one; beta = G.one }

  (** Multiply two ElGamal ciphertexts. *)
  let eg_combine (c1 : _ ciphertext) (c2 : _ ciphertext) : _ ciphertext =
    { alpha = c1.alpha *~ c2.alpha; beta = c1.beta *~ c2.beta }

  (** ElGamal encryption. *)
  let eg_encrypt y r x : _ ciphertext =
    { alpha = g **~ r; beta = (y **~ r) *~ (g **~ Zq.of_int x) }

  let dummy_proof : _ proof = { challenge = Zq.zero; response = Zq.zero }

  (** Fiat-Shamir non-interactive zero-knowledge proofs of knowledge *)

  let fs_prove gs x oracle : _ proof =
    let w = random () in
    let commitments = Array.map (fun g -> g **~ w) gs in
    let challenge = oracle commitments in
    let response = Zq.(w - (x * challenge)) in
    { challenge; response }

  (** ZKPs for disjunctions *)

  let eg_disj_prove y d zkp x r ({ alpha; beta } : _ ciphertext) =
    (* prove that alpha = g^r and beta = y^r/d_x *)
    (* the size of d is the number of disjuncts *)
    let n = Array.length d in
    assert (0 <= x && x < n);
    let proofs = Array.make n dummy_proof
    and commitments = Array.make (2 * n) g
    and total_challenges = ref Zq.zero in
    (* compute fake proofs *)
    let f i =
      let challenge = random () in
      let response = random () in
      proofs.(i) <- { challenge; response };
      commitments.(2 * i) <- (g **~ Zq.(response + (r * challenge)));
      commitments.((2 * i) + 1) <-
        (y **~ response) *~ ((beta *~ d.(i)) **~ challenge);
      total_challenges := Zq.(!total_challenges + challenge)
    in
    let rec loop i =
      if i < x then
        let () = f i in
        loop (succ i)
      else if i = x then loop (succ i)
      else if i < n then
        let () = f i in
        loop (succ i)
      else ()
    in
    let () = loop 0 in
    (* compute genuine proof *)
    let p =
      fs_prove [| g; y |] r (fun commitx ->
          Array.blit commitx 0 commitments (2 * x) 2;
          let dst = dst_prefix ^ "-prove" in
          let prefix =
            Printf.sprintf "%s|%s,%s|" zkp (G.to_string alpha)
              (G.to_string beta)
          in
          Zq.(G.hash ~dst prefix commitments - !total_challenges))
    in
    proofs.(x) <- p;
    proofs

  let eg_disj_verify y d zkp proofs ({ alpha; beta } : _ ciphertext) =
    G.check alpha && G.check beta
    &&
    let n = Array.length d in
    n = Array.length proofs
    &&
    let commitments = Array.make (2 * n) g and total_challenges = ref Zq.zero in
    for i = 0 to n - 1 do
      let { challenge; response } : _ proof = proofs.(i) in
      commitments.(2 * i) <- (g **~ response) *~ (alpha **~ challenge);
      commitments.((2 * i) + 1) <-
        (y **~ response) *~ ((beta *~ d.(i)) **~ challenge);
      total_challenges := Zq.(!total_challenges + challenge)
    done;
    let dst = dst_prefix ^ "-prove" in
    let prefix =
      Printf.sprintf "%s|%s,%s|" zkp (G.to_string alpha) (G.to_string beta)
    in
    Zq.(G.hash ~dst prefix commitments =% !total_challenges)

  (** ZKPs for blank ballots *)

  let make_blank_proof y zkp min max m0 (c0 : _ ciphertext) r0 mS
      (cS : _ ciphertext) rS =
    if m0 = 0 then
      let blank_proof : _ disjunctive_proof =
        (* proof of m0 = 0 \/ mS = 0 (first is true) *)
        let challenge1 = random () in
        let response1 = random () in
        let commitmentA1 = g **~ Zq.(response1 + (rS * challenge1)) in
        let commitmentB1 = (y **~ response1) *~ (cS.beta **~ challenge1) in
        let w = random () in
        let commitmentA0 = g **~ w and commitmentB0 = y **~ w in
        let dst = dst_prefix ^ "-bproof0" in
        let h =
          G.hash ~dst zkp
            [| commitmentA0; commitmentB0; commitmentA1; commitmentB1 |]
        in
        let challenge0 = Zq.(h - challenge1) in
        let response0 = Zq.(w - (r0 * challenge0)) in
        [|
          { challenge = challenge0; response = response0 };
          { challenge = challenge1; response = response1 };
        |]
      in
      let overall_proof =
        (* proof of m0 = 1 \/ min <= mS <= max (second is true) *)
        assert (min <= mS && mS <= max);
        let challenge0 = random () in
        let response0 = random () in
        let proof0 : _ proof =
          { challenge = challenge0; response = response0 }
        in
        let overall_proof = Array.make (max - min + 2) proof0 in
        let commitments = Array.make (2 * (max - min + 2)) g in
        let total_challenges = ref challenge0 in
        commitments.(0) <- (g **~ Zq.(response0 + (r0 * challenge0)));
        commitments.(1) <- (y **~ response0) *~ ((c0.beta / g) **~ challenge0);
        let index_true = mS - min + 1 in
        let rec loop i =
          if i < max - min + 2 then
            if i <> index_true then (
              let challenge = random () in
              let response = random () in
              let g' =
                if min + i - 1 = 0 then G.one else g **~ Zq.of_int (min + i - 1)
              in
              let nbeta = cS.beta / g' in
              let j = 2 * i in
              overall_proof.(i) <- { challenge; response };
              commitments.(j) <- (g **~ Zq.(response + (rS * challenge)));
              commitments.(j + 1) <- (y **~ response) *~ (nbeta **~ challenge);
              (total_challenges := Zq.(!total_challenges + challenge));
              loop (i + 1))
            else loop (i + 1)
          else ()
        in
        let () = loop 1 in
        let w = random () in
        let j = 2 * index_true in
        commitments.(j) <- g **~ w;
        commitments.(j + 1) <- y **~ w;
        let dst = dst_prefix ^ "-bproof1" in
        let h = G.hash ~dst zkp commitments in
        let challenge = Zq.(h - !total_challenges) in
        let response = Zq.(w - (rS * challenge)) in
        overall_proof.(index_true) <- { challenge; response };
        overall_proof
      in
      (overall_proof, blank_proof)
    else
      let blank_proof : _ disjunctive_proof =
        (* proof of m0 = 0 \/ mS = 0 (second is true) *)
        assert (mS = 0);
        let challenge0 = random () in
        let response0 = random () in
        let commitmentA0 = g **~ Zq.(response0 + (r0 * challenge0)) in
        let commitmentB0 = (y **~ response0) *~ (c0.beta **~ challenge0) in
        let w = random () in
        let commitmentA1 = g **~ w and commitmentB1 = y **~ w in
        let dst = dst_prefix ^ "-bproof0" in
        let h =
          G.hash ~dst zkp
            [| commitmentA0; commitmentB0; commitmentA1; commitmentB1 |]
        in
        let challenge1 = Zq.(h - challenge0) in
        let response1 = Zq.(w - (rS * challenge1)) in
        [|
          { challenge = challenge0; response = response0 };
          { challenge = challenge1; response = response1 };
        |]
      in
      let overall_proof =
        (* proof of m0 = 1 \/ min <= mS <= max (first is true) *)
        assert (m0 = 1);
        let overall_proof = Array.make (max - min + 2) dummy_proof in
        let commitments = Array.make (2 * (max - min + 2)) g in
        let total_challenges = ref Zq.zero in
        let rec loop i =
          if i < max - min + 2 then (
            let challenge = random () in
            let response = random () in
            let g' =
              if min + i - 1 = 0 then G.one else g **~ Zq.of_int (min + i - 1)
            in
            let nbeta = cS.beta / g' in
            let j = 2 * i in
            overall_proof.(i) <- { challenge; response };
            commitments.(j) <- (g **~ Zq.(response + (rS * challenge)));
            commitments.(j + 1) <- (y **~ response) *~ (nbeta **~ challenge);
            (total_challenges := Zq.(!total_challenges + challenge));
            loop (i + 1))
          else ()
        in
        let () = loop 1 in
        let w = random () in
        commitments.(0) <- g **~ w;
        commitments.(1) <- y **~ w;
        let dst = dst_prefix ^ "-bproof1" in
        let h = G.hash ~dst zkp commitments in
        let challenge = Zq.(h - !total_challenges) in
        let response = Zq.(w - (r0 * challenge)) in
        overall_proof.(0) <- { challenge; response };
        overall_proof
      in
      (overall_proof, blank_proof)

  let verify_blank_proof y zkp min max (c0 : _ ciphertext) (cS : _ ciphertext)
      overall_proof blank_proof =
    G.check c0.alpha && G.check c0.beta && G.check cS.alpha && G.check cS.beta
    (* check blank_proof, proof of m0 = 0 \/ mS = 0 *)
    && Array.length blank_proof = 2
    && (let commitments = Array.make 4 g in
        let total_challenges = ref Zq.zero in
        let { challenge; response } : _ proof = blank_proof.(0) in
        commitments.(0) <- (g **~ response) *~ (c0.alpha **~ challenge);
        commitments.(1) <- (y **~ response) *~ (c0.beta **~ challenge);
        (total_challenges := Zq.(!total_challenges + challenge));
        let { challenge; response } : _ proof = blank_proof.(1) in
        commitments.(2) <- (g **~ response) *~ (cS.alpha **~ challenge);
        commitments.(3) <- (y **~ response) *~ (cS.beta **~ challenge);
        (total_challenges := Zq.(!total_challenges + challenge));
        let dst = dst_prefix ^ "-bproof0" in
        let h = G.hash ~dst zkp commitments in
        Zq.(h =% !total_challenges))
    (* check overall_proof, proof of m0 = 1 \/ min <= mS <= max *)
    && Array.length overall_proof = max - min + 2
    &&
    let commitments = Array.make (2 * (max - min + 2)) g in
    let total_challenges = ref Zq.zero in
    let { challenge; response } : _ proof = overall_proof.(0) in
    commitments.(0) <- (g **~ response) *~ (c0.alpha **~ challenge);
    commitments.(1) <- (y **~ response) *~ ((c0.beta / g) **~ challenge);
    (total_challenges := Zq.(!total_challenges + challenge));
    let rec loop i =
      if i < max - min + 2 then (
        let { challenge; response } : _ proof = overall_proof.(i) in
        let g' =
          if min + i - 1 = 0 then G.one else g **~ Zq.of_int (min + i - 1)
        in
        let nbeta = cS.beta / g' in
        let j = 2 * i in
        commitments.(j) <- (g **~ response) *~ (cS.alpha **~ challenge);
        commitments.(j + 1) <- (y **~ response) *~ (nbeta **~ challenge);
        (total_challenges := Zq.(!total_challenges + challenge));
        loop (i + 1))
      else ()
    in
    loop 1;
    let dst = dst_prefix ^ "-bproof1" in
    let h = G.hash ~dst zkp commitments in
    Zq.(h =% !total_challenges)

  let invg = invert g
  let d01 = [| G.one; invg |]

  let make_d min max =
    let n = max - min + 1 in
    let g' = if min = 0 then G.one else g **~ Zq.of_int min in
    let d = Array.make n (invert g') in
    for i = 1 to n - 1 do
      d.(i) <- d.(i - 1) *~ invg
    done;
    d

  let stringify_choices =
    Array.map (fun ({ alpha; beta } : _ ciphertext) ->
        Printf.sprintf "%s,%s" (G.to_string alpha) (G.to_string beta))
    >> Array.to_list >> String.concat ","

  let create_answer q ~public_key:y ~prefix:zkp m =
    let m = Shape.to_array m in
    let n = Array.length m in
    let r = Array.init n (fun _ -> random ()) in
    let choices = Array.map2 (eg_encrypt y) r m in
    let individual_proofs = Array.map3 (eg_disj_prove y d01 zkp) m r choices in
    let zkp = zkp ^ "|" ^ stringify_choices choices in
    match q.blank with
    | true ->
        (* index 0 is whether the ballot is blank or not,
           indexes 1..n-1 are the actual choices *)
        assert (n = Array.length q.answers + 1);
        let choices' = Array.sub choices 1 (n - 1) in
        let r' = Array.sub r 1 (n - 1) in
        let m' = Array.sub m 1 (n - 1) in
        let sumr = Array.fold_left Zq.( + ) Zq.zero r' in
        let summ = Array.fold_left ( + ) 0 m' in
        let sumc = Array.fold_left eg_combine dummy_ciphertext choices' in
        let overall_proof, blank_proof =
          make_blank_proof y zkp q.min q.max m.(0) choices.(0) r.(0) summ sumc
            sumr
        in
        let blank_proof = Some blank_proof in
        { choices; individual_proofs; overall_proof; blank_proof }
    | false ->
        (* indexes 0..n-1 are the actual choices *)
        assert (n = Array.length q.answers);
        let sumr = Array.fold_left Zq.( + ) Zq.zero r in
        let summ = Array.fold_left ( + ) 0 m in
        let sumc = Array.fold_left eg_combine dummy_ciphertext choices in
        assert (q.min <= summ && summ <= q.max);
        let d = make_d q.min q.max in
        let overall_proof = eg_disj_prove y d zkp (summ - q.min) sumr sumc in
        let blank_proof = None in
        { choices; individual_proofs; overall_proof; blank_proof }

  let verify_answer q ~public_key:y ~prefix:zkp a =
    let n = Array.length a.choices in
    n = Array.length a.individual_proofs
    && Array.for_all2 (eg_disj_verify y d01 zkp) a.individual_proofs a.choices
    &&
    let zkp = zkp ^ "|" ^ stringify_choices a.choices in
    match (q.blank, a.blank_proof) with
    | true, Some blank_proof ->
        n = Array.length q.answers + 1
        &&
        let c = Array.sub a.choices 1 (n - 1) in
        let sumc = Array.fold_left eg_combine dummy_ciphertext c in
        verify_blank_proof y zkp q.min q.max a.choices.(0) sumc a.overall_proof
          blank_proof
    | _, None ->
        n = Array.length q.answers
        &&
        let sumc = Array.fold_left eg_combine dummy_ciphertext a.choices in
        let d = make_d q.min q.max in
        eg_disj_verify y d zkp a.overall_proof sumc
    | _, _ -> false

  let extract_ciphertexts _ a =
    Shape.Array (Array.map (fun x -> Shape.Atomic x) a.choices)

  let process_ciphertexts q es =
    let neutral =
      Shape.Array
        (Array.make (question_length q) (Shape.Atomic dummy_ciphertext))
    in
    let ( * ) = Shape.map2 eg_combine in
    let rec power b n =
      if Z.(compare n zero) > 0 then
        let x = power b Z.(shift_right n 1) in
        (if Z.(compare (logand n one) one) = 0 then b else neutral) * x * x
      else neutral
    in
    let es = List.map (fun (w, b) -> power b (Weight.to_Z w)) es in
    List.fold_left (Shape.map2 eg_combine) neutral es

  let compute_result ~total_weight:total _ =
    let num_tallied = Weight.to_Z total in
    let log =
      let module X = BabyStepGiantStep (G) in
      let log = X.log ~generator:G.g ~max:num_tallied in
      fun x ->
        match log x with
        | Some x -> x
        | None -> invalid_arg "Cannot compute result"
    in
    fun x ->
      Shape.to_array x |> Array.map (fun i -> log i |> Zq.to_Z |> Weight.of_Z)

  let check_result ~total_weight:_ _ x r =
    Array.for_all2
      (fun x r ->
        let r = r |> Weight.to_Z |> Zq.coerce in
        let g' = if Zq.compare r Zq.zero = 0 then G.one else g **~ r in
        x =~ g')
      (Shape.to_array x) r
end
