(**************************************************************************)
(*                                BELENIOS                                *)
(*                                                                        *)
(*  Copyright © 2012-2023 Inria                                           *)
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
open Belenios_question
open Lists
open Syntax

type nonrec 'a cipertext = 'a ciphertext = { alpha : 'a; beta : 'a }
type nonrec question = question
type nonrec result = result [@@deriving yojson]

let id = id

let of_concrete (Q x : Belenios_question.t) : question option =
  let module Q = (val x.type_) in
  match Type.Id.provably_equal Q.id id with
  | Some Equal -> Some x.value
  | _ -> None

(** Helper functions *)

let get_complexity q =
  let nb_ciphertexts =
    Array.fold_left (fun accu l -> accu + 1 + Array.length l) 0 q.answers
  in
  let nb_zkps = (2 * nb_ciphertexts) + 1 + (2 * Array.length q.answers) + 1 in
  { nb_ciphertexts; nb_zkps }

module Make (G : GROUP) = struct
  open G

  type nonrec answer = (G.t, G.Zq.t) answer

  let yojson_of_answer = [%yojson_of_group: answer]
  let answer_of_yojson = [%group_of_yojson: answer]
  let random () = Zq.random (Crypto_primitives.get_rng ())
  let ( / ) x y = x *~ invert y
  let dummy_ciphertext = { alpha = G.one; beta = G.one }

  (** Multiply two ElGamal ciphertexts. *)
  let eg_combine c1 c2 =
    { alpha = c1.alpha *~ c2.alpha; beta = c1.beta *~ c2.beta }

  (** ElGamal encryption. *)
  let eg_encrypt y r x =
    { alpha = g **~ r; beta = (y **~ r) *~ (g **~ Zq.of_int x) }

  let dummy_proof = { challenge = Zq.zero; response = Zq.zero }

  (** Fiat-Shamir non-interactive zero-knowledge proofs of knowledge *)

  let fs_prove gs x oracle =
    let w = random () in
    let commitments = Array.map (fun g -> g **~ w) gs in
    let challenge = oracle commitments in
    let response = Zq.(w - (x * challenge)) in
    { challenge; response }

  (** ZKPs for disjunctions *)

  let eg_disj_prove y d zkp x r { alpha; beta } =
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
          let prefix =
            Printf.sprintf "%s|%s,%s|" zkp (G.to_string alpha)
              (G.to_string beta)
          in
          let dst = dst_prefix ^ "-prove_l" in
          Zq.(G.hash ~dst prefix commitments - !total_challenges))
    in
    proofs.(x) <- p;
    proofs

  let eg_disj_verify y d zkp proofs { alpha; beta } =
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
    let prefix =
      Printf.sprintf "%s|%s,%s|" zkp (G.to_string alpha) (G.to_string beta)
    in
    let dst = dst_prefix ^ "-prove_l" in
    Zq.(G.hash ~dst prefix commitments =% !total_challenges)

  let invg = invert g
  let d01 = [| G.one; invg |]
  let d1 = [| invg |]

  let stringify_choices =
    Array.map (fun xs ->
        xs
        |> Array.map (fun { alpha; beta } ->
            Printf.sprintf "%s,%s" (G.to_string alpha) (G.to_string beta))
        |> Array.to_list |> String.concat ",")
    >> Array.to_list >> String.concat ","

  let unshapify = function
    | Shape.Array xs -> Array.map Shape.to_array xs
    | _ -> invalid_arg "unshapify"

  let split_first f accu xs =
    let n = Array.length xs in
    let rec loop accu i =
      if i < n then loop (f accu xs.(i)) (i + 1) else (xs.(0), accu)
    in
    loop accu 1

  let create_list_proof y zkp m r c =
    let m0, mS = split_first ( + ) 0 m in
    let r0, rS = split_first Zq.( + ) Zq.zero r in
    let c0, cS = split_first eg_combine dummy_ciphertext c in
    (* proof of m0 = 1 \/ mS = 0 *)
    match m0 = 1 with
    | true ->
        (* proof of m0 = 1 \/ mS = 0 (first is true) *)
        let challenge1 = random () in
        let response1 = random () in
        let commitmentA1 = g **~ Zq.(response1 + (rS * challenge1)) in
        let commitmentB1 = (y **~ response1) *~ (cS.beta **~ challenge1) in
        let w = random () in
        let commitmentA0 = g **~ w and commitmentB0 = y **~ w in
        let dst = dst_prefix ^ "-lproof" in
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
    | false ->
        (* proof of m0 = 1 \/ mS = 0 (second is true) *)
        if mS <> 0 then invalid_arg "create_list_proof";
        let challenge0 = random () in
        let response0 = random () in
        let commitmentA0 = g **~ Zq.(response0 + (r0 * challenge0)) in
        let commitmentB0 =
          (y **~ response0) *~ ((c0.beta / g) **~ challenge0)
        in
        let w = random () in
        let commitmentA1 = g **~ w and commitmentB1 = y **~ w in
        let dst = dst_prefix ^ "-lproof" in
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

  let rec random_nonzero () =
    let x = random () in
    if Zq.(x =% zero) then random_nonzero () else x

  let verify_list_proof y zkp c proof =
    let c0, cS = split_first eg_combine dummy_ciphertext c in
    G.check c0.alpha && G.check c0.beta && G.check cS.alpha && G.check cS.beta
    (* check proof of m0 = 1 \/ mS = 0 *)
    &&
    let commitments = Array.make 4 g in
    let total_challenges = ref Zq.zero in
    let { challenge; response } : _ proof = proof.(0) in
    commitments.(0) <- (g **~ response) *~ (c0.alpha **~ challenge);
    commitments.(1) <- (y **~ response) *~ ((c0.beta / g) **~ challenge);
    (total_challenges := Zq.(!total_challenges + challenge));
    let { challenge; response } : _ proof = proof.(1) in
    commitments.(2) <- (g **~ response) *~ (cS.alpha **~ challenge);
    commitments.(3) <- (y **~ response) *~ (cS.beta **~ challenge);
    (total_challenges := Zq.(!total_challenges + challenge));
    let dst = dst_prefix ^ "-lproof" in
    let h = G.hash ~dst zkp commitments in
    Zq.(h =% !total_challenges)

  let create_nonzero_proof y zkp { alpha; beta } r =
    (* proof of "{ alpha; beta } encrypts something non-zero" *)
    let s = random_nonzero () in
    let commitment = (beta **~ s) *~ (y **~ Zq.(zero - (s * r))) in
    if commitment =~ one then invalid_arg "create_inequality_proof";
    let w1 = random () and w2 = random () in
    let a1 = (alpha **~ w1) *~ (g **~ w2) in
    let a2 = (beta **~ w1) *~ (y **~ w2) in
    let dst = dst_prefix ^ "-nonzero" in
    let challenge = G.hash ~dst zkp [| commitment; a1; a2 |] in
    let t1 = Zq.(w1 - (s * challenge)) in
    let t2 = Zq.(w2 + (s * r * challenge)) in
    let response = (t1, t2) in
    { commitment; challenge; response }

  let verify_nonzero_proof y zkp ({ alpha; beta } : _ ciphertext)
      (proof : _ nonzero_proof) =
    (* check proof of "{ alpha; beta } encrypts something non-zero" *)
    let { commitment; challenge; response = t1, t2 } = proof in
    (not (commitment =~ one))
    &&
    let a1 = (alpha **~ t1) *~ (g **~ t2) in
    let a2 = (beta **~ t1) *~ (y **~ t2) *~ (commitment **~ challenge) in
    let dst = dst_prefix ^ "-nonzero" in
    Zq.(challenge =% G.hash ~dst zkp [| commitment; a1; a2 |])

  let combine_except_first f x0 xs =
    let n = Array.length xs in
    if n > 1 then
      let rec loop accu i =
        if i < n then loop (f accu xs.(i)) (i + 1) else accu
      in
      loop x0 1
    else x0

  let combine_list_items_ciphertexts =
    Array.map (combine_except_first eg_combine dummy_ciphertext)
    >> Array.fold_left eg_combine dummy_ciphertext

  let combine_list_items_randoms =
    Array.map (combine_except_first Zq.( + ) Zq.zero)
    >> Array.fold_left Zq.( + ) Zq.zero

  let create_answer q ~public_key:y ~prefix:zkp m =
    let m = unshapify m in
    let r =
      Array.map2
        (fun q m ->
          let n = Array.length q in
          if n = Array.length m then Array.init n (fun _ -> random ())
          else invalid_arg "create_answer")
        q.answers m
    in
    let choices = Array.map2 (Array.map2 (eg_encrypt y)) r m in
    let individual_proofs =
      Array.map3 (Array.map3 (eg_disj_prove y d01 zkp)) m r choices
    in
    let zkp = zkp ^ "|" ^ stringify_choices choices in
    let overall_proof =
      let open Zq in
      let r = Array.fold_left (fun accu r -> accu + r.(0)) zero r in
      let c =
        Array.fold_left
          (fun accu c -> eg_combine accu c.(0))
          dummy_ciphertext choices
      in
      let p = eg_disj_prove y d1 zkp 0 r c in
      p.(0)
    in
    let list_proofs = Array.map3 (create_list_proof y zkp) m r choices in
    let nonzero_proof =
      let c = combine_list_items_ciphertexts choices in
      let r = combine_list_items_randoms r in
      create_nonzero_proof y zkp c r
    in
    { choices; individual_proofs; overall_proof; list_proofs; nonzero_proof }

  let verify_answer q ~public_key:y ~prefix:zkp a =
    let n = Array.length q.answers in
    n = Array.length a.choices
    && n = Array.length a.individual_proofs
    && Array.for_all3
         (fun q p c ->
           let n = Array.length q in
           n = Array.length p
           && n = Array.length c
           && Array.for_all2 (eg_disj_verify y d01 zkp) p c)
         q.answers a.individual_proofs a.choices
    &&
    let zkp = zkp ^ "|" ^ stringify_choices a.choices in
    Array.fold_left
      (fun accu c -> eg_combine accu c.(0))
      dummy_ciphertext a.choices
    |> eg_disj_verify y d1 zkp [| a.overall_proof |]
    && Array.for_all2 (verify_list_proof y zkp) a.choices a.list_proofs
    &&
    let c = combine_list_items_ciphertexts a.choices in
    verify_nonzero_proof y zkp c a.nonzero_proof

  let extract_ciphertexts _ a =
    Shape.Array
      (Array.map
         (Array.map (fun x -> Shape.Atomic x) >> fun x -> Shape.Array x)
         a.choices)

  let process_ciphertexts q es =
    let neutral =
      q.answers
      |> Array.map
           ( Array.map (fun _ -> Shape.Atomic dummy_ciphertext) >> fun x ->
             Shape.Array x )
      |> fun x -> Shape.Array x
    in
    let ( * ) = Shape.map2 eg_combine in
    let rec power b n =
      if Z.(compare n zero) > 0 then
        let x = power b Z.(shift_right n 1) in
        (if Z.(compare (logand n one) one) = 0 then b else neutral) * x * x
      else neutral
    in
    let total =
      let open Weight in
      List.fold_left (fun a (w, _) -> a + w) zero es
    in
    let es = List.map (fun (w, b) -> power b (Weight.expand ~total w)) es in
    List.fold_left (Shape.map2 eg_combine) neutral es

  let compute_result ~total_weight:total _ =
    let num_tallied = Weight.expand ~total total in
    let log =
      let module X = BabyStepGiantStep (G) in
      let log = X.log ~generator:G.g ~max:num_tallied in
      fun x ->
        match log x with
        | Some x -> x
        | None -> invalid_arg "Cannot compute result"
    in
    unshapify
    >> Array.map (Array.map (fun i -> Weight.reduce ~total (log i |> Zq.to_Z)))

  let check_result ~total_weight _ x r =
    Array.for_all2
      (Array.for_all2 (fun x r ->
           let r = Weight.expand ~total:total_weight r |> Zq.coerce in
           let g' = if Zq.compare r Zq.zero = 0 then G.one else g **~ r in
           x =~ g'))
      (unshapify x) r
end
