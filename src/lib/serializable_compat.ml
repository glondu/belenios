open Util
open Serializable_compat_t

let question q =
  let {q_answers; q_min; q_max; q_question; _} = q in
  let q_max = match q_max with
    | Some i -> i
    | None -> Array.length q_answers
  in
  let open Serializable_t in
  {q_answers; q_min; q_max; q_question}

let params e =
  let {
    e_description; e_name; e_public_key;
    e_questions; e_uuid; e_short_name;
    _
  } = e in
  let e_public_key = e_public_key.y in
  let e_questions = Array.map question e_questions in
  let open Serializable_t in
  {
    e_description; e_name; e_public_key;
    e_questions; e_uuid; e_short_name
  }

let proof p =
  let {dp_challenge; dp_response; _} = p in
  let open Serializable_t in
  {challenge = dp_challenge; response = dp_response}

let proofs ps = Array.map proof ps

let answer a =
  let {choices; individual_proofs; overall_proof} = a in
  let individual_proofs = Array.map proofs individual_proofs in
  let overall_proof = proofs overall_proof in
  let open Serializable_t in
  {choices; individual_proofs; overall_proof}

let ballot b =
  let {answers; election_hash; election_uuid} = b in
  let answers = Array.map answer answers in
  let signature = None in
  let open Serializable_t in
  {answers; election_hash; election_uuid; signature}

let partial_decryption p =
  let {decryption_factors; decryption_proofs} = p in
  let decryption_proofs = Array.mmap proof decryption_proofs in
  let open Serializable_t in
  {decryption_factors; decryption_proofs}

module MakeCompat (P : Signatures.ELECTION_PARAMS) = struct
  open Serializable_t
  open P
  open G

  (* The following duplicates parts of module Crypto, in order to
     reconstruct commitments. *)

  let dummy_ciphertext =
    {
      alpha = G.one;
      beta = G.one;
    }

  let eg_combine c1 c2 =
    {
      alpha = c1.alpha *~ c2.alpha;
      beta = c1.beta *~ c2.beta;
    }

  let dummy_proof =
    let open Serializable_compat_t in
    {
      dp_commitment = {a = G.one; b = G.one};
      dp_challenge = Z.zero;
      dp_response = Z.zero;
    }

  let y = params.e_public_key
  let ( / ) x y = x *~ invert y

  let invg = invert G.g
  let d01 = [| G.one; invg |]

  let make_d min max =
    let n = max - min + 1 in
    let d = Array.create n (invert (g **~ Z.of_int min)) in
    for i = 1 to n-1 do
      d.(i) <- d.(i-1) *~ invg
    done;
    d

  let recommit d proofs {alpha; beta} =
    let n = Array.length d in
    assert (n = Array.length proofs);
    let result = Array.create n dummy_proof in
    for i = 0 to n-1 do
      let {challenge; response} = proofs.(i) in
      let dp_commitment = {
        a = g **~ response / alpha **~ challenge;
        b = y **~ response / (beta *~ d.(i)) **~ challenge;
      } in
      let open Serializable_compat_t in
      result.(i) <- {
        dp_commitment;
        dp_challenge = challenge;
        dp_response = response;
      };
    done;
    result

  let answer a q =
    let {choices; individual_proofs; overall_proof} = a in
    let individual_proofs =
      Array.map2 (recommit d01) individual_proofs choices
    in
    let sumc = Array.fold_left eg_combine dummy_ciphertext choices in
    let overall_proof =
      recommit (make_d q.q_min q.q_max) overall_proof sumc
    in
    let open Serializable_compat_t in
    {choices; individual_proofs; overall_proof}

  let ballot b =
    let {answers; election_hash; election_uuid} = b in
    let answers = Array.map2 answer answers params.e_questions in
    let open Serializable_compat_t in
    {answers; election_hash; election_uuid}

  let partial_decryption c p =
    let {decryption_factors; decryption_proofs} = p in
    let decryption_proofs =
      Array.mmap3 (fun {alpha; _} f {challenge; response} ->
        let open Serializable_compat_t in
        let dp_commitment = {
          a = g **~ response / (y **~ challenge);
          b = alpha **~ response / (f **~ challenge);
        } in {
          dp_commitment;
          dp_challenge = challenge;
          dp_response = response;
        }
      ) c decryption_factors decryption_proofs
    in
    let open Serializable_compat_t in
    {decryption_factors; decryption_proofs}
end
