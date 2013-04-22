open Serializable_compat_t

let of_question q =
  let {q_answers; q_min; q_max; q_question; _} = q in
  let q_max = match q_max with
    | Some i -> i
    | None -> Array.length q_answers
  in
  let open Serializable_t in
  {q_answers; q_min; q_max; q_question}

let of_election e =
  let {
    e_description; e_name; e_public_key;
    e_questions; e_uuid; e_short_name;
    _
  } = e in
  let e_public_key = e_public_key.y in
  let e_questions = Array.map of_question e_questions in
  let open Serializable_t in
  {
    e_description; e_name; e_public_key;
    e_questions; e_uuid; e_short_name
  }

let of_proof p =
  let {dp_challenge; dp_response; _} = p in
  let open Serializable_t in
  {challenge = dp_challenge; response = dp_response}

let of_proofs ps = Array.map of_proof ps

let of_answer a =
  let {choices; individual_proofs; overall_proof} = a in
  let individual_proofs = Array.map of_proofs individual_proofs in
  let overall_proof = of_proofs overall_proof in
  let open Serializable_t in
  {choices; individual_proofs; overall_proof}

let of_ballot b =
  let {answers; election_hash; election_uuid} = b in
  let answers = Array.map of_answer answers in
  let open Serializable_t in
  {answers; election_hash; election_uuid}
