open StdExtra
open Helios_datatypes_t

module type GROUP = sig
  type t
  val one : t
  val g : t
  val q : Z.t
  val p : Z.t
  val ( *~ ) : t -> t -> t
  val ( **~ ) : t -> Z.t -> t
  val ( =~ ) : t -> t -> bool
  val inv : t -> t
  val check_exponent : Z.t -> bool
  val check_element : t -> bool
  val hash : t list -> Z.t
end

let hashZ x = Cryptokit.(x |>
  hash_string (Hash.sha1 ()) |>
  transform_string (Hexa.encode ()) |>
  Z.of_string_base 16
)

let check_modulo p x = Z.(geq x zero && lt x p)

let make_ff_msubgroup p q g =
  if
    Z.probab_prime p 10 > 0 &&
    Z.probab_prime q 10 > 0 &&
    check_modulo p g &&
    check_modulo p q &&
    Z.(powm g q p =% one)
  then
    let module G = struct
      open Z
      type t = Z.t
      let one = Z.one
      let p = p
      let q = q
      let g = g
      let ( *~ ) a b = a * b mod p
      let ( **~ ) a b = powm a b p
      let inv x = invert x p
      let ( =~ ) = equal
      let check_element x = check_modulo p x && x **~ q =~ one
      let check_exponent x = check_modulo q x
      let hash x = hashZ (String.concat "," (List.map to_string x)) mod q
    end in (module G : GROUP with type t = Z.t)
  else
    invalid_arg "Invalid parameters for a multiplicative subgroup of finite field"

module type ELGAMAL_CRYPTO = sig
  type t
  val verify_public_key : t public_key -> bool
  val verify_private_key : t private_key -> bool
  val verify_pok : t -> t pok -> bool
  val verify_election_key : t -> t trustee_public_key array -> bool
  val verify_disjunction : t -> t -> t array -> t proof array -> bool
  val verify_range : t -> int -> int -> t -> t -> t proof array -> bool
  val verify_answer : t -> question -> t answer -> bool
  val verify_vote : t election -> string -> t vote -> bool
  val verify_equality : t -> t -> t -> t proof -> bool
  val verify_partial_decryption : t election ->
    t tally -> t trustee_public_key -> t partial_decryption -> bool
  val verify_partial_decryptions : t election ->
    t trustee_public_key array -> t result -> bool
  val verify_result : t election -> t result -> bool
  val compute_encrypted_tally : t election -> t vote array -> t encrypted_tally
end

module Make (G : GROUP) = struct
  open G

  (* FIXME: redundancy of group parameters that are embedded in the
     abstract group *)

  let verify_public_key k =
    let {g = g'; p = p'; q = q'; y} = k in
    g =~ g' && p =% p' && q =% q' && check_element y

  let verify_private_key k =
    let {x; public_key = {y; _}} = k in
    check_exponent x && y =~ g **~ x

  let verify_pok y pok =
    let {pok_commitment; pok_challenge; pok_response} = pok in
    (* NB: we don't check commitment and challenge thanks to hash *)
    check_exponent pok_response &&
    g **~ pok_response =~ pok_commitment *~ y **~ pok_challenge &&
    pok_challenge =% hash [pok_commitment]

  let verify_election_key y tpks =
    let n = Array.length tpks in
    assert (n > 0);
    let rec loop i accu =
      if i >= 0 then
        let tpk = tpks.(i) in
        let {g = g'; p = p'; q = q'; y = y'} = tpk.trustee_public_key in
        g =~ g' && p =% p' && q =% q' && verify_pok y' tpk.trustee_pok &&
        loop (pred i) (accu *~ y')
      else accu =~ y
    in loop (pred n) one

  let verify_disjunction h big_g big_hs proof =
    let n = Array.length big_hs in
    assert (n > 0);
    n = Array.length proof &&
    (let rec check i commitments challenges =
     if i >= 0 then
       let {dp_commitment = {a; b}; dp_challenge; dp_response} = proof.(i) in
       (* NB: we don't check commitment and challenge thanks to hash *)
       check_exponent dp_response &&
       g **~ dp_response =~ big_g **~ dp_challenge *~ a &&
       h **~ dp_response =~ big_hs.(i) **~ dp_challenge *~ b &&
       check (pred i) (a :: b :: commitments) Z.(challenges + dp_challenge)
     else
       hash commitments =% Z.(challenges mod q)
   in check (pred n) [] Z.zero)

  let verify_range h min max alpha beta proof =
    Array.length proof = 2 &&
    let big_hs = Array.init (max-min+1) (fun i -> beta *~ inv (g **~ Z.of_int (i-min))) in
    verify_disjunction h alpha big_hs proof

  let verify_answer y question answer =
    let {q_max; q_min; q_answers; _} = question in
    let q_max =
      match q_max with
        | Some q -> q
        | None -> assert false (* FIXME *)
    in
    let nb = Array.length q_answers in
    Array.length answer.choices = nb &&
    Array.length answer.individual_proofs = nb &&
    (let rec check i alphas betas =
       if i >= 0 then
         let {alpha; beta} = answer.choices.(i) in
         check_element alpha &&
         check_element beta &&
         verify_range y 0 1 alpha beta answer.individual_proofs.(i) &&
         check (pred i) (alphas *~ alpha) (betas *~ beta)
       else
         verify_range y q_min q_max alphas betas answer.overall_proof
     in check (pred nb) one one)

  let verify_vote e fingerprint v =
    v.election_hash = fingerprint &&
    e.e_uuid = v.election_uuid &&
    Array.forall2 (verify_answer e.e_public_key.y) e.e_questions v.answers

  let verify_equality h g' h' proof =
    (* NB: similar to disjunctive, but with different challenge
       checking... hardly factorizable *)
    let {dp_commitment = {a; b}; dp_challenge; dp_response} = proof in
    (* NB: we don't check commitment and challenge thanks to hash *)
    check_exponent dp_response &&
    g **~ dp_response =~ g' **~ dp_challenge *~ a &&
    h **~ dp_response =~ h' **~ dp_challenge *~ b &&
    dp_challenge =% hash [a; b]

  let verify_partial_decryption election tally tpk pds =
    let y = tpk.trustee_public_key.y in
    let {decryption_factors = dfs; decryption_proofs = dps} = pds in
    Array.foralli (fun i question ->
      let dfs_i = dfs.(i) and dps_i = dps.(i) and tally_i = tally.(i) in
      Array.foralli (fun j answer ->
        verify_equality tally_i.(j).alpha y dfs_i.(j) dps_i.(j)
      ) question.q_answers
    ) election.e_questions

  let verify_partial_decryptions election public_keys r =
    Array.forall2 (verify_partial_decryption election r.encrypted_tally.tally)
      public_keys
      r.partial_decryptions

  let verify_result election public_data =
    let pds = public_data.partial_decryptions in
    let tally = public_data.encrypted_tally.tally in
    let result = public_data.result in
    Array.foralli (fun i question ->
      Array.foralli (fun j answer ->
        let combined_factor = Array.fold_left (fun accu f ->
          accu *~ f.decryption_factors.(i).(j)
        ) one pds in
        inv combined_factor *~ tally.(i).(j).beta =~ g **~ Z.of_int result.(i).(j)
      ) question.q_answers
    ) election.e_questions

  let compute_encrypted_tally e vs =
    let ( * ) a b = Z.({ alpha = a.alpha *~ b.alpha; beta = a.beta *~ b.beta}) in
    let num_tallied = Array.length vs in
    let tally = Array.mapi (fun i question ->
      Array.mapi (fun j answer ->
        Array.fold_left (fun accu v ->
          accu * v.answers.(i).choices.(j)
        ) { alpha = one; beta = one} vs
      ) question.q_answers
    ) e.e_questions in
    { num_tallied; tally }

end
