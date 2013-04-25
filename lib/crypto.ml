open Util
open Serializable_t
open Crypto_sigs

(** Helper functions *)

let check_modulo p x = Z.(geq x zero && lt x p)

let hashZ x = Cryptokit.(x |>
  hash_string (Hash.sha1 ()) |>
  transform_string (Hexa.encode ()) |>
  Z.of_string_base 16
)

let map_and_concat_with_commas f xs =
  let n = Array.length xs in
  let res = Buffer.create (n * 1024) in
  for i = 0 to n-1 do
    Buffer.add_string res (f xs.(i));
    Buffer.add_char res ',';
  done;
  let size = Buffer.length res - 1 in
  if size > 0 then Buffer.sub res 0 size else ""

(** Finite field arithmetic *)

let check_finite_field ~p ~q ~g =
  Z.probab_prime p 10 > 0 &&
  Z.probab_prime q 10 > 0 &&
  check_modulo p g &&
  check_modulo p q &&
  Z.(powm g q p =% one)

let finite_field ~p ~q ~g =
  let module G = struct
    open Z
    type t = Z.t
    let q = q
    let one = Z.one
    let g = g
    let ( *~ ) a b = a * b mod p
    let ( **~ ) a b = powm a b p
    let invert x = Z.invert x p
    let ( =~ ) = Z.equal
    let check x = check_modulo p x && x **~ q =~ one
    let hash xs = hashZ (map_and_concat_with_commas Z.to_string xs)
    let compare = Z.compare
  end in (module G : GROUP with type t = Z.t)

(** Parameters *)

let check_election p =
  let module P = (val p : ELECTION_PARAMS) in
  let open P in
  let open G in
  (* check public key *)
  let computed = Array.fold_left ( *~ ) G.one public_keys in
  computed =~ params.e_public_key

(** Dummy monad *)

module MakeDummyMonad (G : GROUP) = struct
  type 'a t = 'a
  let return x = x
  let bind x f = f x
  let random q = Util.random q
  type ballot = G.t Serializable_t.ballot
  let cast x = ()
  let fold f x = return x
end

(** Homomorphic elections *)

module MakeElection
  (P : ELECTION_PARAMS)
  (M : ELECTION_MONAD with type ballot = P.G.t Serializable_t.ballot)
  =
struct
  open P
  open G

  type 'a m = 'a

  type elt = G.t
  type private_key = Z.t
  type public_key = elt

  let election_params = params
  let y = params.e_public_key
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

  let combine_ciphertexts = Array.mmap2 eg_combine

  type plaintext = int array array
  type ballot = elt Serializable_t.ballot
  type randomness = Z.t array array

  (** ElGamal encryption. *)
  let eg_encrypt r x =
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
    let w = random q in
    let commitments = Array.map (fun g -> g **~ w) gs in
    let challenge = oracle commitments in
    let response = Z.((w + x * challenge) mod q) in
    {challenge; response}

  (** ZKPs for disjunctions *)

  let eg_disj_prove d x r {alpha; beta} =
    (* prove that alpha = g^r and beta = y^r/d_x *)
    (* the size of d is the number of disjuncts *)
    let n = Array.length d in
    assert (0 <= x && x < n);
    let proofs = Array.create n dummy_proof
    and commitments = Array.create (2*n) g
    and total_challenges = ref Z.zero in
    (* compute fake proofs *)
    let f i =
      let challenge = random q
      and response = random q in
      proofs.(i) <- {challenge; response};
      commitments.(2*i) <- g **~ response / alpha **~ challenge;
      commitments.(2*i+1) <- y **~ response / (beta *~ d.(i)) **~ challenge;
      total_challenges := Z.(!total_challenges + challenge);
    in
    for i = 0 to x-1 do f i done;
    for i = x+1 to n-1 do f i done;
    total_challenges := Z.(q - !total_challenges mod q);
    (* compute genuine proof *)
    proofs.(x) <- fs_prove [| g; y |] r (fun commitx ->
      Array.blit commitx 0 commitments (2*x) 2;
      Z.((G.hash commitments + !total_challenges) mod q)
    );
    proofs

  let eg_disj_verify d proofs {alpha; beta} =
    G.check alpha && G.check beta &&
    let n = Array.length d in
    n = Array.length proofs &&
    let commitments = Array.create (2*n) g
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
      hash commitments =% !total_challenges
    with Exit -> false

  (** Ballot creation *)

  let invg = invert g
  let d01 = [| G.one; invg |]

  let make_d min max =
    let n = max - min + 1 in
    let d = Array.create n (invert (g **~ Z.of_int min)) in
    for i = 1 to n-1 do
      d.(i) <- d.(i-1) *~ invg
    done;
    d

  let create_answer q r m =
    let choices = Array.map2 eg_encrypt r m in
    let individual_proofs = Array.map3 (eg_disj_prove d01) m r choices in
    (* create overall_proof from homomorphic combination of individual
       weights *)
    let sumr = Array.fold_left Z.(+) Z.zero r in
    let summ = Array.fold_left (+) 0 m in
    let sumc = Array.fold_left eg_combine dummy_ciphertext choices in
    assert (q.q_min <= summ && summ <= q.q_max);
    let d = make_d q.q_min q.q_max in
    let overall_proof = eg_disj_prove d (summ - q.q_min) sumr sumc in
    {choices; individual_proofs; overall_proof}

  let create_randomness () =
    Array.map (fun q ->
      Array.init (Array.length q.q_answers) (fun _ -> random G.q)
    ) params.e_questions

  let create_ballot r m =
    {
      answers = Array.map3 create_answer params.e_questions r m;
      election_hash = fingerprint;
      election_uuid = params.e_uuid
    }

  (** Ballot verification *)

  let verify_answer q a =
    Array.forall2 (eg_disj_verify d01) a.individual_proofs a.choices &&
    let sumc = Array.fold_left eg_combine dummy_ciphertext a.choices in
    let d = make_d q.q_min q.q_max in
    eg_disj_verify d a.overall_proof sumc

  let check_ballot b =
    b.election_uuid = params.e_uuid &&
    b.election_hash = P.fingerprint &&
    Array.forall2 verify_answer params.e_questions b.answers

  let extract_ciphertext b = Array.map (fun x -> x.choices) b.answers

  type factor = elt Serializable_t.partial_decryption

  let eg_factor x {alpha; beta} =
    alpha **~ x,
    fs_prove [| g; alpha |] x hash

  let check_ciphertext c =
    Array.fforall (fun {alpha; beta} -> G.check alpha && G.check beta) c

  let compute_factor c x =
    if check_ciphertext c then (
      let res = Array.mmap (eg_factor x) c in
      let decryption_factors, decryption_proofs = Array.ssplit res in
      {decryption_factors; decryption_proofs}
    ) else (
      invalid_arg "Invalid ciphertext"
    )

  let check_factor c y f =
    Array.fforall3 (fun {alpha; _} f {challenge; response} ->
      check_modulo q challenge &&
      check_modulo q response &&
      let commitments =
        [|
          g **~ response / (y **~ challenge);
          alpha **~ response / (f **~ challenge);
        |]
      in hash commitments =% challenge
    ) c f.decryption_factors f.decryption_proofs

  type result = elt Serializable_t.result

  let combine_factors nb_tallied encrypted_tally partial_decryptions =
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
        if i <= nb_tallied
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
    {nb_tallied; encrypted_tally; partial_decryptions; result}

  let check_result r =
    let {encrypted_tally; partial_decryptions; result; nb_tallied} = r in
    check_ciphertext encrypted_tally &&
    Array.forall2 (check_factor encrypted_tally)
      public_keys partial_decryptions &&
    let dummy = Array.mmap (fun _ -> G.one) encrypted_tally in
    let factors = Array.fold_left (fun a b ->
      Array.mmap2 ( *~ ) a b.decryption_factors
    ) dummy partial_decryptions in
    let results = Array.mmap2 (fun {beta; _} f ->
      beta / f
    ) encrypted_tally factors in
    Array.fforall2 (fun r1 r2 -> r1 =~ g **~ Z.of_int r2) results r.result

  let extract_tally r = r.result
end
