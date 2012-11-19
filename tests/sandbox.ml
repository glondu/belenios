open Helios_datatypes_t

let ( |> ) x f = f x
let ( =~ ) = Z.equal

let hashZ x = Cryptokit.(x |>
  hash_string (Hash.sha1 ()) |>
  transform_string (Hexa.encode ()) |>
  Z.of_string_base 16
)

let hashB x = Cryptokit.(x |>
  hash_string (Hash.sha256 ()) |>
  transform_string (Base64.encode_compact ())
)

let array_forall2 f a b =
  let n = Array.length a in
  n = Array.length b &&
  (let rec check i =
     if i >= 0 then f a.(i) b.(i) && check (pred i)
     else true
   in check (pred n))

let array_foralli f x =
  let rec loop i =
    if i >= 0 then f i x.(i) && loop (pred i)
    else true
  in loop (pred (Array.length x))

let check_modulo p x = Z.(geq x zero && lt x p)
let check_subgroup p q x = Z.(powm x q p =~ one)

module type TYPES = sig
  type 'a t
  val read : 'a t -> Yojson.Safe.lexer_state -> Lexing.lexbuf -> 'a
  val write : 'a t -> Bi_outbuf.t -> 'a -> unit
  val election : election t
  val private_key : private_key t
  val trustee_public_key : trustee_public_key t
  val vote : vote t
  val encrypted_tally : encrypted_tally t
  val partial_decryption : partial_decryption t
  val election_public_data : election_public_data t
  val election_private_data : election_private_data t
end

module Types : TYPES = struct
  open Helios_datatypes_j
  type 'a t = (Yojson.Safe.lexer_state -> Lexing.lexbuf -> 'a) * (Bi_outbuf.t -> 'a -> unit)
  let read = fst
  let write = snd
  let election = (read_election, write_election)
  let private_key = (read_private_key, write_private_key)
  let trustee_public_key = (read_trustee_public_key, write_trustee_public_key)
  let vote = (read_vote, write_vote)
  let encrypted_tally = (read_encrypted_tally, write_encrypted_tally)
  let partial_decryption = (read_partial_decryption, write_partial_decryption)
  let election_public_data = (read_election_public_data, write_election_public_data)
  let election_private_data = (read_election_private_data, write_election_private_data)
end

let load typ fname =
  let i = open_in fname in
  let buf = Lexing.from_channel i in
  let lex = Yojson.init_lexer ~fname () in
  let result = Types.read typ lex buf in
  close_in i;
  result

let save typ fname x =
  let o = open_out fname in
  let buf = Bi_outbuf.create_channel_writer o in
  Types.write typ buf x;
  Bi_outbuf.flush_channel_writer buf;
  close_out o

let load_and_check ?(verbose=false) typ fname =
  if verbose then Printf.eprintf "Loading and checking %s...\n%!" fname;
  let thing = load typ fname in
  let tempfname = Filename.temp_file "belenios" ".json" in
  save typ tempfname thing;
  let r = Printf.ksprintf Sys.command "bash -c 'diff -u <(json_pp < %s) <(json_pp < %s)'" fname tempfname in
  assert (r = 0);
  Sys.remove tempfname;
  thing

type election_test_data = {
  fingerprint : string;
  election : election;
  public_data : election_public_data;
  private_data : election_private_data;
}

let first_line filename =
  let i = open_in filename in
  let r = input_line i in
  close_in i;
  r

let fix_fingerprint x =
  for i = 0 to String.length x - 1 do
    if x.[i] = '+' then x.[i] <- ' '
  done

let load_election_test_data ?(verbose=false) dirname =
  let data x = Filename.concat dirname x in
  let raw_json = first_line (data "election.json") in
  let fingerprint = hashB raw_json in
  fix_fingerprint fingerprint;
  let election = load_and_check ~verbose Types.election (data "election.json") in
  assert (
    let buf = Lexing.from_string raw_json in
    let lex = Yojson.init_lexer () in
    Types.read Types.election lex buf = election
  );
  let public_data = load_and_check ~verbose Types.election_public_data (data "public_data.json") in
  let private_data = load_and_check ~verbose Types.election_private_data (data "private_data.json") in
  { fingerprint; election; public_data; private_data }

let verify_public_key {g; p; q; y} =
  Z.probab_prime p 10 > 0 &&
  check_modulo p g &&
  check_modulo p y &&
  check_modulo p q &&
  check_subgroup p q g &&
  check_subgroup p q y

let verify_pok pk =
  let {g; p; q; y} = pk.trustee_public_key in
  let {pok_commitment; pok_challenge; pok_response} = pk.trustee_pok in
  let ( ** ) a b = Z.powm a b p in
  let ( * ) a b = Z.(a * b mod p) in
  check_modulo p pok_commitment &&
  check_modulo q pok_response &&
  g ** pok_response =~ pok_commitment * y ** pok_challenge &&
  pok_challenge =~ Z.(hashZ (Z.to_string pok_commitment) mod q)


let verify_disjunctive_proof pk big_g big_hs proof =
  let n = Array.length big_hs in
  n = Array.length proof &&
  let {g; p; q; y = h} = pk in
  let ( ** ) a b = Z.powm a b p in
  let ( * ) a b = Z.(a * b mod p) in
  assert (n > 0);
  (let rec check i commitments challenges =
     if i >= 0 then
       let {dp_commitment = {a; b}; dp_challenge; dp_response} = proof.(i) in
       (* FIXME: is it needed to do check_subgroup on a and b? *)
       check_modulo p a &&
       check_modulo p b &&
       check_modulo q dp_challenge &&
       check_modulo q dp_response &&
       g ** dp_response =~ big_g ** dp_challenge * a &&
       h ** dp_response =~ big_hs.(i) ** dp_challenge * b &&
       check (pred i) (Z.to_string a :: Z.to_string b :: commitments) Z.(challenges + dp_challenge)
     else
       let commitments = String.concat "," commitments in
       Z.(hashZ commitments mod q =~ challenges mod q)
   in check (pred n) [] Z.zero)

let verify_range pk min max alpha beta proof =
  let {g; p; q; y} = pk in
  Array.length proof = 2 &&
  let ( ** ) a b = Z.(powm a (of_int b) p) in
  let ( / ) a b = Z.(a * invert b p mod p) in
  let big_hs = Array.init (max-min+1) (fun i -> beta / (g ** (i-min))) in
  verify_disjunctive_proof pk alpha big_hs proof

let verify_answer pk question answer =
  let {q_max; q_min; q_answers; _} = question in
  (* FIXME: handle q_max = infinity *)
  let nb = Array.length q_answers in
  let {g; p; q; y} = pk in
  Array.length answer.choices = nb &&
  Array.length answer.individual_proofs = nb &&
  let ( * ) a b = Z.(a * b mod p) in
  (let rec check i alphas betas =
     if i >= 0 then
       let {alpha; beta} = answer.choices.(i) in
       check_subgroup p q alpha &&
       check_subgroup p q beta &&
       verify_range pk 0 1 alpha beta answer.individual_proofs.(i) &&
       check (pred i) (alphas * alpha) (betas * beta)
     else
       verify_range pk q_min q_max alphas betas answer.overall_proof
   in check (pred nb) Z.one Z.one)

let verify_vote e fingerprint v =
  v.election_hash = fingerprint &&
  e.e_uuid = v.election_uuid &&
  array_forall2 (verify_answer e.e_public_key) e.e_questions v.answers

let compute_encrypted_tally e vs =
  let {g; p; q; y} = e.e_public_key in
  let ( * ) a b = Z.(a * b mod p) in
  let ( *~ ) a b = Z.({ alpha = a.alpha * b.alpha mod p; beta = a.beta * b.beta mod p}) in
  let num_tallied = Array.length vs in
  let tally = Array.mapi (fun i question ->
    Array.mapi (fun j answer ->
      Array.fold_left (fun accu v ->
        accu *~ v.answers.(i).choices.(j)
      ) Z.({ alpha = one; beta = one}) vs
    ) question.q_answers
  ) e.e_questions in
  { num_tallied; tally }

let verify_proof_item challenge_generator g h g' h' p q proof =
  (* FIXME: factorize with verify_disjunctive_proof *)
  let ( ** ) a b = Z.(powm a b p) and ( * ) a b = Z.(a * b mod p) in
  let {dp_commitment = {a; b}; dp_challenge; dp_response} = proof in
  (* FIXME: is it needed to do check_subgroup on a and b? *)
  check_modulo p a &&
  check_modulo p b &&
  check_modulo q dp_challenge &&
  check_modulo q dp_response &&
  g ** dp_response =~ g' ** dp_challenge * a &&
  h ** dp_response =~ h' ** dp_challenge * b &&
  dp_challenge =~ challenge_generator a b

let verify_partial_decryption e tpk pds =
  let {g; p; q; y} = tpk.trustee_public_key in
  let {decryption_factors = dfs; decryption_proofs = dps} = pds in
  let challenge_generator a b =
    Z.(hashZ (to_string a ^ "," ^ to_string b) mod q)
  in
  let tally = e.public_data.encrypted_tally.tally in
  array_foralli (fun i question ->
    let dfs_i = dfs.(i) and dps_i = dps.(i) and tally_i = tally.(i) in
    array_foralli (fun j answer ->
      verify_proof_item challenge_generator g tally_i.(j).alpha y dfs_i.(j) p q dps_i.(j)
    ) question.q_answers
  ) e.election.e_questions

let verify_partial_decryptions e =
  array_forall2 (verify_partial_decryption e)
    e.public_data.public_keys
    e.public_data.partial_decryptions

let verify_election_public_key pk tpks =
  let n = Array.length tpks in
  assert (n > 0);
  let {g; p; q; y} = pk in
  let rec loop i accu =
    if i >= 0 then
      let tpk = tpks.(i) in
      let {g = g'; p = p'; q = q'; y = y'} = tpk.trustee_public_key in
      g =~ g' && p =~ p' && q =~ q' &&
      verify_pok tpk &&
      loop (pred i) Z.(accu * y' mod p)
    else accu =~ y
  in loop (pred n) Z.one

let verify_result e =
  let {g; p; q; y} = e.election.e_public_key in
  let pds = e.public_data.partial_decryptions in
  let tally = e.public_data.encrypted_tally.tally in
  let result = e.public_data.result in
  let ( * ) a b = Z.(a * b mod p) and ( ** ) a b = Z.(powm a (of_int b) p) in
  array_foralli (fun i question ->
    array_foralli (fun j answer ->
      let combined_factor = Array.fold_left (fun accu f ->
        accu * f.decryption_factors.(i).(j)
      ) Z.one pds in
      Z.invert combined_factor p * tally.(i).(j).beta =~ g ** result.(i).(j)
    ) question.q_answers
  ) e.election.e_questions

let verify_private_key k =
  let {x; public_key = {g; p; q; y}} = k in
  check_modulo q x && y =~ Z.powm g x p

let verbose_assert msg it =
  Printf.eprintf "Verifying %s...%!" msg;
  let r = Lazy.force it in
  Printf.eprintf " %s\n%!" (if r then "OK" else "failed!")

let load_election_and_verify_it_all dirname =
  let e = load_election_test_data ~verbose:true dirname in
  verbose_assert "public key"
    (lazy (verify_election_public_key
             e.election.e_public_key
             e.public_data.public_keys));
  verbose_assert "votes"
    (lazy (array_foralli
             (fun _ x -> verify_vote e.election e.fingerprint x)
             e.public_data.votes));
  verbose_assert "encrypted tally"
    (lazy (e.public_data.encrypted_tally =
        compute_encrypted_tally e.election e.public_data.votes));
  verbose_assert "partial decryptions"
    (lazy (verify_partial_decryptions e));
  verbose_assert "result"
    (lazy (verify_result e));
  verbose_assert "private keys"
    (lazy (array_foralli
             (fun _ k -> verify_private_key k)
             e.private_data.private_keys));;

let () = load_election_and_verify_it_all "tests/data/favorite-editor"
