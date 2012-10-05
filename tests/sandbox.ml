open Helios_datatypes_t

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

let () =
  assert (Sys.command "mkdir -p _build/tests/data" = 0)

let load_and_check typ fname =
  let one_thing = load typ fname in
  save typ (Filename.concat "_build" fname) one_thing;
  let r = Printf.ksprintf Sys.command "bash -c '\
    diff -u <(json_pp < %s) <(json_pp < _build/%s)
    '" fname fname
  in
  assert (r = 0);
  one_thing

let one_election = load_and_check Types.election "tests/data/election.json"
let one_trustee_private_key = load_and_check Types.private_key "tests/data/trustee-private-key.json"
let one_trustee_public_key = load_and_check Types.trustee_public_key "tests/data/trustee-public-key.json"
let vote_1 = load_and_check Types.vote "tests/data/vote-emacs-1.json"
let vote_2 = load_and_check Types.vote "tests/data/vote-emacs-2.json"
let encrypted_tally = load_and_check Types.encrypted_tally "tests/data/encrypted-tally.json"
let one_partial_decryption = load_and_check Types.partial_decryption "tests/data/partial-decryption.json"

let verify_public_key {g; p; q; y} =
  let ( = ) = Z.equal and ( ** ) a b = Z.powm a b p in
  Z.probab_prime p 10 > 0 &&
  g = Z.rem g p &&
  y = Z.rem y p &&
  g ** q = Z.one &&
  y ** q = Z.one &&
  true

let () = assert (verify_public_key one_trustee_public_key.trustee_public_key)

let dlog_challenge_generator q x =
  let ( |> ) x f = f x in
  Z.to_string x |>
  Cryptokit.(hash_string (Hash.sha1 ())) |>
  Cryptokit.(transform_string (Hexa.encode ())) |>
  Z.of_string_base 16 |>
  (fun x -> Z.rem x q)

let verify_trustee_pok pk =
  let {g; p; q; y} = pk.trustee_public_key in
  let {pok_commitment; pok_challenge; pok_response} = pk.trustee_pok in
  let ( = ) = Z.equal and ( ** ) a b = Z.powm a b p in
  let ( * ) a b = Z.(rem (a * b) p) in
  pok_commitment = Z.rem pok_commitment p &&
  pok_challenge = Z.rem pok_challenge q &&
  pok_response = Z.rem pok_response q &&
  g ** pok_response = pok_commitment * y ** pok_challenge &&
  let challenge = dlog_challenge_generator q pok_commitment in
  pok_challenge = challenge &&
  true

let () = assert (verify_trustee_pok one_trustee_public_key)

let verify_disjunct pk big_g big_h proof_item =
  let {g; p; q; y = h} = pk in
  let {dp_commitment = {a; b}; dp_challenge; dp_response} = proof_item in
  let ( = ) = Z.equal and ( ** ) a b = Z.powm a b p in
  let ( * ) a b = Z.(rem (a * b) p) in
  a = Z.rem a p &&
  b = Z.rem b p &&
  dp_challenge = Z.rem dp_challenge q &&
  (* dp_response = Z.rem dp_response q && *) (* FIXME *)
  (* g ** dp_response = big_g * a && *) (* FIXME *)
  (* h ** dp_response = big_h * b && *) (* FIXME *)
  true

let verify_disj_proof pk big_g big_hs proof =
  let n = Array.length proof in
  n = Array.length big_hs &&
  (let rec check i =
     i = n || (verify_disjunct pk big_g big_hs.(i) proof.(i) && check (i+1))
   in check 0)

let verify_zero_or_one pk ciphertext proof =
  let {g; p; q; y} = pk in
  let {alpha; beta} = ciphertext in
  Array.length proof = 2 &&
  let ( = ) = Z.equal and ( ** ) a b = Z.(powm a (of_int b) p) in
  let ( / ) a b = Z.(rem (a * invert b p) p) in
  let big_hs = Array.init 2 (fun i -> beta / (g ** i)) in
  verify_disj_proof pk alpha big_hs proof &&
  true

let verify_answer pk answer =
  verify_zero_or_one pk answer.choices.(0) answer.individual_proofs.(0)

let _ = verify_answer one_election.e_public_key vote_1.answers.(0)
