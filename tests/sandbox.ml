open Util
open Serializable_compat_t
open Common

module type TYPES = sig
  type elt
  type 'a t
  val read : 'a t -> Yojson.Safe.lexer_state -> Lexing.lexbuf -> 'a
  val write : 'a t -> Bi_outbuf.t -> 'a -> unit
  val election : elt election t
  val private_key : elt private_key t
  val trustee_public_key : elt trustee_public_key t
  val ballot : elt ballot t
  val encrypted_tally : elt encrypted_tally t
  val partial_decryption : elt partial_decryption t
  val election_public_data : elt election_public_data t
  val election_private_data : elt election_private_data t
end

module type SGROUP = sig
  type t
  val write : Bi_outbuf.t -> t -> unit
  val read : Yojson.Safe.lexer_state -> Lexing.lexbuf -> t
end

module SFiniteFieldMult : SGROUP with type t = Z.t = struct
  type t = Z.t
  let write = Core_datatypes_j.write_number
  let read = Core_datatypes_j.read_number
end

module MakeTypes (G : SGROUP) : TYPES with type elt = G.t = struct
  open Serializable_compat_j
  type elt = G.t
  type 'a t = (Yojson.Safe.lexer_state -> Lexing.lexbuf -> 'a) * (Bi_outbuf.t -> 'a -> unit)
  let read = fst
  let write = snd
  let election = (read_election G.read, write_election G.write)
  let private_key = (read_private_key G.read, write_private_key G.write)
  let trustee_public_key = (read_trustee_public_key G.read, write_trustee_public_key G.write)
  let ballot = (read_ballot G.read, write_ballot G.write)
  let encrypted_tally = (read_encrypted_tally G.read, write_encrypted_tally G.write)
  let partial_decryption = (read_partial_decryption G.read, write_partial_decryption G.write)
  let election_public_data = (read_election_public_data G.read, write_election_public_data G.write)
  let election_private_data = (read_election_private_data G.read, write_election_private_data G.write)
end

module Types : TYPES with type elt = Z.t = MakeTypes (SFiniteFieldMult)

let load typ fname = load_from_file (Types.read typ) fname

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

let load_election_private_data ?(verbose=false) dir uuid =
  Printf.ksprintf (Filename.concat dir) "{%s}/private.json" uuid |>
  load_and_check ~verbose Types.election_private_data

let verbose_assert msg it =
  Printf.eprintf "   %s...%!" msg;
  let r = Lazy.force it in
  Printf.eprintf " %s\n%!" (if r then "OK" else "failed!")

let verbose_verify_election_test_data (e, ballots, signatures, private_data) =
  Printf.eprintf "Verifying election %S:\n%!" e.election.e_short_name;
  let {g; p; q; y} = e.election.e_public_key in
  let module G = (val ElGamal.make_ff_msubgroup p q g : ElGamal.GROUP with type t = Z.t) in
  let module Crypto = ElGamal.Make (G) in
  verbose_assert "election key" (lazy (
    Crypto.check_election_key
      e.election.e_public_key.y
      e.public_data.public_keys
  ));
  if Array.length ballots = 0 then (
    Printf.eprintf "   no ballots available\n%!"
  ) else (
    verbose_assert "ballots" (lazy (
      Array.foralli (fun _ x ->
        Crypto.check_ballot e.election e.fingerprint x
      ) ballots
    ));
    (match e.public_data.election_result with
      | Some r ->
        verbose_assert "encrypted tally" (lazy (
          r.encrypted_tally = Crypto.compute_encrypted_tally e.election ballots
        ))
      | None -> ()
    );
  );
  (match e.public_data.election_result with
    | Some r ->
      verbose_assert "partial decryptions" (lazy (
        Crypto.check_partial_decryptions
          e.election e.public_data.public_keys r
      ));
      verbose_assert "result" (lazy (Crypto.check_result e.election r));
    | None -> Printf.eprintf "   no results available\n%!"
  );
  verbose_assert "signature count" (lazy (
    Array.length signatures = Array.length ballots
  ));
  verbose_assert "private keys" (lazy (
    Array.foralli
      (fun _ k -> Crypto.check_private_key k)
      private_data.private_keys
  ));;

let iter_keep f xs = List.iter f xs; xs;;

let load_election_and_verify_it_all dirname =
  load_elections_and_votes dirname |>
  Lwt_stream.to_list |> Lwt_main.run |>
  List.map (fun (e, ballots, signatures) ->
    let ballots = Lwt_stream.to_list ballots |> Lwt_main.run |> Array.of_list in
    let signatures = Lwt_stream.to_list signatures |> Lwt_main.run |> Array.of_list in
    let private_data = load_election_private_data dirname (Uuidm.to_string e.election.e_uuid) in
    (e, ballots, signatures, private_data)
  ) |>
  iter_keep verbose_verify_election_test_data;;

let all_data = load_election_and_verify_it_all "tests/data";;

let rec get_election name = function
  | [] -> raise Not_found
  | ((e, _, _, _) as x)::xs when e.election.e_short_name = name -> x
  | _::xs -> get_election name xs

let e, ballots, signatures, private_data = get_election "editor" all_data;;
let {g; p; q; y} = e.election.e_public_key
module G = (val ElGamal.make_ff_msubgroup p q g : ElGamal.GROUP with type t = Z.t)
module MyCrypto = ElGamal.Make (G)

let random_exponent =
  let pseudo = lazy Cryptokit.Random.(pseudo_rng (string secure_rng 20)) in
  (* 20 is 160 bits of entropy, taken from secure source *)
  fun () ->
    let raw = Cryptokit.Random.(string (Lazy.force pseudo) 32) in
    (* 32 is 256 bits of entropy, taken from pseudo-random source *)
    let hex = Cryptokit.(transform_string (Hexa.encode ()) raw) in
    Z.(of_string_base 16 hex mod q)

open G

let dummy_proof_item = {
  dp_commitment = { a = Z.one; b = Z.one };
  dp_challenge = Z.zero;
  dp_response = Z.zero;
}

let make_proof min max choice r {alpha; beta} =
  let n = max-min+1 in
  let j = choice-min in
  let proof = Array.create n dummy_proof_item in
  for i = 0 to n-1 do
    if i <> j then (
      let dp_challenge = random_exponent ()
      and dp_response = random_exponent () in
      let a = g **~ dp_response *~ inv (alpha **~ dp_challenge)
      and b = y **~ dp_response *~ inv ((beta *~ inv (g **~ Z.of_int i)) **~ dp_challenge) in
      proof.(i) <- { dp_commitment = {a; b}; dp_challenge; dp_response }
    )
  done;
  let w = random_exponent () in
  let a = g **~ w and b = y **~ w in
  let dp_challenge =
    let commitments = ref [] and challenges = ref Z.zero in
    for i = 0 to j-1 do
      let {a; b} = proof.(i).dp_commitment in
      commitments := b :: a :: !commitments;
      challenges := Z.(!challenges + proof.(i).dp_challenge);
    done;
    commitments := b :: a :: !commitments;
    for i = j+1 to n-1 do
      let {a; b} = proof.(i).dp_commitment in
      commitments := b :: a :: !commitments;
      challenges := Z.(!challenges + proof.(i).dp_challenge);
    done;
    Z.((G.hash (List.rev !commitments) + q - !challenges) mod q)
  in
  let dp_response = Z.((r * dp_challenge + w) mod q) in
  proof.(j) <- { dp_commitment = {a; b}; dp_challenge; dp_response };
  proof

let make_ballot e election_hash answers =
  let y = e.e_public_key.y in
  {
    answers =
      Array.mapi (fun i answer ->
        let randoms = Array.init (Array.length answer) (fun _ -> random_exponent ()) in
        let choices =
          Array.mapi (fun i choice ->
            assert (choice = 0 || choice = 1);
            let r = randoms.(i) in
            { alpha = g **~ r; beta = y **~ r *~ g **~ Z.of_int choice }
          ) answer
        in
        let individual_proofs =
          Array.mapi (fun i x -> make_proof 0 1 answer.(i) randoms.(i) x) choices
        in
        let min = e.e_questions.(i).q_min in
        let max = match e.e_questions.(i).q_max with
          | Some x -> x
          | None -> assert false (* FIXME *)
        in
        let overall_proof =
          let ( *- ) a b = Z.({ alpha = a.alpha * b.alpha; beta = a.beta * b.beta }) in
          let dummy_ciphertext = Z.({ alpha = one; beta = one}) in
          let sum_cleartexts = Array.fold_left ( + ) 0 answer in
          let sum_ciphertexts = Array.fold_left ( *- ) dummy_ciphertext choices in
          let sum_randoms = Z.(Array.fold_left ( + ) zero randoms) in
          make_proof min max sum_cleartexts sum_randoms sum_ciphertexts
        in
        { choices; individual_proofs; overall_proof }
      ) answers;
    election_hash;
    election_uuid = e.e_uuid;
  }

let b1 = make_ballot e.election e.fingerprint [| [| 1; 0; 0; 0 |] |];;
assert (MyCrypto.check_ballot e.election e.fingerprint b1);;

module P = struct
  module G = (val Crypto.finite_field ~p ~q ~g : Crypto_sigs.GROUP with type t = Z.t)
  let params = Serializable_compat.of_election e.election
  let fingerprint = e.fingerprint
end

module Election = Crypto.MakeHomomorphicElection(P)
module Compat = Serializable_compat.MakeCompat(P)

let nballots = Array.map Serializable_compat.of_ballot ballots;;
assert (Array.forall Election.check_ballot nballots);;
assert (Array.forall2 (fun b b' -> b = Compat.to_ballot b') ballots nballots);;

let create_ballot b =
  let randomness = Array.map (fun x ->
    Array.map (fun _ -> random q) x
  ) b in
  Election.create_ballot randomness b

let test_ballot = create_ballot [| [| 1; 0; 0; 0 |] |];;
assert (Election.check_ballot test_ballot);;
