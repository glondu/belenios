open Util
open Serializable_t

let hashB x = Cryptokit.(x |>
  hash_string (Hash.sha256 ()) |>
  transform_string (Base64.encode_compact ())
);;

(* Setup group *)

module G = Election.DefaultGroup;;
assert (Election.check_finite_field G.group);;

module M = Election.MakeSimpleMonad(G);;

(* Setup trustees *)

module KG = Election.MakeSimpleDistKeyGen(G)(M);;

let private_keys, public_keys =
  let rec loop i accu =
    if i >= 0
    then loop (pred i) (KG.generate_and_prove () () :: accu)
    else (
      let a, b = List.split accu in
      Array.of_list a, Array.of_list b
    )
  in loop 1 []
;;
assert (Array.forall KG.check public_keys);;
let y = KG.combine public_keys;;

(* Setup election *)

let params = {
  e_description = "This is a test election.";
  e_name = "Test election";
  e_public_key = y;
  e_questions =
    [|
      {
        q_answers = [| "a"; "b"; "c"; "d"; "e" |];
        q_min = 0;
        q_max = 1;
        q_question = "Question 1?";
      };
      {
        q_answers = [| "a"; "b"; "c"; "d"; "e"; "f" |];
        q_min = 2;
        q_max = 3;
        q_question = "Question 2?";
      };
      {
        q_answers = [| "a"; "b"; "c" |];
        q_min = 1;
        q_max = 1;
        q_question = "Question 3?";
      };
    |];
  e_uuid = Uuidm.create `V4;
  e_short_name = "test";
};;

let metadata =
  let open CalendarLib.Fcalendar.Precise in
  let now = now () in
  {
    e_voting_starts_at = add now Period.(day 1), None;
    e_voting_ends_at = add now Period.(day 8), None;
    e_voters_list = None;
  }
;;

module P = struct
  module G = G
  let params = params
  let metadata = Some metadata
  let public_keys = Lazy.lazy_from_val (
    public_keys |> Array.map (fun x -> x.trustee_public_key)
  )
  let fingerprint =
    params |>
    Serializable_j.string_of_params Serializable_builtin_j.write_number |>
    hashB
end;;

module E = Election.MakeElection(P)(M);;

(* Vote *)

let vote b =
  try
    let b = E.create_ballot (E.make_randomness () ()) b () in
    let ok = E.check_ballot b in
    if ok then M.cast b "anonymous" ();
    ok
  with _ -> false
;;

assert (vote [|[| 0; 0; 0; 0; 0 |]; [| 0; 1; 0; 1; 1; 0 |]; [| 0; 0; 1 |]|]);;
assert (vote [|[| 0; 0; 1; 0; 0 |]; [| 0; 1; 0; 1; 1; 0 |]; [| 0; 0; 1 |]|]);;
assert (vote [|[| 0; 0; 0; 0; 0 |]; [| 0; 1; 0; 1; 1; 0 |]; [| 1; 0; 0 |]|]);;
assert (vote [|[| 1; 0; 0; 0; 0 |]; [| 0; 1; 0; 1; 0; 0 |]; [| 0; 1; 0 |]|]);;
assert (not (vote [|[| 0; 0; 0; 0; 0 |]; [| 0; 1; 0; 1; 1; 0 |]; [| 0; 0; 0 |]|]));;
assert (not (vote [|[| 0; 0; 1; 1; 0 |]; [| 0; 1; 0; 1; 1; 0 |]; [| 0; 0; 1 |]|]));;
assert (not (vote [|[| 0; 0; 0; 0; 0 |]; [| 0; 1; 0; 1; 1; 1 |]; [| 1; 0; 0 |]|]));;
assert (not (vote [|[| 1; 0; 0; 0; 0 |]; [| 0; 1; 0; 1; 0; 0 |]; [| 0; 1; 1 |]|]));;

(* Tally *)

let encrypted_tally = M.fold_ballots (fun b t ->
  M.return (E.combine_ciphertexts (E.extract_ciphertext b) t)
) E.neutral_ciphertext ();;

let factors = Array.map (fun x ->
  E.compute_factor encrypted_tally x ()
) private_keys;;
assert (Array.forall2 (E.check_factor encrypted_tally) (Lazy.force P.public_keys) factors);;

let result = E.combine_factors (M.turnout ()) encrypted_tally factors;;
assert (E.check_result result);;

let tally = E.extract_tally result;;
assert (tally = [|[| 1; 0; 1; 0; 0 |]; [|0; 4; 0; 4; 3; 0|]; [| 1; 1; 2 |]|]);;

(* Save to disk *)

let ( / ) = Filename.concat

let save_to filename writer x =
  let oc = open_out filename in
  let ob = Bi_outbuf.create_channel_writer oc in
  writer ob x;
  Bi_outbuf.flush_channel_writer ob;
  close_out oc;;

let list_save_to filename writer xs =
  let oc = open_out filename in
  let ob = Bi_outbuf.create_channel_writer oc in
  Array.iter (fun x ->
    writer ob x;
    Bi_outbuf.add_char ob '\n';
  ) xs;
  Bi_outbuf.flush_channel_writer ob;
  close_out oc;;

let save_to_disk () =
  let params = { params with
    e_public_key = G.({group; y})
  } in
  let ballots = Array.of_list (M.fold_ballots (fun x xs () -> x::xs) [] ()) in
  let dir = Printf.sprintf "demo/data/%s" (Uuidm.to_string params.e_uuid) in
  Unix.mkdir dir 0o755;
  let open Serializable_j in
  let number = Serializable_builtin_j.write_number in
  save_to (dir/"election.json") (write_params write_ff_pubkey) params;
  save_to (dir/"metadata.json") write_metadata metadata;
  list_save_to (dir/"private_keys.jsons") number private_keys;
  list_save_to (dir/"public_keys.jsons") (write_trustee_public_key number) public_keys;
  list_save_to (dir/"ballots.jsons") (write_ballot number) ballots;
  save_to (dir/"result.json") (write_result number) result;
  ();;
