open Util
open Serializable_t

let hashB x = Cryptokit.(x |>
  hash_string (Hash.sha256 ()) |>
  transform_string (Base64.encode_compact ())
);;

(* Setup group *)

let g = Z.of_string "14887492224963187634282421537186040801304008017743492304481737382571933937568724473847106029915040150784031882206090286938661464458896494215273989547889201144857352611058572236578734319505128042602372864570426550855201448111746579871811249114781674309062693442442368697449970648232621880001709535143047913661432883287150003429802392229361583608686643243349727791976247247948618930423866180410558458272606627111270040091203073580238905303994472202930783207472394578498507764703191288249547659899997131166130259700604433891232298182348403175947450284433411265966789131024573629546048637848902243503970966798589660808533";;
let p = Z.of_string "16328632084933010002384055033805457329601614771185955389739167309086214800406465799038583634953752941675645562182498120750264980492381375579367675648771293800310370964745767014243638518442553823973482995267304044326777047662957480269391322789378384619428596446446984694306187644767462460965622580087564339212631775817895958409016676398975671266179637898557687317076177218843233150695157881061257053019133078545928983562221396313169622475509818442661047018436264806901023966236718367204710755935899013750306107738002364137917426595737403871114187750804346564731250609196846638183903982387884578266136503697493474682071";;
let q = Z.of_string "61329566248342901292543872769978950870633559608669337131139375508370458778917";;
assert (Election.check_finite_field ~p ~q ~g);;

module G = (
  val Election.finite_field ~g ~p ~q : Signatures.GROUP with type t = Z.t
);;

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
  in loop 10 []
;;
assert (Array.forall KG.check public_keys);;
let y = KG.combine public_keys;;

(* Setup election *)

let election = {
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
  }
;;

module P = struct
  module G = G
  let params = election
  let metadata = Some metadata
  let public_keys = Lazy.lazy_from_val (
    public_keys |> Array.map (fun x -> x.trustee_public_key)
  )
  let fingerprint =
    election |>
    Serializable_j.string_of_election Serializable_builtin_j.write_number |>
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
  let election = { election with
    e_public_key = { g; p; q; y }
  } in
  let ballots = Array.of_list (M.fold_ballots (fun x xs () -> x::xs) [] ()) in
  let dir = Printf.sprintf "tests/data/{%s}"
    (Uuidm.to_string election.e_uuid)
  in
  Unix.mkdir dir 0o755;
  let open Serializable_j in
  let number = Serializable_builtin_j.write_number in
  save_to (dir/"election.json") (write_election write_ff_pubkey) election;
  save_to (dir/"metadata.json") write_metadata metadata;
  list_save_to (dir/"private_keys.jsons") number private_keys;
  list_save_to (dir/"public_keys.jsons") (write_trustee_public_key number) public_keys;
  list_save_to (dir/"ballots.jsons") (write_ballot number) ballots;
  save_to (dir/"result.json") (write_result number) result;
  ();;
