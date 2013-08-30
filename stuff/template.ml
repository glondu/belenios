open Util
open Serializable_t

(* Setup group *)

module G = Election.DefaultGroup;;
assert G.(Election.check_finite_field ~p ~q ~g);;

module M = Election.MakeSimpleMonad(G);;

(* Setup trustees *)

module KG = Election.MakeSimpleDistKeyGen(G)(M);;

let public_keys =
  let ic = open_in "public_keys.jsons" in
  let raw_keys =
    let rec loop xs =
      match (try Some (input_line ic) with End_of_file -> None) with
      | Some x -> loop (x::xs)
      | None -> xs
    in loop []
  in
  close_in ic;
  let keys = List.map (fun x ->
    Serializable_j.trustee_public_key_of_string Serializable_builtin_j.read_number x
  ) raw_keys |> Array.of_list in
  assert (Array.forall KG.check keys);
  keys

let y = KG.combine public_keys

(* Setup election *)

let election = {
  e_description = "This is a test election.";
  e_name = "Test election";
  e_public_key = G.({g; p; q; y});
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
    e_voting_starts_at = now, None;
    e_voting_ends_at = now, None;
    e_voters_list = None;
  }
;;

(* Save to disk *)

let save_to filename writer x =
  let oc = open_out filename in
  let ob = Bi_outbuf.create_channel_writer oc in
  writer ob x;
  Bi_outbuf.flush_channel_writer ob;
  close_out oc;;

open Serializable_j;;
save_to "election.json" (write_election write_ff_pubkey) election;;
save_to "metadata.json" write_metadata metadata;;
