open Util
open Serializable_t

(* Setup group *)

module G = Election.DefaultGroup;;
assert (Election.check_finite_field G.group);;

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

let {g; p; q} = G.group

let uuid_of_string x = match Uuidm.of_string x with
  | Some y -> y
  | None -> invalid_arg "invalid UUID"

let params = {
  e_description = "This is a test election.";
  e_name = "Test election";
  e_public_key = {ffpk_g = g; ffpk_p = p; ffpk_q = q; ffpk_y = y};
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
  e_uuid = uuid_of_string "00000000-0000-0000-0000-000000000000";
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

open Serializable_j;;
save_to "election.json" (write_params write_ff_pubkey) params;;
save_to "metadata.json" write_metadata metadata;;
