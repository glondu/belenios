open Util
open Serializable_t

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
  e_public_key = {g; p; q; y};
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
