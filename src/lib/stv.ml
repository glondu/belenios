(**************************************************************************)
(*                                BELENIOS                                *)
(*                                                                        *)
(*  Copyright © 2012-2020 Inria                                           *)
(*                                                                        *)
(*  This program is free software: you can redistribute it and/or modify  *)
(*  it under the terms of the GNU Affero General Public License as        *)
(*  published by the Free Software Foundation, either version 3 of the    *)
(*  License, or (at your option) any later version, with the additional   *)
(*  exemption that compiling, linking, and/or using OpenSSL is allowed.   *)
(*                                                                        *)
(*  This program is distributed in the hope that it will be useful, but   *)
(*  WITHOUT ANY WARRANTY; without even the implied warranty of            *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU     *)
(*  Affero General Public License for more details.                       *)
(*                                                                        *)
(*  You should have received a copy of the GNU Affero General Public      *)
(*  License along with this program.  If not, see                         *)
(*  <http://www.gnu.org/licenses/>.                                       *)
(**************************************************************************)

open Serializable_t

(** Transform a ballot in belenios format (e.g. [4,1,2,5,3]) into a
   list of choices (represented as their index in the vector) in
   preference order (e.g. [1,2,4,0,3]) *)
let process ballot =
  let nchoices = Array.length ballot in
  let used = Array.make nchoices false in
  let rec lookup rank i =
    if i < nchoices then (
      if ballot.(i) = rank then (
        assert (not (used.(i)));
        used.(i) <- true;
        Some i
      ) else lookup rank (i + 1)
    ) else None
  in
  let rec build_preference_list rank accu =
    match lookup rank 0 with
    | Some i -> build_preference_list (rank + 1) (i :: accu)
    | None -> List.rev accu
  in
  let preference_list = build_preference_list 1 [] in
  let rec check i =
    if i < nchoices then (
      if used.(i) || ballot.(i) = 0 then (
        check (i + 1)
      ) else false
    ) else true
  in
  if check 0 then Some preference_list else None

module IntMap = Map.Make (struct type t = int let compare = compare end)

(** Here, [choices] is a map from choices to ballots that has them as
   first choice. This function updates [choices] with a new ballot: it
   assigns the ballot to its first choice, and makes sure all other
   choices also exist in [choices]. *)
let assign choices ((_, ballot) as b) =
  let prepend ballots choices x =
    match IntMap.find_opt x choices with
    | None -> IntMap.add x ballots choices
    | Some bs -> IntMap.add x (List.rev_append ballots bs) choices
  in
  match ballot with
  | [] -> choices
  | x :: xs -> List.fold_left (prepend []) (prepend [b] choices x) xs

(** Here, [scores] is an association list mapping from choices to
   ballots and total score. This function collects all the ballots,
   filters out [i] from them, and multiplies [i]'s ballots by
   [coef]. *)
let transfer coef i scores =
  List.fold_left
    (fun accu (ai, (ab, _)) ->
      List.fold_left
        (fun accu (w, b) ->
          let w = if ai = i then w *. coef else w in
          let b = List.filter (fun x -> x <> i) b in
          (w, b) :: accu
        ) accu ab
    ) [] scores

(** This function performs a round of the STV algorithm. It
   tail-recursively calls itself until [nseats] is [0] or there is not
   enough remaining choices, and returns the list of events
   (Win|Lose|TieWin|TieLose) that occured during the process. *)
let rec run quota ballots events nseats =
  if nseats > 0 then (
    let choices = List.fold_left assign IntMap.empty ballots in
    if IntMap.cardinal choices <= nseats then (
      (* there is not enough choices: they all win *)
      choices
      |> IntMap.bindings
      |> List.map fst
      |> (fun x -> `Win x :: events)
      |> List.rev
    ) else (
      let scores =
        (* for each choice, compute the sum of scores of its assigned
           ballots *)
        choices
        |> IntMap.map
             (fun bs ->
               bs, List.fold_left (fun accu (w, _) -> accu +. w) 0. bs
             )
        |> IntMap.bindings
        |> List.sort
             (* we sort the choices, with greater total score first,
                then in question order (this is our "arbitrary" tie
                breaking, chosen by the election administrator) *)
             (fun (ai, (_, aw)) (bi, (_, bw)) ->
               compare (bw, ai) (aw, bi)
             )
      in
      match scores with
      | (ai, (_, aw)) :: xs when aw >= quota ->
         (* the first choice is above the quota *)
         let events =
           match xs with
           | (bi, (_, bw)) :: _ when aw = bw ->
              (* the second choice has the same total score, we chose
                 the first one, but log the tie *)
              `TieWin [ai; bi] :: events
           | _ -> events
         in
         (* note that we select a single winner, even if there are
            several choices above quota *)
         let c = (aw -. quota) /. aw in
         run quota (transfer c ai scores) (`Win [ai] :: events) (nseats - 1)
      | scores ->
         match List.rev scores with
         | (ai, (_, aw)) :: xs ->
            (* we select the last choice *)
            let events =
              match xs with
              | (bi, (_, bw)) :: _ when aw = bw ->
                 (* the second last choice has the same total score,
                    we chose the last one, but log the tie *)
                 `TieLose [ai; bi] :: events
              | _ -> events
            in
            run quota (transfer 1. ai scores) (`Lose ai :: events) nseats
         | [] ->
            (* should not happen, because if there is no choices left,
               the condition "there is not enough choices" above
               should have been triggered *)
            assert false
    )
  ) else List.rev events

let compute ~nseats ballots =
  let nballots = Array.length ballots in
  let rec partition accu invalid i =
    if i < nballots then (
      let ballot = ballots.(i) in
      match process ballot with
      | None -> partition accu (ballot :: invalid) (i + 1)
      | Some x -> partition (x :: accu) invalid (i + 1)
    ) else (
      List.sort compare accu,
      Array.of_list (List.sort compare invalid)
    )
  in
  let stv_ballots, stv_invalid = partition [] [] 0 in
  let n = List.length stv_ballots in
  let quota = floor (float n /. float (nseats + 1)) +. 1. in
  let wballots = List.map (fun b -> 1., b) stv_ballots in
  let stv_events = run quota wballots [] nseats in
  let stv_winners =
    stv_events
    |> List.map
         (function
          | `Win x -> x
          | _ -> []
         )
    |> List.flatten
  in
  assert
    (if n > 0 then (
       let nwinners = List.length stv_winners in
       let nchoices = Array.length ballots.(0) in
       if nchoices > nseats then
         nwinners = nseats
       else
         nwinners = nchoices
     ) else stv_winners = []
    );
  {stv_ballots; stv_invalid; stv_events; stv_winners}
