(**************************************************************************)
(*                                BELENIOS                                *)
(*                                                                        *)
(*  Copyright © 2012-2022 Inria                                           *)
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

(** A module for computing the Schulze method using both winning and
   losing votes (losing votes are purely secondary in the
   ordering). *)

(* References:
   For the theory, see
   [Wikipedia](https://en.wikipedia.org/wiki/Schulze_method). However,
   here, we use the implementation of
   [CIVS](https://github.com/andrewcmyers/civs/blob/master/cgi-bin/beatpath2.pm).
*)

open Serializable_t

(* Definitions:
   The strength of a direct beat by choice A over choice B is a pair
   (W,L) where W>L and W is the number of ballots that rank A over B,
   and L is the number of ballots that rank B over A.

   Direct beats are totally ordered, with losing votes mattering
   only if winning votes are tied:
   (W1,L1) > (W2,L2) iff (W1 > W2) or (W1 = W2 and L1 < L2)
*)

module Beat = struct
  let compare (w1, l1) (w2, l2) =
    if w1 > w2 then 1
    else if w1 < w2 then -1
    else if l1 < l2 then 1
    else if l1 > l2 then -1
    else 0

  let min x y = if compare x y > 0 then y else x
  let max x y = if compare x y > 0 then x else y
end

(** [compute_raw n ballots] computes the raw preference matrix [m],
   where [m.(i).(j)] is the number of ballots where [i] beats [j]. *)
let compute_raw nchoices ballots =
  let result = Array.make_matrix nchoices nchoices 0 in
  List.iter
    (fun ballot ->
      assert (nchoices = Array.length ballot);
      let get i =
        let x = ballot.(i) in
        if x = 0 then max_int else x
      in
      for i = 0 to nchoices - 2 do
        let x = get i in
        for j = i + 1 to nchoices - 1 do
          let y = get j in
          if x < y then result.(i).(j) <- result.(i).(j) + 1
          else if y < x then result.(j).(i) <- result.(j).(i) + 1
        done
      done)
    ballots;
  result

(* The strength of a path (beatpath) is the min of the strengths of
   all the links along the path. To compare two choices, we look at
   the max of all the beatpaths between them.  If the max of the
   beatpaths from A to B is stronger than the max of all the beatpaths
   from B to A, then A is ranked above B. *)

(** [compute_initial_matrix m] returns a matrix which is the initial
   starting point for the Floyd-Warshall algorithm.  Input [m] is a
   reference to an n-by-n matrix.  For any given pair of elements ij
   and ji in [m], at most one is initialized to something other than
   (0,0): the one that contains a larger value in [m]. That element is
   initialized to a pair containing the larger and the smaller of the
   two values.  Thus, diagonal elements are initialized to (0,0); if
   m_ij=m_ji, both are initialized to (0,0).  *)
let compute_initial_matrix m =
  let n = Array.length m in
  let r = Array.make_matrix n n (0, 0) in
  for i = 0 to n - 1 do
    for j = 0 to n - 1 do
      let x = m.(i).(j) and y = m.(j).(i) in
      if x > y then r.(i).(j) <- (x, y) else if x < y then r.(j).(i) <- (y, x)
    done
  done;
  r

(** [transitive_closure m] computes the transitive (beatpath) closure
   of the square matrix referenced by $m.  Result is destructively
   returned in $m itself.

 Implementation: Computes the transitive closure with ratings, using
   the Floyd-Warshall algorithm, but with min = max, + = min.  This
   gives the necessary commutative semiring. Run time is O(n^3).  A
   classic of dynamic programming.  *)
let compute_transitive_closure m =
  let n = Array.length m in
  for k = 0 to n - 1 do
    for i = 0 to n - 1 do
      for j = 0 to n - 1 do
        (* consider going from i to j via k, comparing to existing path *)
        m.(i).(j) <- Beat.(max m.(i).(j) (min m.(i).(k) m.(k).(j)))
      done
    done
  done

(** [winner m ignore] returns the winners, according to the transitive
   beatpath closure in [m]. These are the choices that are
   unbeaten. Choices whose corresponding entry in [ignore] is [true]
   are ignored, others are considered both as possible winners and as
   beaters.  *)
let compute_winners m ignore =
  let n = Array.length m in
  let winners = ref [] in
  for i = 0 to n - 1 do
    if not ignore.(i) then (
      let won = ref true in
      (try
         for j = 0 to n - 1 do
           if not ignore.(j) then
             if Beat.compare m.(j).(i) m.(i).(j) > 0 then (
               won := false;
               raise Exit)
         done
       with Exit -> ());
      if !won then winners := i :: !winners)
  done;
  List.rev !winners

(** [rank_candidates raw] ranks the choices using the raw information
   in [raw], according to the beatpath winner criterion. *)
let rank_candidates raw =
  let n = Array.length raw in
  let beatpaths = compute_initial_matrix raw in
  let ignore = Array.make n false in
  compute_transitive_closure beatpaths;
  let num_ranked = ref 0 and result = ref [] in
  while !num_ranked < n do
    let winners = compute_winners beatpaths ignore in
    result := winners :: !result;
    List.iter
      (fun j ->
        ignore.(j) <- true;
        incr num_ranked)
      winners
  done;
  (List.rev !result, beatpaths)

let compute ~nchoices ~blank_allowed ballots =
  let n = Array.length ballots in
  let ballots = Array.to_list ballots in
  let null_ballot = Array.make nchoices 0 in
  let schulze_valid, schulze_blank, ballots =
    if blank_allowed then
      let rec loop valid blank ballots = function
        | [] -> (valid, Some blank, ballots)
        | ballot :: xs ->
            if ballot = null_ballot then loop valid (blank + 1) ballots xs
            else loop (valid + 1) blank (ballot :: ballots) xs
      in
      loop 0 0 [] ballots
    else (n, None, ballots)
  in
  let schulze_raw = compute_raw nchoices ballots in
  let schulze_winners, schulze_beatpaths = rank_candidates schulze_raw in
  {
    schulze_raw;
    schulze_valid;
    schulze_blank;
    schulze_beatpaths;
    schulze_winners;
  }
