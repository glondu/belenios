(**************************************************************************)
(*                                BELENIOS                                *)
(*                                                                        *)
(*  Copyright © 2012-2021 Inria                                           *)
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

open Serializable_j

let compute_matrix ~ngrades ~nchoices ~blank_allowed ballots =
  let n = Array.length ballots in
  let raw = Array.make_matrix nchoices ngrades 0 in
  let rec add_ballot i invalid blank valid =
    if i < n then (
      let ballot = ballots.(i) in
      assert (nchoices = Array.length ballot);
      let rec check j =
        if j < nchoices then (
          let grade = ballot.(j) in
          if 0 < grade && grade <= ngrades then check (j + 1) else j
        ) else j
      in
      let rec is_blank_ballot j =
        if j < nchoices then
          if ballot.(j) = 0 then
            is_blank_ballot (j + 1)
          else
            false
        else
          true
      in
      if check 0 = nchoices then (
        let rec fill j =
          if j < nchoices then (
            let grade = ballot.(j) - 1 in
            raw.(j).(grade) <- raw.(j).(grade) + 1;
            fill (j + 1)
          ) else ()
        in
        fill 0;
        add_ballot (i + 1) invalid blank (valid + 1)
      ) else if blank_allowed && is_blank_ballot 0 then (
        add_ballot (i + 1) invalid (blank + 1) valid
      ) else (
        add_ballot (i + 1) (ballot :: invalid) blank valid
      )
    ) else invalid, blank, valid
  in
  let invalid, blank, valid = add_ballot 0 [] 0 0 in
  let blank =
    if blank_allowed then (
      Some blank
    ) else (
      assert (blank = 0);
      None
    )
  in
  raw, Array.of_list invalid, blank, valid

let compute_increasing_vector grades =
  let sum = Array.fold_left ( + ) 0 grades in
  let res = Array.make sum (-1) in
  let ngrades = Array.length grades in
  let rec process i grade =
    if grade < ngrades then (
      let x = grades.(grade) in
      assert (i + x <= sum);
      let rec fill j n =
        if n > 0 then (
          res.(j) <- grade;
          fill (j + 1) (n - 1)
        ) else j
      in
      let j = fill i x in
      process j (grade + 1)
    ) else assert (i = sum)
  in
  process 0 0;
  res

let compute_median_sequence increasing_vector =
  let n = Array.length increasing_vector in
  let tmp = Array.copy increasing_vector in
  let res = Array.make n 0 in
  for i = 0 to n - 1 do
    let n' = n - i in
    let imedian = (n' + 1) / 2 - 1 in
    res.(i) <- tmp.(imedian);
    Array.blit tmp (imedian + 1) tmp imedian (n' - 1 - imedian);
  done;
  res

let lex_compare a b =
  let n = Array.length a in
  assert (n = Array.length b);
  let rec loop i =
    if i < n then (
      let x = a.(i) - b.(i) in
      if x = 0 then loop (i + 1) else x
    ) else 0
  in
  loop 0

let compute_winners matrix =
  let n = Array.length matrix in
  let sorted =
    matrix
    |> Array.map compute_increasing_vector
    |> Array.map compute_median_sequence
    |> Array.mapi (fun i x -> i, x)
  in
  Array.sort (fun (_, a) (_, b) -> lex_compare a b) sorted;
  let rec main i accu =
    if i < n then
      let a, aa = sorted.(i) in
      let i', level =
        let rec exaequos j accu =
          if j < n then
            let b, bb = sorted.(j) in
            if lex_compare aa bb = 0 then
              exaequos (j + 1) (b :: accu)
            else
              j, accu
          else
            j, accu
        in
        exaequos (i + 1) [a]
      in
      main i' (level :: accu)
    else
      List.rev accu
  in
  main 0 []

let compute ~ngrades ~nchoices ~blank_allowed ballots =
  let mj_raw, mj_invalid, mj_blank, mj_valid = compute_matrix ~ngrades ~nchoices ~blank_allowed ballots in
  let mj_winners = compute_winners mj_raw in
  {mj_raw; mj_valid; mj_blank; mj_invalid; mj_winners}
