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

open Serializable_t

let compute_raw nchoices ballots =
  let result = Array.make_matrix nchoices nchoices 0 in
  Array.iter
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
      done
    ) ballots;
  result

(* See https://en.wikipedia.org/wiki/Schulze_method *)
let compute_strongest d =
  let c = Array.length d in
  let p = Array.make_matrix c c 0 in
  for i = 0 to c - 1 do
    for j = 0 to c - 1 do
      if i <> j then
        begin
          if d.(i).(j) > d.(j).(i) then
            p.(i).(j) <- d.(i).(j) - d.(j).(i)
          else
            p.(i).(j) <- 0
        end
    done
  done;
  for i = 0 to c - 1 do
    for j = 0 to c - 1 do
      if i <> j then
        for k = 0 to c - 1 do
          if i <> k && j <> k then
            p.(j).(k) <- max p.(j).(k) (min p.(j).(i) p.(i).(k))
        done
    done
  done;
  p

let sort_options x =
  let n = Array.length x in
  let rec mk_options n accu =
    if n >= 0 then mk_options (n - 1) (n :: accu) else accu
  in
  mk_options (n - 1) []
  |> List.sort (fun i j -> x.(j).(i) - x.(i).(j))

let compute_winners x =
  let sorted = Array.of_list (sort_options x) in
  let n = Array.length sorted in
  let rec main i accu =
    if i < n then
      let a = sorted.(i) in
      let i', level =
        let rec exaequos j accu =
          if j < n then
            let b = sorted.(j) in
            if x.(a).(b) = x.(b).(a) then
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

let compute ~nchoices ballots =
  let schulze_raw = compute_raw nchoices ballots in
  let schulze_strongest = compute_strongest schulze_raw in
  let schulze_winners = compute_winners schulze_strongest in
  {
    schulze_raw;
    schulze_strongest;
    schulze_winners;
  }
