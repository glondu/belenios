(**************************************************************************)
(*                                BELENIOS                                *)
(*                                                                        *)
(*  Copyright © 2023-2023 Inria                                           *)
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

open Js_of_ocaml
open Belenios_core.Serializable_j
open Belenios_core.Common
open Belenios_js.Common
open Belenios_js.Messages

let handle_shuffle { election; ciphertexts } =
  let election = Js.to_string election in
  let ciphertexts = Js.to_string ciphertexts in
  let module W =
    Belenios.Election.Make
      (struct
        let raw_election = election
      end)
      (Random)
      ()
  in
  let ciphertexts =
    nh_ciphertexts_of_string W.(sread G.of_string) ciphertexts
  in
  let nballots =
    if Array.length ciphertexts > 0 then Array.length ciphertexts.(0) else 0
  in
  let threshold = 5 in
  let@ () =
   fun cont ->
    if nballots > threshold then (
      let sub = Array.map (fun x -> Array.sub x 0 threshold) ciphertexts in
      let start = new%js Js.date_now in
      let _ = W.E.shuffle_ciphertexts sub in
      let stop = new%js Js.date_now in
      let delta = (stop##valueOf -. start##valueOf) /. 1000. in
      let eta =
        int_of_float
          (ceil (float_of_int nballots *. delta /. float_of_int threshold))
      in
      Worker.post_message (ShuffleEstimate eta);
      cont ())
    else cont ()
  in
  W.E.shuffle_ciphertexts ciphertexts
  |> string_of_shuffle W.(swrite G.to_string)
  |> fun r -> Worker.post_message (ShuffleResult (Js.string r))

let handle_request = function Shuffle r -> handle_shuffle r
let () = Worker.set_onmessage handle_request
