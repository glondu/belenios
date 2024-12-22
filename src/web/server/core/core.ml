(**************************************************************************)
(*                                BELENIOS                                *)
(*                                                                        *)
(*  Copyright © 2012-2024 Inria                                           *)
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

open Lwt.Syntax
open Belenios
open Serializable_j

exception Race_condition
exception Election_not_found of uuid * string

let ( let&* ) x f = match x with None -> Lwt.return_none | Some x -> f x

let ( let*& ) x f =
  let* x = x in
  match x with None -> Lwt.return_none | Some x -> f x

let sleep = Lwt_unix.sleep

module Random = struct
  open Crypto_primitives

  let init_prng () = lazy (pseudo_rng (random_string secure_rng 16))
  let prng = ref (init_prng ())

  let () =
    let rec loop () =
      let* () = sleep 1800. in
      prng := init_prng ();
      loop ()
    in
    Lwt.async loop

  let get_rng () = Lazy.force !prng
end

include MakeGenerateToken (Random)
