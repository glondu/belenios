(**************************************************************************)
(*                                BELENIOS                                *)
(*                                                                        *)
(*  Copyright © 2012-2023 Inria                                           *)
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

open Belenios_platform
open Belenios_core
open Platform
open Common
open Serializable_core_t
open Signatures

module Make (W : ELECTION_DATA with type question := Question.t) (M : RANDOM) =
struct
  module G = W.G
  open G

  let random () = M.random Zq.q |> Zq.coerce
  let randoms n = Array.init n (fun _ -> random ())

  let gen_permutation n =
    let tmp = Array.init n (fun i -> i) in
    let psi = Array.make n 0 in
    let rec loop i =
      if i < n then (
        let k = M.random (Z.of_int Stdlib.(n - i)) in
        let k = Stdlib.(Z.to_int k + i) in
        psi.(i) <- tmp.(k);
        tmp.(k) <- tmp.(i);
        loop (succ i))
      else psi
    in
    loop 0

  let re_encrypt y { alpha; beta } r =
    { alpha = alpha *~ (g **~ r); beta = beta *~ (y **~ r) }

  let gen_shuffle y e =
    let n = Array.length e in
    let psi = gen_permutation n in
    let r = randoms n in
    let e = Array.map2 (re_encrypt y) e r in
    let e = Array.init n (fun i -> e.(psi.(i))) in
    (e, r, psi)

  let gen_permutation_commitment psi h =
    let n = Array.length psi in
    let c = Array.make n G.one and r = Array.make n Zq.zero in
    let rec loop i =
      if i < n then (
        let r_ = random () in
        r.(psi.(i)) <- r_;
        c.(psi.(i)) <- (g **~ r_) *~ h.(i);
        loop (succ i))
      else (c, r)
    in
    loop 0

  let get_nizkp_challenges n str =
    let h = sha256_hex str in
    Array.init n (fun i ->
        let i = sha256_hex (string_of_int i) in
        Zq.reduce_hex (sha256_hex (h ^ i)))

  let get_nizkp_challenge str = Zq.reduce_hex (sha256_hex str)

  let str_egs e =
    let b = Buffer.create 1024 in
    for i = 0 to pred (Array.length e) do
      let { alpha; beta } = e.(i) in
      Printf.bprintf b "%s,%s," (G.to_string alpha) (G.to_string beta)
    done;
    Buffer.contents b

  let str_elts c =
    let b = Buffer.create 1024 in
    for i = 0 to pred (Array.length c) do
      Printf.bprintf b "%s," (G.to_string c.(i))
    done;
    Buffer.contents b

  let gen_commitment_chain c0 uu =
    let n = Array.length uu in
    let rr = randoms n in
    let cc = Array.make n G.one in
    let rec loop i =
      if i < n then (
        let ccpred = if i = 0 then c0 else cc.(pred i) in
        cc.(i) <- (g **~ rr.(i)) *~ (ccpred **~ uu.(i));
        loop (succ i))
      else (cc, rr)
    in
    loop 0

  module GMap = Map.Make (G)

  let make_get_generator_indep () =
    let to_avoid = ref GMap.empty in
    fun n ->
      let x = G.get_generator n in
      match GMap.find_opt x !to_avoid with
      | None ->
          to_avoid := GMap.add x n !to_avoid;
          x
      | Some n' ->
          Printf.ksprintf failwith "Generator #%d collides with #%d!" n n'

  let gen_shuffle_proof y ee ee' rr' psi =
    let get_generator_indep = make_get_generator_indep () in
    let n = Array.length ee in
    let h = get_generator_indep (-1) in
    assert (n = Array.length ee');
    assert (n = Array.length rr');
    assert (n = Array.length psi);
    let hh = Array.init n get_generator_indep in
    let cc, rr = gen_permutation_commitment psi hh in
    let str1 = str_egs ee ^ str_egs ee' ^ str_elts cc in
    let uu =
      get_nizkp_challenges n ("shuffle-challenges|" ^ W.fingerprint ^ "|" ^ str1)
    in
    let uu' = Array.init n (fun i -> uu.(psi.(i))) in
    let cc_hat, rr_hat = gen_commitment_chain h uu' in
    let w1 = random () in
    let w2 = random () in
    let w3 = random () in
    let w4 = random () in
    let ww_hat = randoms n in
    let ww' = randoms n in
    let t1 = g **~ w1 and t2 = g **~ w2 in
    let t3_ = Array.map2 ( **~ ) hh ww' in
    let t3 = Array.fold_left ( *~ ) (g **~ w3) t3_ in
    let t41_ = Array.map2 (fun e' w' -> e'.beta **~ w') ee' ww' in
    let t41 = Array.fold_left ( *~ ) (invert (y **~ w4)) t41_ in
    let t42_ = Array.map2 (fun e' w' -> e'.alpha **~ w') ee' ww' in
    let t42 = Array.fold_left ( *~ ) (invert (g **~ w4)) t42_ in
    let cc_hat' =
      Array.init n (fun i -> if i = 0 then h else cc_hat.(pred i))
    in
    let tt_hat =
      Array.map3
        (fun w_hat w' c_hat -> (g **~ w_hat) *~ (c_hat **~ w'))
        ww_hat ww' cc_hat'
    in
    let t = (t1, t2, t3, (t41, t42), tt_hat) in
    let str2 = str_elts [| t1; t2; t3; t41; t42 |] ^ str_elts tt_hat in
    let str3 = str1 ^ str_elts cc_hat ^ G.to_string y in
    let c =
      get_nizkp_challenge
        ("shuffle-challenge|" ^ W.fingerprint ^ "|" ^ str2 ^ str3)
    in
    let r_bar = Zq.(Array.fold_left ( + ) zero rr) in
    let s1 = Zq.(w1 + (c * r_bar)) in
    let vv = Array.make n Zq.one in
    for i = n - 2 downto 0 do
      vv.(i) <- Zq.(uu'.(succ i) * vv.(succ i))
    done;
    let r_hat = Zq.(Array.fold_left ( + ) zero (Array.map2 ( * ) rr_hat vv)) in
    let s2 = Zq.(w2 + (c * r_hat)) in
    let r_tilde = Zq.(Array.fold_left ( + ) zero (Array.map2 ( * ) rr uu)) in
    let s3 = Zq.(w3 + (c * r_tilde)) in
    let r' = Zq.(Array.fold_left ( + ) zero (Array.map2 ( * ) rr' uu)) in
    let s4 = Zq.(w4 + (c * r')) in
    let ss_hat = Array.init n (fun i -> Zq.(ww_hat.(i) + (c * rr_hat.(i)))) in
    let ss' = Array.init n (fun i -> Zq.(ww'.(i) + (c * uu'.(i)))) in
    let s = (s1, s2, s3, s4, ss_hat, ss') in
    (t, s, cc, cc_hat)

  let check_shuffle_proof y ee ee' proof =
    let get_generator_indep = make_get_generator_indep () in
    let n = Array.length ee in
    let h = get_generator_indep (-1) in
    n = Array.length ee'
    &&
    let t, s, cc, cc_hat = proof in
    let t1, t2, t3, (t41, t42), tt_hat = t in
    let s1, s2, s3, s4, ss_hat, ss' = s in
    Array.for_all G.check [| t1; t2; t3; t41; t42 |]
    && n = Array.length cc
    && n = Array.length cc_hat
    && n = Array.length tt_hat
    && n = Array.length ss_hat
    && n = Array.length ss'
    && Array.for_all G.check cc
    && Array.for_all G.check cc_hat
    && Array.for_all G.check tt_hat
    &&
    let hh = Array.init n get_generator_indep in
    let str1 = str_egs ee ^ str_egs ee' ^ str_elts cc in
    let uu =
      get_nizkp_challenges n ("shuffle-challenges|" ^ W.fingerprint ^ "|" ^ str1)
    in
    let str2 = str_elts [| t1; t2; t3; t41; t42 |] ^ str_elts tt_hat in
    let str3 = str1 ^ str_elts cc_hat ^ G.to_string y in
    let c =
      get_nizkp_challenge
        ("shuffle-challenge|" ^ W.fingerprint ^ "|" ^ str2 ^ str3)
    in
    let c_bar =
      Array.fold_left ( *~ ) one cc *~ invert (Array.fold_left ( *~ ) one hh)
    in
    let u = Zq.(Array.fold_left ( * ) one uu) in
    let c_hat = (if n = 0 then h else cc_hat.(pred n)) *~ invert (h **~ u) in
    let c_tilde = Array.fold_left ( *~ ) one (Array.map2 ( **~ ) cc uu) in
    let a' =
      Array.fold_left ( *~ ) one (Array.map2 (fun x u -> x.beta **~ u) ee uu)
    in
    let b' =
      Array.fold_left ( *~ ) one (Array.map2 (fun x u -> x.alpha **~ u) ee uu)
    in
    let t1' = invert (c_bar **~ c) *~ (g **~ s1) in
    let t2' = invert (c_hat **~ c) *~ (g **~ s2) in
    let t3' =
      invert (c_tilde **~ c)
      *~ Array.fold_left ( *~ ) (g **~ s3) (Array.map2 ( **~ ) hh ss')
    in
    let t41' =
      Array.fold_left ( *~ )
        (invert ((a' **~ c) *~ (y **~ s4)))
        (Array.map2 (fun x s -> x.beta **~ s) ee' ss')
    in
    let t42' =
      Array.fold_left ( *~ )
        (invert ((b' **~ c) *~ (g **~ s4)))
        (Array.map2 (fun x s -> x.alpha **~ s) ee' ss')
    in
    let tt'_hat =
      Array.init n (fun i ->
          let x = if i = 0 then h else cc_hat.(pred i) in
          invert (cc_hat.(i) **~ c) *~ (g **~ ss_hat.(i)) *~ (x **~ ss'.(i)))
    in
    G.compare t1 t1' = 0
    && G.compare t2 t2' = 0
    && G.compare t3 t3' = 0
    && G.compare t41 t41' = 0
    && G.compare t42 t42' = 0
    && Array.for_all2 (fun t t' -> G.compare t t' = 0) tt_hat tt'_hat
end
