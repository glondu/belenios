(**************************************************************************)
(*                                BELENIOS                                *)
(*                                                                        *)
(*  Copyright © 2012-2019 Inria                                           *)
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

open Platform
open Common
open Serializable_core_t
open Signatures

module Make (M : RANDOM) (G : GROUP) = struct
  open G
  let ( >>= ) = M.bind

  let mmap2 f a b =
    let n = Array.length a in
    assert (n = Array.length b);
    if n > 0 then
      let r = Array.make n (f a.(0) b.(0)) in
      M.yield () >>= fun () ->
      let rec loop i =
        if i < n then (
          r.(i) <- f a.(i) b.(i);
          M.yield () >>= fun () ->
          loop (succ i)
        ) else M.return r
      in
      loop 1
    else M.return [||]

  let mmap3 f a b c =
    let n = Array.length a in
    assert (n = Array.length b);
    assert (n = Array.length c);
    if n > 0 then
      let r = Array.make n (f a.(0) b.(0) c.(0)) in
      M.yield () >>= fun () ->
      let rec loop i =
        if i < n then (
          r.(i) <- f a.(i) b.(i) c.(i);
          M.yield () >>= fun () ->
          loop (succ i)
        ) else M.return r
      in
      loop 1
    else M.return [||]

  let randoms n =
    let res = Array.make n Z.zero in
    let rec loop i =
      if i < n then
        M.random G.q >>= fun x ->
        res.(i) <- x;
        loop (succ i)
      else
        M.return res
    in
    loop 0

  let gen_permutation n =
    let tmp = Array.init n (fun i -> i) in
    let psi = Array.make n 0 in
    let rec loop i =
      if i < n then
        M.random (Z.of_int Stdlib.(n - i)) >>= fun k ->
        let k = Stdlib.(Z.to_int k + i) in
        psi.(i) <- tmp.(k);
        tmp.(k) <- tmp.(i);
        loop (succ i)
      else
        M.return psi
    in
    loop 0

  let re_encrypt y {alpha; beta} r =
    {
      alpha = alpha *~ (g **~ r);
      beta = beta *~ (y **~ r);
    }

  let gen_shuffle y e =
    let n = Array.length e in
    gen_permutation n >>= fun psi ->
    randoms n >>= fun r ->
    mmap2 (re_encrypt y) e r >>= fun e ->
    let e = Array.init n (fun i -> e.(psi.(i))) in
    M.return (e, r, psi)

  let gen_permutation_commitment psi h =
    let n = Array.length psi in
    let c = Array.make n G.one and r = Array.make n Z.zero in
    let rec loop i =
      if i < n then
        M.random G.q >>= fun r_ ->
        r.(psi.(i)) <- r_;
        c.(psi.(i)) <- (g **~ r_) *~ h.(i);
        loop (succ i)
      else
        M.return (c, r)
    in loop 0

  let get_nizkp_challenges n str =
    let h = sha256_hex str in
    Array.init n (fun i ->
        let i = sha256_hex (string_of_int i) in
        Z.(of_string_base 16 (sha256_hex (h ^ i)) mod G.q)
      )

  let get_nizkp_challenge str =
    let h = sha256_hex str in
    Z.(of_string_base 16 h mod G.q)

  let str_egs e =
    let b = Buffer.create 1024 in
    for i = 0 to pred (Array.length e) do
      let {alpha; beta} = e.(i) in
      Printf.bprintf b "%s,%s," (G.to_string alpha) (G.to_string beta);
    done;
    Buffer.contents b

  let str_elts c =
    let b = Buffer.create 1024 in
    for i = 0 to pred (Array.length c) do
      Printf.bprintf b "%s," (G.to_string c.(i));
    done;
    Buffer.contents b

  let gen_commitment_chain c0 uu =
    let n = Array.length uu in
    randoms n >>= fun rr ->
    let cc = Array.make n G.one in
    let rec loop i =
      if i < n then (
        let ccpred = if i = 0 then c0 else cc.(pred i) in
        cc.(i) <- (g **~ rr.(i)) *~ (ccpred **~ uu.(i));
        M.yield () >>= fun () ->
        loop (succ i)
      ) else M.return (cc, rr)
    in
    loop 0

  module GMap = Map.Make (G)

  let make_get_generator_indep () =
    let to_avoid = ref GMap.empty in
    fun n ->
    let x = G.get_generator n in
    match GMap.find_opt x !to_avoid with
    | None -> to_avoid := GMap.add x n !to_avoid; x
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
    gen_permutation_commitment psi hh >>= fun (cc, rr) ->
    let str1 = str_egs ee ^ str_egs ee' ^ str_elts cc in
    let uu = get_nizkp_challenges n ("shuffle-challenges|" ^ str1) in
    let uu' = Array.init n (fun i -> uu.(psi.(i))) in
    gen_commitment_chain h uu' >>= fun (cc_hat, rr_hat) ->
    M.random G.q >>= fun w1 ->
    M.random G.q >>= fun w2 ->
    M.random G.q >>= fun w3 ->
    M.random G.q >>= fun w4 ->
    randoms n >>= fun ww_hat ->
    randoms n >>= fun ww' ->
    let t1 = g **~ w1 and t2 = g **~ w2 in
    M.yield () >>= fun () ->
    mmap2 ( **~ ) hh ww' >>= fun t3_ ->
    let t3 = Array.fold_left ( *~ ) (g **~ w3) t3_ in
    mmap2 (fun e' w' -> e'.beta **~ w') ee' ww' >>= fun t41_ ->
    let t41 = Array.fold_left ( *~ ) (invert (y **~ w4)) t41_ in
    mmap2 (fun e' w' -> e'.alpha **~ w') ee' ww' >>= fun t42_ ->
    let t42 = Array.fold_left ( *~ ) (invert (g **~ w4)) t42_ in
    let cc_hat' = Array.init n (fun i -> if i = 0 then h else cc_hat.(pred i)) in
    mmap3 (fun w_hat w' c_hat -> (g **~ w_hat) *~ (c_hat **~ w')) ww_hat ww' cc_hat' >>= fun tt_hat ->
    let t = (t1, t2, t3, (t41, t42), tt_hat) in
    let str2 = str_elts [| t1; t2; t3; t41; t42 |] ^ str_elts tt_hat in
    let str3 = str1 ^ str_elts cc_hat ^ G.to_string y in
    let c = get_nizkp_challenge ("shuffle-challenge|" ^ str2 ^ str3) in
    let r_bar = Z.(Array.fold_left ( + ) zero rr mod G.q) in
    let s1 = Z.((w1 + c * r_bar) mod G.q) in
    let vv = Array.make n Z.one in
    for i = n - 2 downto 0 do
      vv.(i) <- Z.((uu'.(succ i) * vv.(succ i)) mod G.q);
    done;
    let r_hat = Z.(Array.fold_left ( + ) zero (Array.map2 ( * ) rr_hat vv) mod G.q) in
    let s2 = Z.((w2 + c * r_hat) mod G.q) in
    let r_tilde = Z.(Array.fold_left ( + ) zero (Array.map2 ( * ) rr uu) mod G.q) in
    let s3 = Z.((w3 + c * r_tilde) mod G.q) in
    let r' = Z.(Array.fold_left ( + ) zero (Array.map2 ( * ) rr' uu) mod G.q) in
    let s4 = Z.((w4 + c * r') mod G.q) in
    let ss_hat = Array.init n (fun i -> Z.((ww_hat.(i) + c * rr_hat.(i)) mod G.q)) in
    let ss' = Array.init n (fun i -> Z.((ww'.(i) + c * uu'.(i)) mod G.q)) in
    let s = (s1, s2, s3, s4, ss_hat, ss') in
    M.return (t, s, cc, cc_hat)

  let check_modulo p x = Z.(geq x zero && lt x p)

  let check_shuffle_proof y ee ee' proof =
    let get_generator_indep = make_get_generator_indep () in
    let n = Array.length ee in
    let h = get_generator_indep (-1) in
    n = Array.length ee' &&
    let t, s, cc, cc_hat = proof in
    let t1, t2, t3, (t41, t42), tt_hat = t in
    let s1, s2, s3, s4, ss_hat, ss' = s in
    Array.forall G.check [| t1; t2; t3; t41; t42 |] &&
    Array.forall (check_modulo G.q) [| s1; s2; s3; s4 |] &&
    n = Array.length cc &&
    n = Array.length cc_hat &&
    n = Array.length tt_hat &&
    n = Array.length ss_hat &&
    n = Array.length ss' &&
    Array.forall G.check cc &&
    Array.forall G.check cc_hat &&
    Array.forall G.check tt_hat &&
    Array.forall (check_modulo G.q) ss_hat &&
    Array.forall (check_modulo G.q) ss' &&
    let hh = Array.init n get_generator_indep in
    let str1 = str_egs ee ^ str_egs ee' ^ str_elts cc in
    let uu = get_nizkp_challenges n ("shuffle-challenges|" ^ str1) in
    let str2 = str_elts [| t1; t2; t3; t41; t42 |] ^ str_elts tt_hat in
    let str3 = str1 ^ str_elts cc_hat ^ G.to_string y in
    let c = get_nizkp_challenge ("shuffle-challenge|" ^ str2 ^ str3) in
    let c_bar = (Array.fold_left ( *~ ) one cc) *~ invert (Array.fold_left ( *~ ) one hh) in
    let u = Z.(Array.fold_left ( * ) one uu mod G.q) in
    let c_hat = (if n = 0 then h else cc_hat.(pred n)) *~ invert (h **~ u) in
    let c_tilde = Array.fold_left ( *~ ) one (Array.map2 ( **~ ) cc uu) in
    let a' = Array.fold_left ( *~ ) one (Array.map2 (fun x u -> x.beta **~ u) ee uu) in
    let b' = Array.fold_left ( *~ ) one (Array.map2 (fun x u -> x.alpha **~ u) ee uu) in
    let t1' = invert (c_bar **~ c) *~ (g **~ s1) in
    let t2' = invert (c_hat **~ c) *~ (g **~ s2) in
    let t3' = invert (c_tilde **~ c) *~ Array.fold_left ( *~ ) (g **~ s3) (Array.map2 ( **~ ) hh ss') in
    let t41' = Array.fold_left ( *~ ) (invert ((a' **~ c) *~ (y **~ s4))) (Array.map2 (fun x s -> x.beta **~ s) ee' ss') in
    let t42' = Array.fold_left ( *~ ) (invert ((b' **~ c) *~ (g **~ s4))) (Array.map2 (fun x s -> x.alpha **~ s) ee' ss') in
    let tt'_hat =
      Array.init n (fun i ->
          let x = if i = 0 then h else cc_hat.(pred i) in
          invert (cc_hat.(i) **~ c) *~ (g **~ ss_hat.(i)) *~ (x **~ ss'.(i))
        )
    in
    G.compare t1 t1' = 0 &&
    G.compare t2 t2' = 0 &&
    G.compare t3 t3' = 0 &&
    G.compare t41 t41' = 0 &&
    G.compare t42 t42' = 0 &&
    Array.forall2 (fun t t' -> G.compare t t' = 0) tt_hat tt'_hat

end
