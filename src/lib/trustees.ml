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

open Belenios_platform
open Platform
open Serializable_core_t
open Serializable_j
open Signatures
open Common

exception PedersenFailure of string

module MakeVerificator (G : GROUP) = struct

  let verify vk {s_message; s_signature = { challenge; response }} =
    check_modulo G.q challenge &&
    check_modulo G.q response &&
    let commitment = G.(g **~ response *~ vk **~ challenge) in
    let prefix = "sigmsg|" ^ s_message ^ "|" in
    Z.(challenge =% G.hash prefix [|commitment|])

  let verify_cert x =
    let keys = cert_keys_of_string G.read x.s_message in
    verify keys.cert_verification x

  let compute_verification_keys coefexps =
    let n = Array.length coefexps in
    assert (n > 0);
    let threshold = Array.length coefexps.(0) in
    assert (threshold > 0);
    Array.init n (fun j ->
        let jj = Z.of_int (j+1) in
        let rec loop_compute_vk i vk =
          if i < n then
            let c = coefexps.(i) in
            assert (threshold = Array.length c);
            let rec loop k jk accu =
              if k < threshold then
                loop (k+1) Z.(jk * jj) G.(accu *~ (c.(k) **~ jk))
              else accu
            in
            let computed_gsij = loop 0 Z.one G.one in
            loop_compute_vk (i+1) G.(vk *~ computed_gsij)
          else vk
        in
        loop_compute_vk 0 G.one
      )

end

exception CombinationError of combination_error

let string_of_combination_error = function
  | MissingPartialDecryption -> "a partial decryption is missing"
  | NotEnoughPartialDecryptions -> "not enough partial decryptions"
  | UnusedPartialDecryption -> "unused partial decryption"

module MakeCombinator (G : GROUP) = struct

  module V = MakeVerificator (G)

  let ( / ) x y = G.(x *~ invert y)

  let check_single {trustee_pok; trustee_public_key = y; _} =
    G.check y &&
    let {challenge; response} = trustee_pok in
    check_modulo G.q challenge &&
    check_modulo G.q response &&
    let commitment = G.(g **~ response / (y **~ challenge)) in
    let zkp = "pok|" ^ G.to_string y ^ "|" in
    Z.(challenge =% G.hash zkp [| commitment |])

  let check_pedersen t =
    Array.forall V.verify_cert t.t_certs &&
    let certs = Array.map (fun x -> cert_keys_of_string G.read x.s_message) t.t_certs in
    Array.forall2 (fun cert x ->
        V.verify cert.cert_verification x
      ) certs t.t_coefexps &&
    let coefexps = Array.map (fun x -> (raw_coefexps_of_string G.read x.s_message).coefexps) t.t_coefexps in
    Array.forall check_single t.t_verification_keys &&
    let computed_vks = V.compute_verification_keys coefexps in
    t.t_threshold = Array.length coefexps.(0) &&
    Array.forall2 G.(fun vk computed_vk ->
        vk.trustee_public_key =~ computed_vk
      ) t.t_verification_keys computed_vks

  let check trustees =
    trustees
    |> List.for_all
         (function
          | `Single t -> check_single t
          | `Pedersen t -> check_pedersen t
         )

  let combine_keys trustees =
    trustees
    |> List.map
         (function
          | `Single t -> t.trustee_public_key
          | `Pedersen p ->
             p.t_coefexps
             |> Array.map (fun x -> (raw_coefexps_of_string G.read x.s_message).coefexps)
             |> Array.fold_left (fun accu x -> G.(accu *~ x.(0))) G.one
         )
    |> List.fold_left G.( *~ ) G.one

  let lagrange indexes j =
    List.fold_left (fun accu k ->
        let kj = k - j in
        if kj = 0 then accu
        else Z.(accu * (of_int k) * invert (of_int kj) G.q mod G.q)
      ) Z.one indexes

  let combine_factors_exc trustees check partial_decryptions =
    (* neutral factor *)
    let dummy =
      match partial_decryptions with
      | x :: _ -> Shape.map (fun _ -> G.one) x.decryption_factors
      | [] -> failwith "no partial decryptions"
    in
    let partial_decryptions =
      List.map (fun x -> x, ref true) partial_decryptions
    in
    let check t (x, unchecked) =
      if !unchecked then (
        let r = check t x in
        if r then unchecked := false;
        r
      ) else false
    in
    (* compute synthetic factor for each trustee_kind *)
    let factors =
      List.map
        (function
         | `Single t ->
            (match List.find_opt (check t.trustee_public_key) partial_decryptions with
             | None -> raise (CombinationError MissingPartialDecryption)
             | Some (pd, _) -> pd.decryption_factors
            )
         | `Pedersen p ->
            let rec take n accu xs =
              if n > 0 then
                match xs with
                | [] -> raise (CombinationError NotEnoughPartialDecryptions)
                | x :: xs -> take (n-1) (x :: accu) xs
              else accu
            in
            let pds_with_ids =
              Array.to_list p.t_verification_keys
              |> List.map (fun t -> List.find_opt (check t.trustee_public_key) partial_decryptions)
              |> List.mapi
                   (fun i x ->
                     match x with
                     | Some (x, _) -> [i+1, x]
                     | None -> []
                   )
              |> List.flatten
              |> take p.t_threshold []
            in
            let indexes = List.map fst pds_with_ids in
            List.fold_left
              (fun a (j, b) ->
                let l = lagrange indexes j in
                Shape.map2 G.(fun x y -> x *~ y **~ l) a b.decryption_factors
              ) dummy pds_with_ids
        ) trustees
    in
    (* check that all partial decryptions have been used *)
    if List.exists (fun (_, x) -> !x) partial_decryptions then
      raise (CombinationError UnusedPartialDecryption);
    (* combine all factors into one *)
    List.fold_left (fun a b -> Shape.map2 G.( *~ ) a b) dummy factors

  let combine_factors trustees check partial_decryptions =
    try Ok (combine_factors_exc trustees check partial_decryptions)
    with CombinationError e -> Error e

end

(** Distributed key generation *)

module MakeSimple (G : GROUP) (M : RANDOM) = struct
  open G
  open M

  let ( >>= ) = bind

  (** Fiat-Shamir non-interactive zero-knowledge proofs of
      knowledge *)

  let fs_prove gs x oracle =
    random q >>= fun w ->
    let commitments = Array.map (fun g -> g **~ w) gs in
    let challenge = oracle commitments in
    let response = Z.((w + x * challenge) mod q) in
    return {challenge; response}

  let generate () = random q

  let prove x =
    let trustee_public_key = g **~ x in
    let zkp = "pok|" ^ G.to_string trustee_public_key ^ "|" in
    fs_prove [| g |] x (G.hash zkp) >>= fun trustee_pok ->
    return {trustee_pok; trustee_public_key; trustee_name = None}

end

module MakePKI (G : GROUP) (M : RANDOM) = struct

  type 'a m = 'a M.t
  type private_key = Z.t
  type public_key = G.t

  let genkey () =
    let b58_digits = "123456789ABCDEFGHJKLMNPQRSTUVWXYZabcdefghijkmnopqrstuvwxyz" in
    let n = 22 and z58 = Z.of_int 58 in
    let res = Bytes.create n in
    let rec loop i =
      if i < n then
        M.bind (M.random z58) (fun x ->
            Bytes.set res i b58_digits.[Z.to_int x];
            loop (i+1)
          )
      else M.return (Bytes.to_string res)
    in loop 0

  let derive_sk p =
    Z.of_hex (sha256_hex ("sk|" ^ p))

  let derive_dk p =
    Z.of_hex (sha256_hex ("dk|" ^ p))

  let sign sk s_message =
    M.bind (M.random G.q) (fun w ->
        let commitment = G.(g **~ w) in
        let prefix = "sigmsg|" ^ s_message ^ "|" in
        let challenge = G.hash prefix [|commitment|] in
        let response = Z.(erem (w - sk * challenge) G.q) in
        let s_signature = { challenge; response } in
        M.return { s_message; s_signature }
      )

  let encrypt y plaintext =
    M.bind (M.random G.q) (fun r ->
        M.bind (M.random G.q) (fun key ->
            let key = G.(g **~ key) in
            let y_alpha = G.(g **~ r) in
            let y_beta = G.((y **~ r) *~ key) in
            let key = sha256_hex ("key|" ^ G.to_string key) in
            let iv = sha256_hex ("iv|" ^ G.to_string y_alpha) in
            let y_data = Platform.encrypt ~key ~iv ~plaintext in
            let msg = {y_alpha; y_beta; y_data} in
            M.return (string_of_encrypted_msg G.write msg)
          )
      )

  let decrypt x msg =
    let {y_alpha; y_beta; y_data} = encrypted_msg_of_string G.read msg in
    let key = sha256_hex G.("key|" ^ to_string (y_beta *~ invert (y_alpha **~ x))) in
    let iv = sha256_hex ("iv|" ^ G.to_string y_alpha) in
    Platform.decrypt ~key ~iv ~ciphertext:y_data

  let make_cert ~sk ~dk =
    let cert_keys = {
        cert_verification = G.(g **~ sk);
        cert_encryption = G.(g **~ dk);
      } in
    let cert = string_of_cert_keys G.write cert_keys in
    sign sk cert

    include MakeVerificator (G)
end

module MakeChannels (G : GROUP) (M : RANDOM)
         (P : PKI with type 'a m = 'a M.t
                   and type private_key = Z.t
                   and type public_key = G.t) = struct

  type 'a m = 'a P.m
  type private_key = P.private_key
  type public_key = P.public_key

  let send sk c_recipient c_message =
    let msg = { c_recipient; c_message } in
    let msg = string_of_channel_msg G.write msg in
    M.bind (P.sign sk msg) (fun msg ->
        P.encrypt c_recipient (string_of_signed_msg msg)
      )

  let recv dk vk msg =
    let msg = P.decrypt dk msg |> signed_msg_of_string in
    if not (P.verify vk msg) then
      failwith "invalid signature on received message";
    let msg = channel_msg_of_string G.read msg.s_message in
    let { c_recipient; c_message } = msg in
    if not G.(c_recipient =~ g **~ dk) then
      failwith "invalid recipient on received message";
    c_message

end

module MakePedersen (G : GROUP) (M : RANDOM)
         (P : PKI with type 'a m = 'a M.t
                   and type private_key = Z.t
                   and type public_key = G.t)
         (C : CHANNELS with type 'a m = 'a M.t
                        and type private_key = Z.t
                        and type public_key = G.t) = struct

  type 'a m = 'a M.t
  type elt = G.t
  open G
  let (>>=) = M.bind

  module K = MakeSimple (G) (M)
  module V = MakeVerificator (G)
  module L = MakeCombinator (G)

  let step1 () =
    P.genkey () >>= fun seed ->
    let sk = P.derive_sk seed in
    let dk = P.derive_dk seed in
    P.make_cert ~sk ~dk >>= fun cert ->
    M.return (seed, cert)

  let step1_check cert = P.verify_cert cert

  let step2 {certs} =
    Array.iteri (fun i cert ->
        if P.verify_cert cert then ()
        else
          let msg = Printf.sprintf "certificate %d does not validate" (i+1) in
          raise (PedersenFailure msg)
      ) certs

  let eval_poly polynomial x =
    let cur = ref Z.one and res = ref Z.zero in
    for i = 0 to Array.length polynomial - 1 do
      res := Z.(!res + !cur * polynomial.(i) mod q);
      cur := Z.(!cur * x mod q);
    done;
    !res

  let step3 certs seed threshold =
    let n = Array.length certs.certs in
    let () = step2 certs in
    let certs = Array.map (fun x -> cert_keys_of_string G.read x.s_message) certs.certs in
    let sk = P.derive_sk seed and dk = P.derive_dk seed in
    let vk = g **~ sk and ek = g **~ dk in
    let i =
      Array.findi (fun i cert ->
          if cert.cert_verification =~ vk && cert.cert_encryption =~ ek
          then Some (i+1) else None
        ) certs
    in
    let () = match i with
      | None -> raise (PedersenFailure "could not find my certificate")
      | Some _ -> ()
    in
    let polynomial = Array.make threshold Z.zero in
    let rec fill_polynomial i =
      if i < threshold then
        M.random q >>= fun a ->
        polynomial.(i) <- a;
        fill_polynomial (i+1)
      else M.return ()
    in fill_polynomial 0 >>= fun () ->
    C.send sk ek (string_of_raw_polynomial {polynomial}) >>= fun p_polynomial ->
    let coefexps = Array.map (fun x -> g **~ x) polynomial in
    let coefexps = string_of_raw_coefexps G.write {coefexps} in
    P.sign sk coefexps >>= fun p_coefexps ->
    let p_secrets = Array.make n "" in
    let rec fill_secrets j =
      if j < n then
        let secret = eval_poly polynomial (Z.of_int (j+1)) in
        let secret = string_of_secret {secret} in
        C.send sk certs.(j).cert_encryption secret >>= fun x ->
        p_secrets.(j) <- x;
        fill_secrets (j+1)
      else M.return ()
    in fill_secrets 0 >>= fun () ->
    M.return {p_polynomial; p_secrets; p_coefexps}

  let step3_check certs i polynomial =
    let certs = Array.map (fun x -> cert_keys_of_string G.read x.s_message) certs.certs in
    P.verify certs.(i).cert_verification polynomial.p_coefexps

  let step4 certs polynomials =
    let n = Array.length certs.certs in
    let () = step2 certs in
    assert (n = Array.length polynomials);
    let certs = Array.map (fun x -> cert_keys_of_string G.read x.s_message) certs.certs in
    let vi_coefexps = Array.map (fun x -> x.p_coefexps) polynomials in
    Array.iteri (fun i x ->
        if P.verify certs.(i).cert_verification x then ()
        else
          let msg = Printf.sprintf "coefexps %d does not validate" (i+1) in
          raise (PedersenFailure msg)
      ) vi_coefexps;
    Array.init n (fun j ->
        let vi_polynomial = polynomials.(j).p_polynomial in
        let vi_secrets = Array.init n (fun i -> polynomials.(i).p_secrets.(j)) in
        {vi_polynomial; vi_secrets; vi_coefexps}
      )

  let step5 certs seed vinput =
    let n = Array.length certs.certs in
    let () = step2 certs in
    let certs = Array.map (fun x -> cert_keys_of_string G.read x.s_message) certs.certs in
    let sk = P.derive_sk seed and dk = P.derive_dk seed in
    let vk = g **~ sk and ek = g **~ dk in
    let j =
      Array.findi (fun i cert ->
          if cert.cert_verification =~ vk && cert.cert_encryption =~ ek
          then Some (i+1) else None
        ) certs
    in
    let j = match j with
      | None -> raise (PedersenFailure "could not find my certificate")
      | Some i -> Z.of_int i
    in
    let {polynomial} = C.recv dk vk vinput.vi_polynomial |> raw_polynomial_of_string in
    let threshold = Array.length polynomial in
    assert (n = Array.length vinput.vi_secrets);
    let secrets =
      Array.init n (fun i ->
          let x = C.recv dk certs.(i).cert_verification vinput.vi_secrets.(i) in
          (secret_of_string x).secret
        )
    in
    assert (n = Array.length vinput.vi_coefexps);
    let coefexps =
      Array.init n (fun i ->
          let x = vinput.vi_coefexps.(i) in
          if not (P.verify certs.(i).cert_verification x) then
            raise (PedersenFailure (Printf.sprintf "coefexps %d does not validate" (i+1)));
          let res = (raw_coefexps_of_string G.read x.s_message).coefexps in
          assert (Array.length res = threshold);
          res
        )
    in
    for i = 0 to n-1 do
      let c = coefexps.(i) in
      let rec loop k jk accu =
        if k < threshold then
          loop (k+1) Z.(jk * j) (accu *~ (c.(k) **~ jk))
        else accu
      in
      let computed_gsij = loop 0 Z.one one in
      if not (g **~ secrets.(i) =~ computed_gsij) then
        raise (PedersenFailure (Printf.sprintf "secret %d does not validate" (i+1)));
    done;
    let pdk_decryption_key = Array.fold_left Z.(+) Z.zero secrets in
    let pdk = string_of_partial_decryption_key {pdk_decryption_key} in
    M.bind (K.prove pdk_decryption_key) (fun vo_public_key ->
        M.bind (C.send sk ek pdk) (fun vo_private_key ->
            M.return { vo_public_key; vo_private_key }
          )
      )

  let step5_check certs i polynomials voutput =
    let n = Array.length certs.certs in
    let certs = Array.map (fun x -> cert_keys_of_string G.read x.s_message) certs.certs in
    assert (n = Array.length polynomials);
    let coefexps =
      Array.init n (fun i ->
          let x = polynomials.(i).p_coefexps in
          if not (P.verify certs.(i).cert_verification x) then
            raise (PedersenFailure (Printf.sprintf "coefexps %d does not validate" (i+1)));
          (raw_coefexps_of_string G.read x.s_message).coefexps
        )
    in
    let computed_vk = (V.compute_verification_keys coefexps).(i) in
    L.check [`Single voutput.vo_public_key] &&
    voutput.vo_public_key.trustee_public_key =~ computed_vk

  let step6 certs polynomials voutputs =
    let n = Array.length certs.certs in
    let () = step2 certs in
    let t_certs = certs.certs in
    let certs = Array.map (fun x -> cert_keys_of_string G.read x.s_message) t_certs in
    assert (n = Array.length polynomials);
    assert (n = Array.length voutputs);
    let coefexps =
      Array.init n (fun i ->
          let x = polynomials.(i).p_coefexps in
          if not (P.verify certs.(i).cert_verification x) then
            raise (PedersenFailure (Printf.sprintf "coefexps %d does not validate" (i+1)));
          (raw_coefexps_of_string G.read x.s_message).coefexps
        )
    in
    let computed_vks = V.compute_verification_keys coefexps in
    for j = 0 to n - 1 do
      let voutput = voutputs.(j) in
      if not (L.check [`Single voutput.vo_public_key]) then
        raise (PedersenFailure (Printf.sprintf "pok %d does not validate" (j+1)));
      if not (voutput.vo_public_key.trustee_public_key =~ computed_vks.(j)) then
        raise (PedersenFailure (Printf.sprintf "verification key %d is incorrect" (j+1)));
    done;
    {
      t_threshold = Array.length coefexps.(0);
      t_certs;
      t_coefexps = Array.map (fun x -> x.p_coefexps) polynomials;
      t_verification_keys = Array.map (fun x -> x.vo_public_key) voutputs;
    }

end
