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

open Lwt.Syntax
open Belenios_platform
open Belenios_core
open Serializable_j
open Signatures
open Common

exception PedersenFailure of string

module type VERIFY_CERT = sig
  module G : GROUP

  val verify_cert : full_context -> (G.t, G.Zq.t) cert -> bool
end

module MakeCert (P : PKI) = struct
  module G = P.Group

  let xch_cert_keys =
    {
      dst = dst_prefix ^ "-pedersen-cert";
      to_string = string_of_cert_keys (swrite G.to_string) write_full_context;
      of_string = cert_keys_of_string (sread G.of_string) read_full_context;
    }

  let compute_polynomial ~seed ~threshold =
    let open Crypto_std.Expand_message (Crypto_std.SHA256) in
    let open Crypto_std.Hash_to_field (struct
      let k = 128
      let p = G.Zq.q
      let m = 1
      let expand_message = expand_message_xmd
    end) in
    let dst = dst_prefix ^ "-pedersen-polynomial" in
    hash_to_field ~dst seed threshold |> Array.map (fun x -> G.Zq.coerce x.(0))

  let compute_coefexps polynomial = Array.map (fun x -> G.(g **~ x)) polynomial

  let make_cert ~seed ~sk ~dk ~(context : full_context) =
    let polynomial =
      compute_polynomial ~seed ~threshold:context.context.threshold
    in
    let coefexps = compute_coefexps polynomial in
    let cert_coefexps =
      { coefexps }
      |> string_of_coefexps (swrite G.to_string)
      |> Hash.hash_string
    in
    let signed =
      P.sign xch_cert_keys sk
        {
          cert_verification = G.(g **~ sk);
          cert_encryption = G.(g **~ dk);
          cert_context = context;
          cert_coefexps;
        }
    in
    let s_message = { signed.s_message with cert_context = context.index } in
    { signed with s_message }

  let verify_cert cert_context x =
    let keys = x.s_message in
    G.check keys.cert_verification
    && G.check keys.cert_encryption
    && cert_context.index = keys.cert_context
    &&
    let s_message = { x.s_message with cert_context } in
    P.verify xch_cert_keys keys.cert_verification { x with s_message }
end

module MakeVerificator (G : GROUP) = struct
  let compute_verification_keys coefexps =
    let n = Array.length coefexps in
    assert (n > 0);
    let threshold = Array.length coefexps.(0) in
    assert (threshold > 0);
    Array.init n (fun j ->
        let jj = G.Zq.of_int (j + 1) in
        let rec loop_compute_vk i vk =
          if i < n then (
            let c = coefexps.(i) in
            assert (threshold = Array.length c);
            let rec loop k jk accu =
              if k < threshold then
                loop (k + 1) G.Zq.(jk * jj) G.(accu *~ (c.(k) **~ jk))
              else accu
            in
            let computed_gsij = loop 0 G.Zq.one G.one in
            loop_compute_vk (i + 1) G.(vk *~ computed_gsij))
          else vk
        in
        loop_compute_vk 0 G.one)
end

module MakeComb (P : PKI) (C : VERIFY_CERT with module G = P.Group) = struct
  module Group = P.Group
  module G = Group
  module V = MakeVerificator (G)

  let xch_single_verification_key =
    {
      dst = dst_prefix ^ "-verification_key";
      of_string =
        raw_trustee_public_key_of_string (sread G.of_string)
          (sread G.Zq.of_string);
      to_string =
        string_of_raw_trustee_public_key (swrite G.to_string)
          (swrite G.Zq.to_string);
    }

  let check_single trustee =
    let { trustee_public_key = y; _ } = trustee.s_message in
    G.check y && P.verify xch_single_verification_key y trustee

  let xch_certs =
    {
      dst = dst_prefix ^ "-pedersen-certs";
      of_string = certs_of_string (sread G.of_string) (sread G.Zq.of_string);
      to_string = string_of_certs (swrite G.to_string) (swrite G.Zq.to_string);
    }

  let xch_verification_key =
    {
      dst = dst_prefix ^ "-pedersen-verification_key";
      of_string =
        trustee_public_key_of_string (sread G.of_string) (sread G.Zq.of_string);
      to_string =
        string_of_trustee_public_key (swrite G.to_string)
          (swrite G.Zq.to_string);
    }

  let xch_decryption_key =
    {
      dst = dst_prefix ^ "-pedersen-decryption_key";
      of_string = partial_decryption_key_of_string (sread G.Zq.of_string);
      to_string = string_of_partial_decryption_key (swrite G.Zq.to_string);
    }

  let check_pedersen t =
    let group = G.description in
    let names =
      Array.map
        (fun x -> x.s_message.s_message.trustee_name)
        t.t_verification_keys
    in
    let threshold = t.t_threshold in
    let context = { group; names; threshold } in
    Array.for_alli
      (fun i c -> C.verify_cert { context; index = i + 1 } c)
      t.t_certs
    &&
    let certs = Array.map (fun x -> x.s_message) t.t_certs in
    Array.for_all2
      (fun cert x ->
        let hash =
          x |> string_of_coefexps (swrite G.to_string) |> Hash.hash_string
        in
        hash = cert.cert_coefexps)
      certs t.t_coefexps
    && (let sigs = t.t_signatures in
        let n = Array.length sigs in
        n = Array.length certs
        && n = Array.length t.t_coefexps
        && Array.for_all2
             (fun cert s_signature ->
               let s_message = { context; certs = t.t_certs } in
               P.verify xch_certs cert.cert_verification
                 { s_message; s_signature })
             certs sigs)
    &&
    let coefexps = Array.map (fun x -> x.coefexps) t.t_coefexps in
    Array.for_all (fun x -> check_single x.s_message) t.t_verification_keys
    &&
    let computed_vks = V.compute_verification_keys coefexps in
    t.t_threshold = Array.length coefexps.(0)
    && Array.for_all3
         G.(
           fun cert vk computed_vk ->
             vk.s_message.s_message.trustee_public_key =~ computed_vk
             && P.verify xch_verification_key cert.cert_verification vk)
         certs t.t_verification_keys computed_vks

  let check trustees =
    trustees
    |> List.for_all (function
      | `Single t -> check_single t
      | `Pedersen t -> check_pedersen t)

  let combine_keys trustees =
    trustees
    |> List.map (function
      | `Single t -> t.s_message.trustee_public_key
      | `Pedersen p ->
          p.t_coefexps
          |> Array.map (fun x -> x.coefexps)
          |> Array.fold_left (fun accu x -> G.(accu *~ x.(0))) G.one)
    |> List.fold_left G.( *~ ) G.one

  let lagrange indexes j =
    List.fold_left
      (fun accu k ->
        let kj = k - j in
        if kj = 0 then accu else G.Zq.(accu * of_int k * invert (of_int kj)))
      G.Zq.one indexes

  let combine_factors trustees check partial_decryptions =
    (* neutral factor *)
    let dummy =
      match partial_decryptions with
      | x :: _ -> Shape.map (fun _ -> G.one) x.owned_payload.decryption_factors
      | [] -> failwith "no partial decryptions"
    in
    (* compute synthetic factor for each trustee_kind *)
    let fold pds_with_ids =
      let indexes = List.map fst pds_with_ids in
      List.fold_left
        (fun a (j, b) ->
          let l = lagrange indexes j in
          Shape.map2 G.(fun x y -> x *~ (y **~ l)) a b.decryption_factors)
        dummy pds_with_ids
    in
    let r =
      Util.compute_synthetic_factors trustees check partial_decryptions fold
    in
    (* combine all factors into one *)
    match r with
    | Ok factors ->
        Ok (List.fold_left (fun a b -> Shape.map2 G.( *~ ) a b) dummy factors)
    | Error _ as x -> x
end

(** Distributed key generation *)

module MakeSimple (G : GROUP) = struct
  open G
  module P = Pki.Make (G)
  module C = MakeCert (P)
  module Comb = MakeComb (P) (C)

  let random () = G.Zq.random (Crypto_primitives.get_rng ())
  let generate = random

  let prove ~name x =
    let trustee_public_key = g **~ x in
    P.sign Comb.xch_single_verification_key x
      { trustee_public_key; trustee_name = name }
end

module MakePedersen (C : CHANNELS) = struct
  module Channels = C
  module P = C.P
  module G = P.Group

  type scalar = G.Zq.t
  type element = G.t
  type nonrec cert = (element, scalar) cert
  type nonrec certs = (element, scalar) certs
  type nonrec polynomial = (element, scalar) polynomial

  module Cert = MakeCert (P)
  module Comb = MakeComb (P) (Cert)
  open G
  module K = MakeSimple (G)
  module V = MakeVerificator (G)

  let xch_decryption_key = Comb.xch_decryption_key

  let xch_secret =
    {
      dst = dst_prefix ^ "-secret";
      of_string = secret_of_string (sread Zq.of_string);
      to_string = string_of_secret (swrite Zq.to_string);
    }

  let random () = Zq.random (Crypto_primitives.get_rng ())

  let step1 context =
    let seed = P.genkey () in
    let sk = P.derive_sk seed in
    let dk = P.derive_dk seed in
    let cert = Cert.make_cert ~seed ~sk ~dk ~context in
    (seed, cert)

  let step1_check = Cert.verify_cert

  let step2 { context; certs; _ } =
    Array.iteri
      (fun i cert ->
        if Cert.verify_cert { context; index = i + 1 } cert then ()
        else
          let msg = Printf.sprintf "certificate %d does not validate" (i + 1) in
          raise (PedersenFailure msg))
      certs;
    context.threshold

  let eval_poly polynomial x =
    let cur = ref Zq.one and res = ref Zq.zero in
    for i = 0 to Array.length polynomial - 1 do
      (res := Zq.(!res + (!cur * polynomial.(i))));
      cur := Zq.(!cur * x)
    done;
    !res

  let step3 certs seed =
    let threshold = step2 certs in
    let n = Array.length certs.certs in
    let sk = P.derive_sk seed and dk = P.derive_dk seed in
    let certs' = Array.map (fun x -> x.s_message) certs.certs in
    let vk = g **~ sk and ek = g **~ dk in
    let i =
      Array.findi
        (fun i cert ->
          if cert.cert_verification =~ vk && cert.cert_encryption =~ ek then
            Some (i + 1)
          else None)
        certs'
    in
    let () =
      match i with
      | None -> raise (PedersenFailure "could not find my certificate")
      | Some _ -> ()
    in
    let polynomial = Array.make threshold Zq.zero in
    let rec fill_polynomial i =
      if i < threshold then (
        let a = random () in
        polynomial.(i) <- a;
        fill_polynomial (i + 1))
      else ()
    in
    let () = fill_polynomial 0 in
    let polynomial = Cert.compute_polynomial ~seed ~threshold in
    let coefexps = Cert.compute_coefexps polynomial in
    let p_coefexps = { coefexps } in
    let p_signature = (P.sign Comb.xch_certs sk certs).s_signature in
    let* p_secrets =
      Array.init_lwt n (fun j ->
          let secret = eval_poly polynomial (Zq.of_int (j + 1)) in
          C.send xch_secret sk certs'.(j).cert_encryption { secret })
    in
    Lwt.return { p_secrets; p_coefexps; p_signature }

  let step3_check certs i polynomial =
    let hash =
      polynomial.p_coefexps
      |> string_of_coefexps (swrite G.to_string)
      |> Hash.hash_string
    in
    hash = certs.certs.(i).s_message.cert_coefexps
    && P.verify Comb.xch_certs certs.certs.(i).s_message.cert_verification
         { s_message = certs; s_signature = polynomial.p_signature }

  let step4 certs polynomials =
    let threshold = step2 certs in
    let n = Array.length certs.certs in
    assert (n = Array.length polynomials);
    let certs = Array.map (fun x -> x.s_message) certs.certs in
    let vi_signatures = Array.map (fun x -> x.p_signature) polynomials in
    let vi_coefexps = Array.map (fun x -> x.p_coefexps) polynomials in
    Array.iteri
      (fun i x ->
        let hash =
          x |> string_of_coefexps (swrite G.to_string) |> Hash.hash_string
        in
        if hash = certs.(i).cert_coefexps then
          if threshold = Array.length x.coefexps then ()
          else
            let msg = Printf.sprintf "coefexps %d has wrong length" (i + 1) in
            raise (PedersenFailure msg)
        else
          let msg = Printf.sprintf "coefexps %d does not validate" (i + 1) in
          raise (PedersenFailure msg))
      vi_coefexps;
    Array.init n (fun j ->
        let vi_secrets =
          Array.init n (fun i -> polynomials.(i).p_secrets.(j))
        in
        { vi_secrets; vi_coefexps; vi_signatures })

  let sign_trustee_public_key ~sk x = P.sign Comb.xch_verification_key sk x

  let step5 certs seed vinput =
    let threshold = step2 certs in
    let context = certs.context in
    let n = Array.length certs.certs in
    let certs = Array.map (fun x -> x.s_message) certs.certs in
    let sk = P.derive_sk seed and dk = P.derive_dk seed in
    let vk = g **~ sk and ek = g **~ dk in
    let j =
      Array.findi
        (fun i cert ->
          if cert.cert_verification =~ vk && cert.cert_encryption =~ ek then
            Some (i + 1)
          else None)
        certs
    in
    let index, j =
      match j with
      | None -> raise (PedersenFailure "could not find my certificate")
      | Some i -> (i, Zq.of_int i)
    in
    assert (n = Array.length vinput.vi_secrets);
    let* secrets =
      Array.init_lwt n (fun i ->
          let* { secret } =
            C.recv xch_secret dk certs.(i).cert_verification
              vinput.vi_secrets.(i)
          in
          Lwt.return secret)
    in
    assert (n = Array.length vinput.vi_coefexps);
    let coefexps =
      Array.init n (fun i ->
          let x = vinput.vi_coefexps.(i) in
          let hash =
            x |> string_of_coefexps (swrite G.to_string) |> Hash.hash_string
          in
          if hash <> certs.(i).cert_coefexps then
            raise
              (PedersenFailure
                 (Printf.sprintf "coefexps %d does not validate" (i + 1)));
          let res = x.coefexps in
          assert (Array.length res = threshold);
          res)
    in
    for i = 0 to n - 1 do
      let c = coefexps.(i) in
      let rec loop k jk accu =
        if k < threshold then loop (k + 1) Zq.(jk * j) (accu *~ (c.(k) **~ jk))
        else accu
      in
      let computed_gsij = loop 0 Zq.one one in
      if not (g **~ secrets.(i) =~ computed_gsij) then
        raise
          (PedersenFailure
             (Printf.sprintf "secret %d does not validate" (i + 1)))
    done;
    let pdk_decryption_key = Array.fold_left Zq.( + ) Zq.zero secrets in
    let pdk = { pdk_decryption_key } in
    let name = context.names.(index - 1) in
    let vo_public_key =
      K.prove ~name pdk_decryption_key |> sign_trustee_public_key ~sk
    in
    let* vo_private_key = C.send xch_decryption_key sk ek pdk in
    Lwt.return { vo_public_key; vo_private_key }

  let step5_check certs i polynomials voutput =
    let n = Array.length certs.certs in
    let certs = Array.map (fun x -> x.s_message) certs.certs in
    assert (n = Array.length polynomials);
    let coefexps =
      Array.init n (fun i ->
          let x = polynomials.(i).p_coefexps in
          let hash =
            x |> string_of_coefexps (swrite G.to_string) |> Hash.hash_string
          in
          if hash <> certs.(i).cert_coefexps then
            raise
              (PedersenFailure
                 (Printf.sprintf "coefexps %d does not validate" (i + 1)));
          x.coefexps)
    in
    let y = (V.compute_verification_keys coefexps).(i) in
    let { vo_public_key; _ } = voutput in
    Comb.check [ `Single vo_public_key.s_message ]
    && vo_public_key.s_message.s_message.trustee_public_key =~ y
    && P.verify Comb.xch_verification_key certs.(i).cert_verification
         vo_public_key

  let step6 certs polynomials voutputs =
    let t_threshold = step2 certs in
    let n = Array.length certs.certs in
    let t_certs = certs.certs in
    let t_signatures = Array.map (fun x -> x.p_signature) polynomials in
    let certs = Array.map (fun x -> x.s_message) t_certs in
    assert (n = Array.length polynomials);
    assert (n = Array.length voutputs);
    let coefexps =
      Array.init n (fun i ->
          let fail () =
            raise
              (PedersenFailure
                 (Printf.sprintf "coefexps %d does not validate" (i + 1)))
          in
          let x = polynomials.(i).p_coefexps in
          let hash =
            x |> string_of_coefexps (swrite G.to_string) |> Hash.hash_string
          in
          if hash <> certs.(i).cert_coefexps then fail ();
          let r = x.coefexps in
          if not (t_threshold = Array.length r) then fail ();
          r)
    in
    let computed_vks = V.compute_verification_keys coefexps in
    for j = 0 to n - 1 do
      let voutput = voutputs.(j) in
      if not (Comb.check [ `Single voutput.vo_public_key.s_message ]) then
        raise
          (PedersenFailure (Printf.sprintf "pok %d does not validate" (j + 1)));
      if
        not
          (voutput.vo_public_key.s_message.s_message.trustee_public_key
         =~ computed_vks.(j))
      then
        raise
          (PedersenFailure
             (Printf.sprintf "verification key %d is incorrect" (j + 1)))
    done;
    {
      t_threshold;
      t_certs;
      t_coefexps = Array.map (fun x -> x.p_coefexps) polynomials;
      t_signatures;
      t_verification_keys = Array.map (fun x -> x.vo_public_key) voutputs;
    }
end

module MakeCombinator (G : GROUP) = struct
  module P = Pki.Make (G)
  module Channels = Pki.MakeChannels (P)
  module Cert = MakeCert (P)
  include MakeComb (P) (Cert)
end
