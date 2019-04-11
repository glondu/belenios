(**************************************************************************)
(*                                BELENIOS                                *)
(*                                                                        *)
(*  Copyright © 2012-2018 Inria                                           *)
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
open Serializable_j
open Signatures
open Common

module type PARAMS = sig
  val election : string
  val get_public_keys : unit -> string array option
  val get_threshold : unit -> string option
  val get_public_creds : unit -> string Stream.t option
  val get_ballots : unit -> string Stream.t option
  val get_shuffles : unit -> string Stream.t option
  val get_result : unit -> string option
  val print_msg : string -> unit
end

module type S = sig
  val vote : string option -> int array array -> string
  val decrypt : string -> string
  val tdecrypt : string -> string -> string
  val validate : string list -> string
  val verify : unit -> unit
  val shuffle_ciphertexts : unit -> string * string
end

module type PARSED_PARAMS = sig
  include PARAMS
  include ELECTION_DATA
end

let parse_params p =
  let module P = (val p : PARAMS) in
  let params = Election.(get_group (of_string P.election)) in
  let module R = struct
    include P
    include (val params : ELECTION_DATA)
  end in
  (module R : PARSED_PARAMS)

module Make (P : PARSED_PARAMS) : S = struct

  open P
  module E = Election.Make (P) (DirectRandom)

  module KG = Trustees.MakeSimple (G) (DirectRandom)

  module P = Trustees.MakePKI (G) (DirectRandom)
  module C = Trustees.MakeChannels (G) (DirectRandom) (P)
  module KP = Trustees.MakePedersen (G) (DirectRandom) (P) (C)

  (* Load and check trustee keys, if present *)

  let threshold =
    match get_threshold () with
    | None -> None
    | Some x -> Some (threshold_parameters_of_string G.read x)

  let public_keys_with_pok =
    match threshold with
    | None ->
       get_public_keys () |> Option.map @@
       Array.map (trustee_public_key_of_string G.read)
    | Some t -> Some t.t_verification_keys

  let () =
    match public_keys_with_pok, threshold with
    | Some pks, None ->
      assert (Array.forall KG.check pks);
      let y' = KG.combine pks in
      assert G.(election.e_params.e_public_key =~ y')
    | _ -> ()

  let public_keys =
    Option.map (
      Array.map (fun pk -> pk.trustee_public_key)
    ) public_keys_with_pok

  let pks = lazy (match public_keys with
                  | Some pks -> pks
                  | None -> failwith "missing public keys")

  (* Load ballots, if present *)

  module GSet = Map.Make (G)

  let public_creds = lazy (
    get_public_creds () |> Option.map (fun creds ->
      let res = ref GSet.empty in
      Stream.iter (fun x -> res := GSet.add (G.of_string x) false !res) creds;
      res
    )
  )

  let ballots = lazy (
    get_ballots () |> Option.map (fun ballots ->
      let res = ref [] in
      Stream.iter (fun x ->
        res := (ballot_of_string G.read x, sha256_b64 x) :: !res
      ) ballots;
      List.rev !res
    )
  )

  let check_signature_present = lazy (
    match Lazy.force public_creds with
    | Some creds -> (fun b ->
      match b.signature with
      | Some s ->
         (match GSet.find_opt s.s_public_key !creds with
          | Some false -> creds := GSet.add s.s_public_key true !creds; true
          | _ -> false)
      | None -> false
    )
    | None -> (fun _ -> true)
  )

  let cast (b, hash) =
    if Lazy.force check_signature_present b && E.check_ballot b
    then ()
    else Printf.ksprintf failwith "ballot %s failed tests" hash

  let ballots_check = lazy (
    Lazy.force ballots |> Option.map (List.iter cast)
  )

  let raw_encrypted_tally =
    lazy (
        match Lazy.force ballots with
        | None -> failwith "ballots.jsons is missing"
        | Some ballots ->
           let ballots = Array.map fst (Array.of_list ballots) in
           E.process_ballots ballots,
           Array.length ballots
      )

  let shuffles =
    lazy (
        get_shuffles ()
        |> Option.map (fun s ->
               let shuffles = ref [] and shuffle_proofs = ref [] in
               let rec loop () =
                 if (try Stream.empty s; true with Stream.Failure -> false) then
                   !shuffles, !shuffle_proofs
                 else (
                   shuffle_proofs := (shuffle_proofs_of_string G.read (Stream.next s)) :: !shuffle_proofs;
                   shuffles := (nh_ciphertexts_of_string G.read (Stream.next s)) :: !shuffles;
                   loop ()
                 )
               in
               loop ()
             )
      )

  let shuffles_check =
    lazy (
        let rtally, _ = Lazy.force raw_encrypted_tally in
        let cc = E.extract_nh_ciphertexts rtally in
        let rec loop i cc shuffles shuffle_proofs =
          match shuffles, shuffle_proofs with
          | s :: shuffles, p :: shuffle_proofs ->
             if E.check_shuffle cc s p then
               loop (i+1) s shuffles shuffle_proofs
             else
               Printf.ksprintf failwith "shuffle #%d failed tests" i
          | [], [] -> true
          | _, _ -> failwith "shuffles failed tests"
        in
        match Lazy.force shuffles with
        | Some (ss, pp) -> loop 0 cc (List.rev ss) (List.rev pp)
        | None -> true
      )

  let encrypted_tally =
    lazy (
        let raw_encrypted_tally, ntally = Lazy.force raw_encrypted_tally in
        match Lazy.force shuffles with
        | Some (cc :: _, _) -> E.merge_nh_ciphertexts cc raw_encrypted_tally, ntally
        | _ -> raw_encrypted_tally, ntally
      )

  let vote privcred ballot =
    let sk =
      privcred |> Option.map (fun cred ->
        let module CD = Credential.MakeDerive (G) in
        CD.derive election.e_params.e_uuid cred
      )
    in
    let b = E.create_ballot ?sk ballot in
    assert (E.check_ballot b);
    string_of_ballot G.write b

  let decrypt privkey =
    let sk = number_of_string privkey in
    let pk = G.(g **~ sk) in
    if Array.forall (fun x -> not G.(x =~ pk)) (Lazy.force pks) then (
      print_msg "W: your key is not present in public_keys.jsons";
    );
    let tally, _ = Lazy.force encrypted_tally in
    let factor = E.compute_factor tally sk in
    assert (E.check_factor tally pk factor);
    string_of_partial_decryption G.write factor

  let tdecrypt key pdk =
    let sk = P.derive_sk key and dk = P.derive_dk key in
    let vk = G.(g **~ sk) in
    let pdk = C.recv dk vk pdk in
    let pdk = (partial_decryption_key_of_string pdk).pdk_decryption_key in
    let pvk = G.(g **~ pdk) in
    (match threshold with
     | None -> print_msg "W: threshold parameters are missing"
     | Some t ->
        if Array.forall (fun x ->
               not G.(x.trustee_public_key =~ pvk)
             ) t.t_verification_keys then
          print_msg "W: your key is not present in threshold parameters"
    );
    let tally, _ = Lazy.force encrypted_tally in
    let factor = E.compute_factor tally pdk in
    assert (E.check_factor tally pvk factor);
    string_of_partial_decryption G.write factor

  let validate factors =
    let factors = List.map (partial_decryption_of_string G.read) factors in
    let tally, nballots = Lazy.force encrypted_tally in
    let checker = E.check_factor tally in
    let combinator =
      match threshold with
      | None ->
         KG.combine_factors checker (Lazy.force pks)
      | Some t -> KP.combine_factors checker t
    in
    let result = E.compute_result nballots tally factors combinator in
    assert (E.check_result combinator result);
    string_of_election_result G.write result

  let verify () =
    (match threshold with
     | Some t ->
        assert (KP.check t);
        assert G.(election.e_params.e_public_key =~ KP.combine t)
     | None -> ignore (Lazy.force pks)
    );
    (match Lazy.force ballots_check with
    | Some () -> assert (Lazy.force shuffles_check)
    | None -> print_msg "W: no ballots to check"
    );
    (match get_result () with
    | Some result ->
       let result = election_result_of_string G.read result in
       assert (fst (Lazy.force encrypted_tally) = result.encrypted_tally);
       let checker = E.check_factor result.encrypted_tally in
       let combinator = match threshold with
         | None -> KG.combine_factors checker (Lazy.force pks)
         | Some t -> KP.combine_factors checker t
       in
       assert (E.check_result combinator result)
    | None -> print_msg "W: no result to check"
    );
    print_msg "I: all checks passed"

  let shuffle_ciphertexts () =
    let cc, _ = Lazy.force encrypted_tally in
    let cc = E.extract_nh_ciphertexts cc in
    let cc', p = E.shuffle_ciphertexts cc in
    string_of_nh_ciphertexts G.write cc',
    string_of_shuffle_proofs G.write p

end

let make params =
  let module P = (val parse_params params : PARSED_PARAMS) in
  let module R = Make (P) in
  (module R : S)
