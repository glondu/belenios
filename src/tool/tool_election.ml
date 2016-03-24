(**************************************************************************)
(*                                BELENIOS                                *)
(*                                                                        *)
(*  Copyright © 2012-2016 Inria                                           *)
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
  val get_public_creds : unit -> string Stream.t option
  val get_ballots : unit -> string Stream.t option
  val get_result : unit -> string option
  val print_msg : string -> unit
end

module type S = sig
  val vote : string option -> int array array -> string
  val decrypt : string -> string
  val finalize : string array -> string
  val verify : unit -> unit
end

module type PARSED_PARAMS = sig
  include PARAMS
  include ELECTION_DATA
end

let parse_params p =
  let module P = (val p : PARAMS) in
  let params = Group.election_params_of_string P.election in
  let module R = struct
    include P
    include (val params : ELECTION_DATA)
  end in
  (module R : PARSED_PARAMS)

module Make (P : PARSED_PARAMS) : S = struct

  open P
  module M = Election.MakeSimpleMonad(G)
  module E = Election.MakeElection(G)(M);;

  (* Load and check trustee keys, if present *)

  module KG = Election.MakeSimpleDistKeyGen(G)(M);;

  let public_keys_with_pok =
    get_public_keys () |> option_map @@
    Array.map (trustee_public_key_of_string G.read)

  let () =
    match public_keys_with_pok with
    | Some pks ->
      assert (Array.forall KG.check pks);
      let y' = KG.combine pks in
      assert G.(election.e_params.e_public_key =~ y')
    | None -> ()

  let public_keys =
    option_map (
      Array.map (fun pk -> pk.trustee_public_key)
    ) public_keys_with_pok

  (* Finish setting up the election *)

  let pks = match public_keys with
    | Some pks -> pks
    | None -> failwith "missing public keys"

  (* Load ballots, if present *)

  module GSet = Map.Make (G)

  let public_creds = lazy (
    get_public_creds () |> option_map (fun creds ->
      let res = ref GSet.empty in
      Stream.iter (fun x -> res := GSet.add (G.of_string x) false !res) creds;
      res
    )
  )

  let ballots = lazy (
    get_ballots () |> option_map (fun ballots ->
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
         (try
            if GSet.find s.s_public_key !creds then false
            else (creds := GSet.add s.s_public_key true !creds; true)
          with Not_found -> false)
      | None -> false
    )
    | None -> (fun _ -> true)
  )

  let cast (b, hash) =
    if Lazy.force check_signature_present b && E.check_ballot election b
    then M.cast b ()
    else Printf.ksprintf failwith "ballot %s failed tests" hash

  let ballots_check = lazy (
    Lazy.force ballots |> option_map (List.iter cast)
  )

  let encrypted_tally = lazy (
    match Lazy.force ballots_check with
      | None -> failwith "ballots.jsons is missing"
      | Some () ->
        M.fold (fun () b t ->
          M.return (E.combine_ciphertexts (E.extract_ciphertext b) t)
        ) (E.neutral_ciphertext election) ()
  )

  let vote privcred ballot =
    let sk =
      privcred |> option_map (fun cred ->
        let module CD = Credential.MakeDerive (G) in
        CD.derive election.e_params.e_uuid cred
      )
    in
    let b = E.create_ballot election ?sk (E.make_randomness election ()) ballot () in
    assert (E.check_ballot election b);
    string_of_ballot G.write b

  let decrypt privkey =
    let sk = number_of_string privkey in
    let pk = G.(g **~ sk) in
    if Array.forall (fun x -> not G.(x =~ pk)) pks then (
      print_msg "W: your key is not present in public_keys.jsons";
    );
    let tally = Lazy.force encrypted_tally in
    let factor = E.compute_factor tally sk () in
    assert (E.check_factor tally pk factor);
    string_of_partial_decryption G.write factor

  let finalize factors =
    let factors = Array.map (partial_decryption_of_string G.read) factors in
    let tally = Lazy.force encrypted_tally in
    assert (Array.forall2 (E.check_factor tally) pks factors);
    let result = E.combine_factors (M.cardinal ()) tally factors in
    assert (E.check_result pks result);
    string_of_result G.write result

  let verify () =
    (match Lazy.force ballots_check with
    | Some () -> ()
    | None -> print_msg "W: no ballots to check"
    );
    (match get_result () with
    | Some result ->
      assert (E.check_result pks (result_of_string G.read result))
    | None -> print_msg "W: no result to check"
    );
    print_msg "I: all checks passed"

end

let make params =
  let module P = (val parse_params params : PARSED_PARAMS) in
  let module R = Make (P) in
  (module R : S)
