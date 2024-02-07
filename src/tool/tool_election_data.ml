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

open Belenios
open Common

module type GETTERS = sig
  val fsck : unit -> unit
  val setup_data : setup_data
  val raw_election : string
  val get_trustees : unit -> string option
  val get_salts : unit -> salts option
  val get_public_creds : unit -> string list option
  val get_ballots : unit -> string list option
  val get_shuffles : unit -> (hash * hash owned * string) list option
  val get_pds : unit -> (hash * hash owned * string) list option
  val get_result : unit -> string option
end

module type PARAMS = sig
  val file : string
  val salts_file : string option
end

module MakeGetters (X : PARAMS) : GETTERS = struct
  let index = Tool_events.get_index ~file:X.file
  let roots = Tool_events.get_roots index
  let get_data x = Tool_events.get_data index x
  let fsck () = Tool_events.fsck index

  let setup_data =
    match roots.roots_setup_data with
    | None -> failcmd "setup data are missing"
    | Some x -> (
        match get_data x with
        | None -> failcmd "could not get setup data"
        | Some x -> setup_data_of_string x)

  let raw_election =
    match get_data setup_data.setup_election with
    | None -> failcmd "could not get election"
    | Some x -> x

  let get_public_creds () =
    get_data setup_data.setup_credentials
    |> Option.map public_credentials_of_string

  let get_trustees () = get_data setup_data.setup_trustees

  let get_salts () =
    match X.salts_file with
    | None -> None
    | Some f -> Some (f |> string_of_file |> salts_of_string)

  let get_ballots () =
    match roots.roots_last_ballot_event with
    | None -> Some []
    | Some x ->
        Some
          (Tool_events.fold_on_event_payloads index `Ballot x
             (fun x accu -> x :: accu)
             [])

  let get_shuffles () =
    let& x = roots.roots_last_shuffle_event in
    Tool_events.fold_on_event_payload_hashes index `Shuffle x
      (fun x accu ->
        match get_data x with
        | None -> failwith "could not get shuffle"
        | Some y -> (
            let owned = owned_of_string read_hash y in
            match get_data owned.owned_payload with
            | None -> failwith "could not get shuffle payload"
            | Some z -> (x, owned, z) :: accu))
      []
    |> fun x -> Some x

  let get_pds () =
    let& x = roots.roots_last_pd_event in
    Tool_events.fold_on_event_payload_hashes index `PartialDecryption x
      (fun x accu ->
        match get_data x with
        | None -> failwith "could not get partial decryption"
        | Some y -> (
            let owned = owned_of_string read_hash y in
            match get_data owned.owned_payload with
            | None -> failwith "could not get partial decryption payload"
            | Some z -> (x, owned, z) :: accu))
      []
    |> fun x -> Some x

  let get_result () =
    let& x = roots.roots_result in
    get_data x
end

module type ELECTION_DATA = sig
  type s
  type t
  type r

  module Cred :
    Belenios_core.Credential.S
      with type private_key := s
       and type public_key := t
       and type 'a m := 'a

  val trustees_as_string : string option
  val trustees : (t, s) trustees option
  val pks : t array Lazy.t
  val raw_public_creds : string list option Lazy.t
  val public_creds_weights : (bool * weight SMap.t) option Lazy.t
  val raw_ballots : string list option Lazy.t
  val verified_ballots : (hash * string * weight * string) list Lazy.t
  val unverified_ballots : (hash * string * weight * string) list Lazy.t
  val string_of_cast_error : cast_error -> string

  val pre_cast :
    ?skip_ballot_check:bool ->
    SSet.t ->
    string ->
    (hash * (string * weight * string), cast_error) result

  val raw_encrypted_tally :
    (t encrypted_tally * hash sized_encrypted_tally) Lazy.t

  val raw_shuffles : (hash * hash owned * string) list option Lazy.t
  val shuffles : (t, s) shuffle list option Lazy.t
  val shuffles_hash : string list option Lazy.t
  val encrypted_tally : (t encrypted_tally * hash sized_encrypted_tally) Lazy.t
  val pds : (hash * hash owned * string) list option Lazy.t
  val result : r election_result option Lazy.t
  val fsck : unit -> unit
  val election_hash : hash
end

module Make (Getters : GETTERS) (Election : ELECTION) :
  ELECTION_DATA
    with type s := Election.G.Zq.t
     and type t := Election.G.t
     and type r := Election.result = struct
  include Getters
  include Election

  let trustees_as_string = get_trustees ()

  let trustees =
    Option.map
      (trustees_of_string (sread G.of_string) (sread G.Zq.of_string))
      trustees_as_string

  let pks =
    lazy
      (let public_keys_with_pok =
         trustees
         |> Option.map (fun x ->
                x
                |> List.map (function
                     | `Single x -> [ x ]
                     | `Pedersen t -> Array.to_list t.t_verification_keys)
                |> List.flatten |> Array.of_list)
       in
       let public_keys =
         Option.map
           (Array.map (fun pk -> pk.trustee_public_key))
           public_keys_with_pok
       in
       match public_keys with
       | Some pks -> pks
       | None -> failwith "missing public keys")

  let raw_public_creds = lazy (get_public_creds ())

  module rec Cred :
    (Belenios_core.Credential.S
      with type private_key := G.Zq.t
       and type public_key := G.t
       and type 'a m := 'a) =
    Belenios_core.Credential.Make
      (G)
      (struct
        type 'a t = 'a

        let return x = x
        let bind x f = f x
        let pause () = ()
        let uuid = uuid

        let get_salt_lazy =
          lazy
            (match get_salts () with
            | None -> fun _ -> None
            | Some salts -> (
                match Lazy.force raw_public_creds with
                | None -> fun _ -> None
                | Some creds ->
                    let salts =
                      List.combine salts creds
                      |> List.map (fun (salt, cred) ->
                             match Cred.parse_public_credential cred with
                             | Some (_, public_credential) ->
                                 { salt; public_credential }
                             | None ->
                                 Printf.ksprintf failwith
                                   "%s is not a valid public credential" cred)
                      |> Array.of_list
                    in
                    let n = Array.length salts in
                    fun i -> if 0 <= i && i < n then Some salts.(i) else None))

        let get_salt i = Lazy.force get_salt_lazy i
      end)

  let public_creds_weights =
    lazy
      (Lazy.force raw_public_creds
      |> Option.map
           (List.fold_left
              (fun (has_weights, accu) x ->
                let has_weights =
                  has_weights || String.index_opt x ',' <> None
                in
                match Cred.parse_public_credential x with
                | Some (w, y) ->
                    let y = G.to_string y in
                    if SMap.mem y accu then
                      Printf.ksprintf failwith "duplicate credential: %s" y
                    else (has_weights, SMap.add y w accu)
                | None ->
                    Printf.ksprintf failwith
                      "%s is not a valid public credential" x)
              (false, SMap.empty)))

  let public_creds = lazy (Lazy.force public_creds_weights |> Option.map snd)
  let raw_ballots = lazy (get_ballots ())

  let string_of_cast_error = function
    | `SerializationError e ->
        Printf.sprintf "ill-formed ballot: %s" (Printexc.to_string e)
    | `NonCanonical -> "ballot not in canonical form"
    | `InvalidBallot -> "invalid ballot"
    | `InvalidCredential -> "invalid credential"
    | `WrongCredential -> "wrong credential"
    | `WrongWeight -> "wrong weight"
    | `UsedCredential -> "used credential"
    | `RevoteNotAllowed -> "revote not allowed"
    | `DuplicateBallot -> "duplicate ballot"
    | `ExpiredBallot -> "expired ballot"
    | `WrongUsername -> "wrong username"

  let pre_cast ?(skip_ballot_check = false) ballot_box rawballot =
    let hash = Hash.hash_string rawballot in
    let ballot_id = Hash.to_b64 hash in
    let@ creds cont =
      match Lazy.force public_creds with
      | None -> failwith "missing public credentials"
      | Some creds -> cont creds
    in
    let is_duplicate = SSet.mem ballot_id ballot_box in
    let@ rc cont =
      match (is_duplicate, E.check_rawballot rawballot) with
      | true, _ -> Error `DuplicateBallot
      | _, (Error _ as e) -> e
      | _, Ok rc -> cont rc
    in
    match SMap.find_opt rc.rc_credential creds with
    | None -> Error `InvalidCredential
    | Some w when skip_ballot_check || rc.rc_check () ->
        Ok (hash, (rc.rc_credential, w, rawballot))
    | Some _ -> Error `InvalidBallot

  let collect_ballots ?(skip_ballot_check = false) () =
    let ballot_box =
      let rec cast_all accu seen = function
        | [] -> accu
        | b :: bs -> (
            match pre_cast seen b ~skip_ballot_check with
            | Error e ->
                Printf.ksprintf failwith "error while casting ballot %s: %s"
                  (sha256_b64 b) (string_of_cast_error e)
            | Ok (hash, x) ->
                let ballot_id = Hash.to_b64 hash in
                cast_all ((hash, x) :: accu) (SSet.add ballot_id seen) bs)
      in
      match Lazy.force raw_ballots with
      | None -> []
      | Some bs -> cast_all [] SSet.empty bs
    in
    List.fold_left
      (fun ((seen, bs) as accu) (h, (credential, w, b)) ->
        if SSet.mem credential seen then accu
        else (SSet.add credential seen, (h, credential, w, b) :: bs))
      (SSet.empty, []) ballot_box
    |> snd

  let verified_ballots = lazy (collect_ballots ())
  let unverified_ballots = lazy (collect_ballots ~skip_ballot_check:true ())

  let raw_encrypted_tally =
    lazy
      (let ballots =
         Lazy.force verified_ballots
         |> List.rev_map (fun (_, _, w, b) -> (w, read_ballot ++ b))
       in
       let sized_total_weight =
         let open Weight in
         List.fold_left (fun accu (w, _) -> accu + w) zero ballots
       in
       let encrypted_tally = E.process_ballots ballots in
       ( encrypted_tally,
         {
           sized_num_tallied = List.length ballots;
           sized_total_weight;
           sized_encrypted_tally =
             Hash.hash_string
               (string_of_encrypted_tally (swrite G.to_string) encrypted_tally);
         } ))

  let raw_shuffles = lazy (get_shuffles ())

  let shuffles_as_text =
    lazy (Lazy.force raw_shuffles |> Option.map (List.map (fun (_, _, x) -> x)))

  let shuffles =
    lazy
      (Lazy.force shuffles_as_text
      |> Option.map
           (List.map
              (shuffle_of_string (sread G.of_string) (sread G.Zq.of_string))))

  let shuffles_hash =
    lazy (Lazy.force shuffles_as_text |> Option.map (List.map sha256_b64))

  let encrypted_tally =
    lazy
      (let raw_encrypted_tally, ntally = Lazy.force raw_encrypted_tally in
       match Option.map List.rev (Lazy.force shuffles) with
       | Some (s :: _) ->
           ( E.merge_nh_ciphertexts s.shuffle_ciphertexts raw_encrypted_tally,
             ntally )
       | _ -> (raw_encrypted_tally, ntally))

  let pds = lazy (get_pds ())

  let result =
    lazy (get_result () |> Option.map (election_result_of_string read_result))

  let fsck = fsck
  let election_hash = setup_data.setup_election
end
