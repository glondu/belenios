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
open Belenios
open Common

module type GETTERS = sig
  val fsck : unit -> unit Lwt.t
  val setup_data : setup_data Lwt.t
  val raw_election : string Lwt.t
  val get_trustees : unit -> string option Lwt.t
  val get_public_creds : unit -> string list option Lwt.t
  val get_ballots : unit -> string list option Lwt.t

  val get_encrypted_tally :
    unit -> (string * hash sized_encrypted_tally) option Lwt.t

  val get_shuffles : unit -> (hash * hash owned * string) list option Lwt.t
  val get_pds : unit -> (hash * hash owned * string) list option Lwt.t
  val get_result : unit -> string option Lwt.t
  val get_final : unit -> hash option Lwt.t
end

module type PARAMS = sig
  val file : string
end

module MakeGetters (X : PARAMS) : GETTERS = struct
  let index = Tool_events.get_index ~file:X.file

  let roots =
    let* index = index in
    Lwt.return @@ Tool_events.get_roots index

  let get_data x =
    let* index = index in
    Tool_events.get_data index x

  let fsck () =
    let* index = index in
    Tool_events.fsck index

  let setup_data =
    let* roots = roots in
    match roots.roots_setup_data with
    | None -> failcmd "setup data are missing"
    | Some x -> (
        let* x = get_data x in
        match x with
        | None -> failcmd "could not get setup data"
        | Some x -> Lwt.return @@ setup_data_of_string x)

  let raw_election =
    let* setup_data = setup_data in
    let* x = get_data setup_data.setup_election in
    match x with
    | None -> failcmd "could not get election"
    | Some x -> Lwt.return x

  let get_public_creds () =
    let* setup_data = setup_data in
    let* x = get_data setup_data.setup_credentials in
    Lwt.return @@ Option.map public_credentials_of_string x

  let get_trustees () =
    let* setup_data = setup_data in
    get_data setup_data.setup_trustees

  let get_ballots () =
    let* roots = roots in
    let* index = index in
    match roots.roots_last_ballot_event with
    | None -> Lwt.return_some []
    | Some x ->
        let* x =
          Tool_events.fold_on_event_payloads index `Ballot x
            (fun x accu -> Lwt.return @@ (x :: accu))
            []
        in
        Lwt.return_some x

  let ( let& ) x f = match x with None -> Lwt.return_none | Some x -> f x

  let get_encrypted_tally () =
    let* roots = roots in
    let ( let*& ) x f =
      let* x = x in
      let& x = x in
      f x
    in
    let& x = roots.roots_encrypted_tally in
    let*& x = get_data x in
    let x = sized_encrypted_tally_of_string read_hash x in
    let*& y = get_data x.sized_encrypted_tally in
    Lwt.return_some (y, x)

  let get_shuffles () =
    let* roots = roots in
    let* index = index in
    let& x = roots.roots_last_shuffle_event in
    let* x =
      Tool_events.fold_on_event_payload_hashes index `Shuffle x
        (fun x accu ->
          let* xx = get_data x in
          match xx with
          | None -> failwith "could not get shuffle"
          | Some y -> (
              let owned = owned_of_string read_hash y in
              let* xx = get_data owned.owned_payload in
              match xx with
              | None -> failwith "could not get shuffle payload"
              | Some z -> Lwt.return @@ ((x, owned, z) :: accu)))
        []
    in
    Lwt.return_some x

  let get_pds () =
    let* roots = roots in
    let* index = index in
    let& x = roots.roots_last_pd_event in
    let* x =
      Tool_events.fold_on_event_payload_hashes index `PartialDecryption x
        (fun x accu ->
          let* xx = get_data x in
          match xx with
          | None -> failwith "could not get partial decryption"
          | Some y -> (
              let owned = owned_of_string read_hash y in
              let* xx = get_data owned.owned_payload in
              match xx with
              | None -> failwith "could not get partial decryption payload"
              | Some z -> Lwt.return @@ ((x, owned, z) :: accu)))
        []
    in
    Lwt.return_some x

  let get_result () =
    let* roots = roots in
    let& x = roots.roots_result in
    get_data x

  let get_final () =
    let* roots = roots in
    let& _ = roots.roots_result in
    let* index = index in
    Lwt.return @@ Tool_events.get_last_event index
end

module type ELECTION_DATA = sig
  type s
  type t
  type r

  module Cred :
    Credential.S
      with type private_key := s
       and type public_key := t
       and type 'a m := 'a

  val trustees_as_string : string option Lwt.t
  val trustees : (t, s) trustees option Lwt.t
  val pks : t array Lwt.t Lazy.t
  val raw_public_creds : string list option Lwt.t Lazy.t
  val public_creds_weights : (bool * weight SMap.t) option Lwt.t Lazy.t
  val raw_ballots : string list option Lwt.t Lazy.t
  val verified_ballots : (hash * string * weight * string) list Lwt.t Lazy.t
  val unverified_ballots : (hash * string * weight * string) list Lwt.t Lazy.t

  val pre_cast :
    ?skip_ballot_check:bool ->
    SSet.t ->
    string ->
    (hash * (string * weight * string), cast_error) result Lwt.t

  val raw_encrypted_tally :
    (t encrypted_tally * hash sized_encrypted_tally) Lwt.t Lazy.t

  val raw_shuffles : (hash * hash owned * string) list option Lwt.t Lazy.t
  val shuffles : (t, s) shuffle list option Lwt.t Lazy.t
  val shuffles_hash : string list option Lwt.t Lazy.t

  val encrypted_tally :
    (t encrypted_tally * hash sized_encrypted_tally) Lwt.t Lazy.t

  val pds : (hash * hash owned * string) list option Lwt.t Lazy.t
  val result : r election_result option Lwt.t Lazy.t
  val fsck : unit -> unit Lwt.t
  val election_hash : hash Lwt.t
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
    let* trustees_as_string = trustees_as_string in
    Lwt.return
    @@ Option.map
         (trustees_of_string (sread G.of_string) (sread G.Zq.of_string))
         trustees_as_string

  let pks =
    lazy
      (let* trustees = trustees in
       let public_keys_with_pok =
         trustees
         |> Option.map
              (List.map (function
                 | `Single x -> [ x ]
                 | `Pedersen t -> Array.to_list t.t_verification_keys)
              >> List.flatten >> Array.of_list)
       in
       let public_keys =
         Option.map
           (Array.map (fun pk -> pk.trustee_public_key))
           public_keys_with_pok
       in
       match public_keys with
       | Some pks -> Lwt.return pks
       | None -> failwith "missing public keys")

  let raw_public_creds = lazy (get_public_creds ())

  module rec Cred :
    (Credential.S
      with type private_key := G.Zq.t
       and type public_key := G.t
       and type 'a m := 'a) =
    Credential.Make
      (G)
      (struct
        type 'a t = 'a

        let return = Fun.id
        let bind x f = f x
        let pause () = ()
        let uuid = uuid
        let get_salt _ = None
      end)

  let public_creds_weights =
    lazy
      (let* x = Lazy.force raw_public_creds in
       x
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
               (false, SMap.empty))
       |> Lwt.return)

  let public_creds =
    lazy
      (let* x = Lazy.force public_creds_weights in
       x |> Option.map snd |> Lwt.return)

  let raw_ballots = lazy (get_ballots ())

  let pre_cast ?(skip_ballot_check = false) ballot_box rawballot =
    let hash = Hash.hash_string rawballot in
    let ballot_id = Hash.to_b64 hash in
    let@ creds cont =
      let* x = Lazy.force public_creds in
      match x with
      | None -> failwith "missing public credentials"
      | Some creds -> cont creds
    in
    let is_duplicate = SSet.mem ballot_id ballot_box in
    let@ rc cont =
      match (is_duplicate, E.check_rawballot rawballot) with
      | true, _ -> Lwt.return @@ Error `DuplicateBallot
      | _, (Error _ as e) -> Lwt.return e
      | _, Ok rc -> cont rc
    in
    match SMap.find_opt rc.rc_credential creds with
    | None -> Lwt.return @@ Error `InvalidCredential
    | Some w when skip_ballot_check || rc.rc_check () ->
        Lwt.return @@ Ok (hash, (rc.rc_credential, w, rawballot))
    | Some _ -> Lwt.return @@ Error `InvalidBallot

  let collect_ballots ?(skip_ballot_check = false) () =
    let* ballot_box =
      let rec cast_all accu seen = function
        | [] -> Lwt.return accu
        | b :: bs -> (
            let* x = pre_cast seen b ~skip_ballot_check in
            match x with
            | Error e ->
                Printf.ksprintf failwith "error while casting ballot %s: %s"
                  (sha256_b64 b) (string_of_cast_error e)
            | Ok (hash, x) ->
                let ballot_id = Hash.to_b64 hash in
                cast_all ((hash, x) :: accu) (SSet.add ballot_id seen) bs)
      in
      let* x = Lazy.force raw_ballots in
      match x with
      | None -> Lwt.return_nil
      | Some bs -> cast_all [] SSet.empty bs
    in
    List.fold_left
      (fun ((seen, bs) as accu) (h, (credential, w, b)) ->
        if SSet.mem credential seen then accu
        else (SSet.add credential seen, (h, credential, w, b) :: bs))
      (SSet.empty, []) ballot_box
    |> snd |> Lwt.return

  let verified_ballots = lazy (collect_ballots ())
  let unverified_ballots = lazy (collect_ballots ~skip_ballot_check:true ())

  let raw_encrypted_tally =
    lazy
      (let* ballots =
         let* x = Lazy.force unverified_ballots in
         x
         |> List.rev_map (fun (_, _, w, b) -> (w, read_ballot ++ b))
         |> Lwt.return
       in
       let sized_total_weight =
         let open Weight in
         List.fold_left (fun accu (w, _) -> accu + w) zero ballots
       in
       let encrypted_tally = E.process_ballots ballots in
       Lwt.return
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
    lazy
      (let* x = Lazy.force raw_shuffles in
       x |> Option.map (List.map (fun (_, _, x) -> x)) |> Lwt.return)

  let shuffles =
    lazy
      (let* x = Lazy.force shuffles_as_text in
       x
       |> Option.map
            (List.map
               (shuffle_of_string (sread G.of_string) (sread G.Zq.of_string)))
       |> Lwt.return)

  let shuffles_hash =
    lazy
      (let* x = Lazy.force shuffles_as_text in
       x |> Option.map (List.map sha256_b64) |> Lwt.return)

  let encrypted_tally =
    lazy
      (let* x = get_encrypted_tally () in
       match x with
       | None -> failwith "encrypted tally is missing"
       | Some (x, ntally) -> (
           let raw_encrypted_tally =
             encrypted_tally_of_string (sread G.of_string) x
           in
           let* x = Lazy.force shuffles in
           match Option.map List.rev x with
           | Some (s :: _) ->
               Lwt.return
                 ( E.merge_nh_ciphertexts s.shuffle_ciphertexts
                     raw_encrypted_tally,
                   ntally )
           | _ -> Lwt.return (raw_encrypted_tally, ntally)))

  let pds = lazy (get_pds ())

  let result =
    lazy
      (let* x = get_result () in
       x |> Option.map (election_result_of_string read_result) |> Lwt.return)

  let fsck = fsck

  let election_hash =
    let* setup_data = setup_data in
    Lwt.return setup_data.setup_election
end
