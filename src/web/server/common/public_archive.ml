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
open Belenios_server_core

exception Not_in_cache

let not_in_cache _ = Lwt.fail Not_in_cache

let get_roots s uuid =
  let module S = (val s : Storage_sig.BACKEND) in
  let* x = S.get (Election (uuid, Roots)) in
  match x with
  | None -> Lwt.return Events.empty_roots
  | Some x -> Lwt.return @@ roots_of_string x

let get_data s uuid x =
  let module S = (val s : Storage_sig.BACKEND) in
  S.get (Election (uuid, Data x))

let get_event s uuid x =
  let* x = get_data s uuid x in
  Lwt.return @@ Option.map event_of_string x

let get_from_data s uuid f =
  let* x = get_roots s uuid in
  match f x with None -> Lwt.return_none | Some x -> get_data s uuid x

let get_from_setup_data s uuid f =
  let* x = get_roots s uuid in
  match x.roots_setup_data with
  | None -> Lwt.return_none
  | Some x -> (
      let* x = get_data s uuid x in
      match x with
      | None -> Lwt.return_none
      | Some x -> get_data s uuid (f (setup_data_of_string x)))

let fold_on_event_payload_hashes s uuid typ last_event f accu =
  let rec loop e accu =
    let* e = get_event s uuid e in
    match e with
    | None -> assert false
    | Some e ->
        if e.event_typ = typ then
          match (e.event_payload, e.event_parent) with
          | Some payload, Some parent ->
              let* accu = f payload accu in
              loop parent accu
          | _ -> assert false
        else Lwt.return accu
  in
  loop last_event accu

let fold_on_event_payloads s uuid typ last_event f accu =
  fold_on_event_payload_hashes s uuid typ last_event
    (fun payload accu ->
      let* x = get_data s uuid payload in
      match x with None -> assert false | Some x -> f payload x accu)
    accu

let get_trustees s uuid =
  let* x = get_from_setup_data s uuid (fun x -> x.setup_trustees) in
  let@ () =
   fun cont -> match x with None -> cont () | Some x -> Lwt.return x
  in
  let msg =
    Printf.sprintf "missing trustees for election %s" (Uuid.unwrap uuid)
  in
  Lwt.fail (Failure msg)

let get_election s uuid = get_from_setup_data s uuid (fun x -> x.setup_election)

module ElectionCacheTypes = struct
  type key = uuid
  type value = (module Site_common_sig.ELECTION)
end

module ElectionCache = Ocsigen_cache.Make (ElectionCacheTypes)

exception Not_cachable

let raw_get_election s uuid =
  let* x = get_election s uuid in
  match x with
  | None -> Lwt.fail Not_cachable
  | Some x ->
      let module W =
        Election.Make
          (struct
            let raw_election = x
          end)
          (Random)
          ()
      in
      Lwt.return (module W : Site_common_sig.ELECTION)

let election_cache = new ElectionCache.cache not_in_cache ~timer:3600. 500

let election_cache_find s uuid =
  match election_cache#find_in_cache uuid with
  | x -> Lwt.return x
  | exception Not_found ->
      let* x = raw_get_election s uuid in
      election_cache#add uuid x;
      Lwt.return x

let with_election s uuid ~fallback f =
  Lwt.try_bind
    (fun () -> election_cache_find s uuid)
    f
    (function Not_cachable -> fallback () | e -> Lwt.reraise e)

let get_partial_decryptions s uuid =
  let* x = get_roots s uuid in
  match x.roots_last_pd_event with
  | None -> Lwt.return []
  | Some x ->
      fold_on_event_payloads s uuid `PartialDecryption x
        (fun _ x accu ->
          let x = owned_of_string read_hash x in
          let* pd =
            let* x = get_data s uuid x.owned_payload in
            match x with None -> assert false | Some x -> Lwt.return x
          in
          let x = { x with owned_payload = pd } in
          Lwt.return @@ (x :: accu))
        []

let get_result s uuid = get_from_data s uuid (fun x -> x.roots_result)

let get_public_creds s uuid =
  let* x = get_from_setup_data s uuid (fun x -> x.setup_credentials) in
  match x with
  | None -> assert false
  | Some x -> Lwt.return @@ public_credentials_of_string x

let get_credential_weight s uuid cred =
  let module S = (val s : Storage_sig.BACKEND) in
  let* x = S.get (Election (uuid, Credential_weight cred)) in
  match x with
  | Some x -> Lwt.return @@ Weight.of_string x
  | None ->
      Lwt.fail
        (Failure
           (Printf.sprintf "could not find credential weight of %s/%s"
              (Uuid.unwrap uuid) cred))

let get_ballot_weight s election ballot =
  let module W = (val election : Site_common_sig.ELECTION) in
  Lwt.catch
    (fun () ->
      let ballot = W.read_ballot ++ ballot in
      match W.get_credential ballot with
      | None -> failwith "missing signature"
      | Some credential ->
          get_credential_weight s W.uuid (W.G.to_string credential))
    (fun e ->
      Printf.ksprintf failwith "anomaly in get_ballot_weight (%s)"
        (Printexc.to_string e))

module BallotsCacheTypes = struct
  type key = uuid
  type value = Weight.t SMap.t
end

module BallotsCache = Ocsigen_cache.Make (BallotsCacheTypes)

let fold_on_ballots s uuid f accu =
  let* x = get_roots s uuid in
  match x.roots_last_ballot_event with
  | None -> Lwt.return accu
  | Some e -> fold_on_event_payloads s uuid `Ballot e f accu

let fold_on_ballots_weeded s election f accu =
  let module W = (val election : Site_common_sig.ELECTION) in
  let module GSet = Set.Make (W.G) in
  let* _, accu =
    fold_on_ballots s W.uuid
      (fun _ b ((seen, accu) as x) ->
        let ballot = W.read_ballot ++ b in
        match W.get_credential ballot with
        | None -> assert false
        | Some credential ->
            if GSet.mem credential seen then Lwt.return x
            else
              let seen = GSet.add credential seen in
              let* accu = f b accu in
              Lwt.return (seen, accu))
      (GSet.empty, accu)
  in
  Lwt.return accu

let raw_get_ballots s uuid =
  let@ election =
    with_election s uuid ~fallback:(fun () -> Lwt.return SMap.empty)
  in
  fold_on_ballots_weeded s election
    (fun b accu ->
      let hash = sha256_b64 b in
      let* weight = get_ballot_weight s election b in
      Lwt.return (SMap.add hash weight accu))
    SMap.empty

let ballots_cache = new BallotsCache.cache not_in_cache ~timer:3600. 10

let ballots_cache_find s uuid =
  match ballots_cache#find_in_cache uuid with
  | x -> Lwt.return x
  | exception Not_found ->
      let* x = raw_get_ballots s uuid in
      ballots_cache#add uuid x;
      Lwt.return x

let get_ballot_hashes s uuid =
  let* ballots = ballots_cache_find s uuid in
  SMap.bindings ballots |> Lwt.return

let get_ballot_by_hash s uuid hash =
  Lwt.catch
    (fun () ->
      let hash = Hash.of_b64 hash in
      get_data s uuid hash)
    (fun _ -> Lwt.return_none)

let get_owned_shuffles s uuid =
  let* x = get_roots s uuid in
  match x.roots_last_shuffle_event with
  | None -> Lwt.return_none
  | Some x ->
      let* x =
        fold_on_event_payloads s uuid `Shuffle x
          (fun h x accu ->
            Lwt.return @@ ((h, owned_of_string read_hash x) :: accu))
          []
      in
      Lwt.return_some x

let raw_get_shuffles s uuid x =
  let* x =
    Lwt_list.map_s
      (fun (h, o) ->
        let* x = get_data s uuid o.owned_payload in
        match x with None -> assert false | Some x -> Lwt.return (h, o, x))
      x
  in
  Lwt.return_some x

let get_nh_ciphertexts s uuid =
  let@ election =
    with_election s uuid ~fallback:(fun () ->
        Lwt.fail (Election_not_found (uuid, "get_nh_ciphertexts")))
  in
  let module W = (val election) in
  let* x = get_roots s uuid in
  match x.roots_last_shuffle_event with
  | None -> (
      match x.roots_encrypted_tally with
      | None -> assert false
      | Some x -> (
          let* x = get_data s uuid x in
          match x with
          | None -> assert false
          | Some x -> (
              let x = sized_encrypted_tally_of_string read_hash x in
              let* x = get_data s uuid x.sized_encrypted_tally in
              match x with
              | None -> assert false
              | Some x ->
                  encrypted_tally_of_string W.(sread G.of_string) x
                  |> W.E.extract_nh_ciphertexts
                  |> string_of_nh_ciphertexts W.(swrite G.to_string)
                  |> Lwt.return)))
  | Some x -> (
      let* x = get_event s uuid x in
      match x with
      | None -> assert false
      | Some x -> (
          match x.event_payload with
          | None -> assert false
          | Some x -> (
              let* x = get_data s uuid x in
              match x with
              | None -> assert false
              | Some x -> (
                  let x = owned_of_string read_hash x in
                  let* x = get_data s uuid x.owned_payload in
                  match x with
                  | None -> assert false
                  | Some x ->
                      let x =
                        shuffle_of_string
                          W.(sread G.of_string)
                          W.(sread G.Zq.of_string)
                          x
                      in
                      Lwt.return
                      @@ string_of_nh_ciphertexts
                           W.(swrite G.to_string)
                           x.shuffle_ciphertexts))))

let get_shuffles s uuid =
  let* x = get_owned_shuffles s uuid in
  let&* x = x in
  raw_get_shuffles s uuid x

let get_sized_encrypted_tally s uuid =
  let* roots = get_roots s uuid in
  match roots.roots_encrypted_tally with
  | None -> Lwt.return_none
  | Some x -> (
      let* x = get_data s uuid x in
      match x with None -> assert false | Some x -> Lwt.return_some x)

let get_latest_encrypted_tally s uuid =
  let@ election =
    with_election s uuid ~fallback:(fun () ->
        Lwt.fail (Election_not_found (uuid, "get_latest_encrypted_tally")))
  in
  let module W = (val election) in
  let* roots = get_roots s uuid in
  let@ tally cont =
    match roots.roots_encrypted_tally with
    | None -> Lwt.return_none
    | Some x -> (
        let* x = get_data s uuid x in
        match x with
        | None -> assert false
        | Some x -> (
            let x = sized_encrypted_tally_of_string read_hash x in
            let* x = get_data s uuid x.sized_encrypted_tally in
            match x with
            | None -> assert false
            | Some x ->
                cont @@ encrypted_tally_of_string W.(sread G.of_string) x))
  in
  let* nh = get_nh_ciphertexts s uuid in
  let nh = nh_ciphertexts_of_string W.(sread G.of_string) nh in
  let tally = W.E.merge_nh_ciphertexts nh tally in
  Lwt.return_some @@ string_of_encrypted_tally W.(swrite G.to_string) tally

let clear_ballot_cache uuid = ballots_cache#remove uuid
