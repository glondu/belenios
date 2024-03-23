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
open Web_serializable_j
open Web_common

let get_from_data uuid f =
  let* x = Web_events.get_roots ~uuid in
  match f x with
  | None -> Lwt.return_none
  | Some x -> Web_events.get_data ~uuid x

let get_from_setup_data uuid f =
  let* x = Web_events.get_roots ~uuid in
  match x.roots_setup_data with
  | None -> Lwt.return_none
  | Some x -> (
      let* x = Web_events.get_data ~uuid x in
      match x with
      | None -> Lwt.return_none
      | Some x -> Web_events.get_data ~uuid (f (setup_data_of_string x)))

let fold_on_event_payload_hashes uuid typ last_event f accu =
  let rec loop e accu =
    let* e = Web_events.get_event ~uuid e in
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

let fold_on_event_payloads uuid typ last_event f accu =
  fold_on_event_payload_hashes uuid typ last_event
    (fun payload accu ->
      let* x = Web_events.get_data ~uuid payload in
      match x with None -> assert false | Some x -> f payload x accu)
    accu

let get_trustees uuid =
  let* x = get_from_setup_data uuid (fun x -> x.setup_trustees) in
  let@ () =
   fun cont -> match x with None -> cont () | Some x -> Lwt.return x
  in
  let msg =
    Printf.sprintf "missing trustees for election %s" (Uuid.unwrap uuid)
  in
  Lwt.fail (Failure msg)

let get_election uuid = get_from_setup_data uuid (fun x -> x.setup_election)

let get_partial_decryptions uuid =
  let* x = Web_events.get_roots ~uuid in
  match x.roots_last_pd_event with
  | None -> Lwt.return []
  | Some x ->
      fold_on_event_payloads uuid `PartialDecryption x
        (fun _ x accu ->
          let x = owned_of_string read_hash x in
          let* pd =
            let* x = Web_events.get_data ~uuid x.owned_payload in
            match x with None -> assert false | Some x -> Lwt.return x
          in
          let x = { x with owned_payload = pd } in
          Lwt.return @@ (x :: accu))
        []

let get_result uuid = get_from_data uuid (fun x -> x.roots_result)

module WeightsCacheTypes = struct
  type key = uuid
  type value = weight SMap.t
end

module WeightsCache = Ocsigen_cache.Make (WeightsCacheTypes)

let get_public_creds uuid =
  let* x = get_from_setup_data uuid (fun x -> x.setup_credentials) in
  match x with
  | None -> assert false
  | Some x -> Lwt.return @@ public_credentials_of_string x

let raw_get_credential_cache uuid =
  let* x = get_public_creds uuid in
  Lwt.return
  @@ List.fold_left
       (fun weights x ->
         let p = parse_public_credential Fun.id x in
         let weight = Option.value ~default:Weight.one p.weight in
         SMap.add p.credential weight weights)
       SMap.empty x

let credential_cache =
  new WeightsCache.cache raw_get_credential_cache ~timer:3600. 10

let get_credential_weight uuid cred =
  Lwt.catch
    (fun () ->
      let* xs = credential_cache#find uuid in
      Lwt.return @@ SMap.find cred xs)
    (fun _ ->
      Lwt.fail
        (Failure
           (Printf.sprintf "could not find credential weight of %s/%s"
              (Uuid.unwrap uuid) cred)))

let get_ballot_weight election ballot =
  let module W = (val election : Site_common_sig.ELECTION) in
  Lwt.catch
    (fun () ->
      let ballot = W.read_ballot ++ ballot in
      match W.get_credential ballot with
      | None -> failwith "missing signature"
      | Some credential ->
          get_credential_weight W.uuid (W.G.to_string credential))
    (fun e ->
      Printf.ksprintf failwith "anomaly in get_ballot_weight (%s)"
        (Printexc.to_string e))

module BallotsCacheTypes = struct
  type key = uuid
  type value = Weight.t SMap.t
end

module BallotsCache = Ocsigen_cache.Make (BallotsCacheTypes)

let fold_on_ballots uuid f accu =
  let* x = Web_events.get_roots ~uuid in
  match x.roots_last_ballot_event with
  | None -> Lwt.return accu
  | Some e -> fold_on_event_payloads uuid `Ballot e f accu

let fold_on_ballots_weeded uuid f accu =
  let@ raw_election cont =
    let* x = get_election uuid in
    match x with None -> Lwt.return accu | Some x -> cont x
  in
  let module W =
    Election.Make
      (struct
        let raw_election = raw_election
      end)
      (Random)
      ()
  in
  let module GSet = Set.Make (W.G) in
  let* _, accu =
    fold_on_ballots uuid
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

let raw_get_ballots uuid =
  let* x = get_election uuid in
  match x with
  | None -> Lwt.return SMap.empty
  | Some x ->
      let module W =
        Election.Make
          (struct
            let raw_election = x
          end)
          (Random)
          ()
      in
      fold_on_ballots_weeded uuid
        (fun b accu ->
          let hash = sha256_b64 b in
          let* weight = get_ballot_weight (module W) b in
          Lwt.return (SMap.add hash weight accu))
        SMap.empty

let ballots_cache = new BallotsCache.cache raw_get_ballots ~timer:3600. 10

let get_ballot_hashes uuid =
  let* ballots = ballots_cache#find uuid in
  SMap.bindings ballots |> Lwt.return

let get_ballot_by_hash uuid hash =
  Lwt.catch
    (fun () ->
      let hash = Hash.of_b64 hash in
      Web_events.get_data ~uuid hash)
    (fun _ -> Lwt.return_none)

let get_owned_shuffles uuid =
  let* x = Web_events.get_roots ~uuid in
  match x.roots_last_shuffle_event with
  | None -> Lwt.return_none
  | Some x ->
      let* x =
        fold_on_event_payloads uuid `Shuffle x
          (fun h x accu ->
            Lwt.return @@ ((h, owned_of_string read_hash x) :: accu))
          []
      in
      Lwt.return_some x

let raw_get_shuffles uuid x =
  let* x =
    Lwt_list.map_s
      (fun (h, o) ->
        let* x = Web_events.get_data ~uuid o.owned_payload in
        match x with None -> assert false | Some x -> Lwt.return (h, o, x))
      x
  in
  Lwt.return_some x

let get_nh_ciphertexts election =
  let module W = (val election : Site_common_sig.ELECTION) in
  let uuid = W.uuid in
  let* x = Web_events.get_roots ~uuid in
  match x.roots_last_shuffle_event with
  | None -> (
      match x.roots_encrypted_tally with
      | None -> assert false
      | Some x -> (
          let* x = Web_events.get_data ~uuid x in
          match x with
          | None -> assert false
          | Some x -> (
              let x = sized_encrypted_tally_of_string read_hash x in
              let* x = Web_events.get_data ~uuid x.sized_encrypted_tally in
              match x with
              | None -> assert false
              | Some x ->
                  encrypted_tally_of_string W.(sread G.of_string) x
                  |> W.E.extract_nh_ciphertexts
                  |> string_of_nh_ciphertexts W.(swrite G.to_string)
                  |> Lwt.return)))
  | Some x -> (
      let* x = Web_events.get_event ~uuid x in
      match x with
      | None -> assert false
      | Some x -> (
          match x.event_payload with
          | None -> assert false
          | Some x -> (
              let* x = Web_events.get_data ~uuid x in
              match x with
              | None -> assert false
              | Some x -> (
                  let x = owned_of_string read_hash x in
                  let* x = Web_events.get_data ~uuid x.owned_payload in
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

let get_shuffles uuid =
  let* x = get_owned_shuffles uuid in
  let&* x = x in
  raw_get_shuffles uuid x

let get_sized_encrypted_tally uuid =
  let* roots = Web_events.get_roots ~uuid in
  match roots.roots_encrypted_tally with
  | None -> Lwt.return_none
  | Some x -> (
      let* x = Web_events.get_data ~uuid x in
      match x with None -> assert false | Some x -> Lwt.return_some x)

let clear_ballot_cache uuid = ballots_cache#remove uuid
