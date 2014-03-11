(**************************************************************************)
(*                                BELENIOS                                *)
(*                                                                        *)
(*  Copyright © 2012-2014 Inria                                           *)
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

open Signatures
open Lwt
open Util
open Serializable_builtin_t
open Serializable_t
open Web_common

type acl =
  | Any
  | Restricted of (Web_signatures.user -> bool Lwt.t)

type election_web = {
  params_fname : string;
  public_keys_fname : string;
  featured_p : bool;
  can_read : acl;
  can_vote : acl;
}

module type WEB_BALLOT_BOX = sig
  module Ballots : Signatures.MONADIC_MAP_RO
    with type 'a m = 'a Lwt.t
    and type elt = string
    and type key = string
  module Records : Signatures.MONADIC_MAP_RO
    with type 'a m = 'a Lwt.t
    and type elt = Serializable_builtin_t.datetime * string
    and type key = string

  val cast : string -> string * datetime -> string Lwt.t
  val inject_creds : SSet.t -> unit Lwt.t
  val extract_creds : unit -> SSet.t Lwt.t
  val update_cred : old:string -> new_:string -> unit Lwt.t
end

module type WEB_ELECTION_BUNDLE =
  Signatures.ELECTION_BUNDLE with type 'a E.m = 'a Lwt.t

module type WEB_BALLOT_BOX_BUNDLE = sig
  include WEB_ELECTION_BUNDLE
  module B : WEB_BALLOT_BOX
end

type 'a web_election = {
  modules : (module WEB_BALLOT_BOX_BUNDLE with type elt = 'a);
  election : 'a Signatures.election;
  election_web : election_web;
}

let make_web_election raw_election e_meta election_web =

  let e_fingerprint = sha256_b64 raw_election in
  let wrapped_params = Serializable_j.params_of_string
    Serializable_j.read_ff_pubkey raw_election
  in
  let {ffpk_g = g; ffpk_p = p; ffpk_q = q; ffpk_y = y} = wrapped_params.e_public_key in
  let group = {g; p; q} in
  let e_params = { wrapped_params with e_public_key = y } in
  let election = {e_params; e_meta; e_pks = None; e_fingerprint} in

  let module X : WEB_BALLOT_BOX_BUNDLE with type elt = Z.t = struct
    type elt = Z.t

    module G = (val Election.finite_field group : Election.FF_GROUP)
    module M = MakeLwtRandom(struct let rng = make_rng () end)
    module E = Election.MakeElection(G)(M)

    module B : WEB_BALLOT_BOX = struct

      let suffix = "_" ^ String.map (function
        | '-' -> '_'
        | c -> c
      ) (Uuidm.to_string e_params.e_uuid)

      module Ballots = struct
        type 'a m = 'a Lwt.t
        type elt = string
        type key = string
        let table = Ocsipersist.open_table ("ballots" ^ suffix)
        let cardinal = Ocsipersist.length table
        let fold f x = Ocsipersist.fold_step f table x
      end

      module Records = struct
        type 'a m = 'a Lwt.t
        type elt = Serializable_builtin_t.datetime * string
        type key = string
        let table = Ocsipersist.open_table ("records" ^ suffix)
        let cardinal = Ocsipersist.length table
        let fold f x = Ocsipersist.fold_step f table x
      end

      let cred_table = Ocsipersist.open_table ("creds" ^ suffix)

      let extract_creds () =
        Ocsipersist.fold_step (fun k v x ->
          return (SSet.add k x)
        ) cred_table SSet.empty

      let inject_creds creds =
        lwt existing_creds = extract_creds () in
        if SSet.is_empty existing_creds then (
          Ocsigen_messages.debug (fun () ->
            "-- injecting credentials"
          );
          SSet.fold (fun x unit ->
            unit >> Ocsipersist.add cred_table x None
          ) creds (return ())
        ) else (
          if SSet.(is_empty (diff creds existing_creds)) then (
            Lwt.return ()
          ) else (
            Ocsigen_messages.warning "public_creds.txt does not match db!";
            Lwt.return ()
          )
        )

      let do_cast rawballot (user, date) =
        let voting_open = match election.e_meta with
          | Some m ->
            let date = fst date in
            let open CalendarLib.Fcalendar.Precise in
            compare (fst m.e_voting_starts_at) date <= 0 &&
            compare date (fst m.e_voting_ends_at) < 0
          | None -> true
        in
        if not voting_open then fail ElectionClosed else return () >>
        if String.contains rawballot '\n' then (
          fail (Serialization (Invalid_argument "multiline ballot"))
        ) else return () >>
        lwt ballot =
          try Lwt.return (
            Serializable_j.ballot_of_string
              Serializable_builtin_j.read_number rawballot
          ) with e -> fail (Serialization e)
        in
        lwt credential =
          match ballot.signature with
            | Some s -> Lwt.return (Z.to_string s.s_public_key)
            | None -> fail MissingCredential
        in
        lwt old_cred =
          try_lwt Ocsipersist.find cred_table credential
          with Not_found -> fail InvalidCredential
        and old_record =
          try_lwt
            lwt x = Ocsipersist.find Records.table user in
            Lwt.return (Some x)
          with Not_found -> Lwt.return None
        in
        match old_cred, old_record with
          | None, None ->
            (* first vote *)
            if E.check_ballot election ballot then (
              let hash = sha256_b64 rawballot in
              Ocsipersist.add cred_table credential (Some hash) >>
              Ocsipersist.add Ballots.table hash rawballot >>
              Ocsipersist.add Records.table user (date, credential) >>
              security_log (fun () ->
                Printf.sprintf "%s successfully cast ballot %s" user hash
              ) >> return hash
            ) else (
              fail ProofCheck
            )
          | Some h, Some (_, old_credential) ->
            (* revote *)
            if credential = old_credential then (
              if E.check_ballot election ballot then (
                lwt old_ballot = Ocsipersist.find Ballots.table h in
                Ocsipersist.remove Ballots.table h >>
                security_log (fun () ->
                  Printf.sprintf "%s successfully removed ballot %S" user old_ballot
                ) >>
                let hash = sha256_b64 rawballot in
                Ocsipersist.add cred_table credential (Some hash) >>
                Ocsipersist.add Ballots.table hash rawballot >>
                Ocsipersist.add Records.table user (date, credential) >>
                security_log (fun () ->
                  Printf.sprintf "%s successfully cast ballot %s" user hash
                ) >> return hash
              ) else (
                fail ProofCheck
              )
            ) else (
              security_log (fun () ->
                Printf.sprintf "%s attempted to revote with already used credential %s" user credential
              ) >> fail WrongCredential
            )
          | None, Some _ ->
            security_log (fun () ->
              Printf.sprintf "%s attempted to revote using a new credential %s" user credential
            ) >> fail RevoteNotAllowed
          | Some _, None ->
            security_log (fun () ->
              Printf.sprintf "%s attempted to vote with already used credential %s" user credential
            ) >> fail ReusedCredential

      let do_update_cred ~old ~new_ =
        match_lwt Ocsipersist.fold_step (fun k v x ->
          if sha256_hex k = old then (
            match v with
              | Some _ -> fail UsedCredential
              | None -> return (Some k)
          ) else return x
        ) cred_table None with
        | None -> fail CredentialNotFound
        | Some x ->
          Ocsipersist.remove cred_table x >>
          Ocsipersist.add cred_table new_ None

      let mutex = Lwt_mutex.create ()

      let cast rawballot (user, date) =
        Lwt_mutex.with_lock mutex (fun () -> do_cast rawballot (user, date))

      let update_cred ~old ~new_ =
        Lwt_mutex.with_lock mutex (fun () -> do_update_cred ~old ~new_)
    end
  end in
  {
    modules = (module X : WEB_BALLOT_BOX_BUNDLE with type elt = Z.t);
    election;
    election_web;
  }
