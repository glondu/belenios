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

open Lwt
open Platform
open Serializable_builtin_t
open Serializable_j
open Signatures
open Common
open Web_serializable_t
open Web_signatures
open Web_common
open Web_services

let ( / ) = Filename.concat

let can_read m user =
  match m.e_readers with
  | None -> false
  | Some acl ->
    match user with
    | None -> acl = `Any (* readers can be anonymous *)
    | Some u -> check_acl (Some acl) u

let can_vote m user =
  match m.e_voters with
  | None -> false
  | Some acl ->
    match user with
    | None -> false (* voters must log in *)
    | Some u -> check_acl (Some acl) u

module type REGISTRATION = sig
  module W : WEB_ELECTION_
  module Register (X : EMPTY) : ELECTION_HANDLERS
end

module type REGISTRABLE = sig
  module W : sig
    include ELECTION_DATA
    include WEB_PARAMS
    module E : ELECTION with type elt = G.t
  end
  module Register (X : EMPTY) : REGISTRATION
end

module Make (D : ELECTION_DATA) (P : WEB_PARAMS) : REGISTRABLE = struct

  module W = struct
    include D
    include P
    module M = MakeLwtRandom(struct let rng = make_rng () end)
    module E = Election.MakeElection(G)(M)
  end

  module Register (X : EMPTY) : REGISTRATION = struct

    let uuid = Uuidm.to_string D.election.e_params.e_uuid
    let base_path = ["elections"; uuid]

    module N = struct
      let name = uuid
      let path = base_path
      let kind = `Election P.dir

      let auth_config =
        match P.metadata.e_auth_config with
        | None -> []
        | Some xs -> xs
    end

    module Auth = Web_auth.Make (N)

    module W = struct
      include W

      module B : WEB_BALLOT_BOX = struct

        let suffix = "_" ^ String.map (function
          | '-' -> '_'
          | c -> c
        ) uuid

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
          type elt = datetime * string
          type key = string
          let table = Ocsipersist.open_table ("records" ^ suffix)
          let cardinal = Ocsipersist.length table
          let fold f x = Ocsipersist.fold_step f table x
        end

        let cred_table = Ocsipersist.open_table ("creds" ^ suffix)

        let inject_cred cred =
          try_lwt
            lwt _ = Ocsipersist.find cred_table cred in
            failwith "trying to add duplicate credential"
          with Not_found ->
            Ocsipersist.add cred_table cred None

        let do_cast rawballot (user, date) =
          let voting_open =
            let compare a b =
              match a, b with
              | Some a, Some b -> datetime_compare a b
              | _, _ -> -1
            in
            compare metadata.e_voting_starts_at (Some date) <= 0 &&
            compare (Some date) metadata.e_voting_ends_at < 0
          in
          if not voting_open then fail ElectionClosed else return () >>
          if String.contains rawballot '\n' then (
            fail (Serialization (Invalid_argument "multiline ballot"))
          ) else return () >>
          lwt ballot =
            try Lwt.return (ballot_of_string G.read rawballot)
            with e -> fail (Serialization e)
          in
          lwt credential =
            match ballot.signature with
              | Some s -> Lwt.return (G.to_string s.s_public_key)
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

        let do_write f =
          Lwt_io.(with_file ~mode:Output (dir / string_of_election_file f))

        let do_write_ballots () =
          do_write ESBallots (fun oc ->
            Ocsipersist.iter_step (fun _ x ->
              Lwt_io.write_line oc x
            ) Ballots.table
          )

        let do_write_creds () =
          do_write ESCreds (fun oc ->
            Ocsipersist.iter_step (fun x _ ->
              Lwt_io.write_line oc x
            ) cred_table
          )

        let do_write_records () =
          do_write ESRecords (fun oc ->
            Ocsipersist.iter_step (fun u (d, _) ->
              Printf.sprintf "%s %S\n" (string_of_datetime d) u |>
              Lwt_io.write oc
            ) Records.table
          )

        let mutex = Lwt_mutex.create ()

        let cast rawballot (user, date) =
          Lwt_mutex.with_lock mutex (fun () ->
            lwt r = do_cast rawballot (user, date) in
            do_write_ballots () >>
            do_write_records () >>
            return r
          )

        let update_cred ~old ~new_ =
          Lwt_mutex.with_lock mutex (fun () ->
            lwt r = do_update_cred ~old ~new_ in
            do_write_creds () >> return r
          )

        let update_files () =
          Lwt_mutex.with_lock mutex (fun () ->
            do_write_ballots () >>
            do_write_records () >>
            do_write_creds ()
          )

      end

      module S = Auth.Services

    end

    module Register (X : EMPTY) : ELECTION_HANDLERS = struct
      open Eliom_registration

      module L = struct
        let login x = Eliom_service.preapply election_login ((W.election.e_params.e_uuid, ()), x)
        let logout = Eliom_service.preapply election_logout (W.election.e_params.e_uuid, ())
      end

      let () =
        let module T = Web_templates.Login (W.S) (L) in
        let templates = (module T : LOGIN_TEMPLATES) in
        Auth.register templates N.auth_config

      let login service () =
        lwt cont = Eliom_reference.get Web_services.cont in
        Auth.Handlers.do_login service cont ()

      let logout () () =
        lwt cont = Eliom_reference.get Web_services.cont in
        Auth.Handlers.do_logout cont ()

      module T = Web_templates

      let if_eligible acl f () x =
        lwt user = W.S.get_user () in
        if acl W.metadata user then
          f user x
        else
          forbidden ()

      let scope = Eliom_common.default_session_scope

      let ballot = Eliom_reference.eref ~scope None
      let cast_confirmed = Eliom_reference.eref ~scope None

      let home =
        (if_eligible can_read
           (fun user () ->
             Eliom_reference.unset ballot >>
             let cont () () =
               Redirection.send
                 (Eliom_service.preapply
                    election_home (W.election.e_params.e_uuid, ()))
             in
             Eliom_reference.set Web_services.cont cont >>
             match_lwt Eliom_reference.get cast_confirmed with
             | Some result ->
               Eliom_reference.unset cast_confirmed >>
               T.cast_confirmed (module W) ~result () >>= Html5.send
             | None -> T.election_home (module W) () >>= Html5.send
           )
        )

      let admin site_user is_featured =
        (fun () () ->
          match site_user with
          | Some u when W.metadata.e_owner = Some u ->
            T.election_admin (module W) ~is_featured () >>= Html5.send
          | _ -> forbidden ()
        )

      let content_type_of_file = function
        | ESRaw | ESKeys | ESBallots -> "application/json"
        | ESCreds | ESRecords -> "text/plain"

      let handle_pseudo_file u f site_user =
        lwt () =
          if f = ESRecords then (
            match site_user with
            | Some u when W.metadata.e_owner = Some u -> return ()
            | _ -> forbidden ()
          ) else return ()
        in
        let content_type = content_type_of_file f in
        File.send ~content_type (W.dir / string_of_election_file f)

      let election_dir site_user =
        (fun f () ->
          let cont () () =
            Redirection.send
              (Eliom_service.preapply
                 election_dir
                 (W.election.e_params.e_uuid, f))
          in
          Eliom_reference.set Web_services.cont cont >>
          handle_pseudo_file () f site_user
        )

      let election_update_credential site_user =
        (fun () () ->
          match site_user with
          | Some u ->
            if W.metadata.e_owner = Some u then (
              T.update_credential (module W) () >>= Html5.send
            ) else (
              forbidden ()
            )
          | _ -> forbidden ()
        )

      let election_update_credential_post site_user =
        (fun () (old, new_) ->
          match site_user with
          | Some u ->
            if W.metadata.e_owner = Some u then (
              try_lwt
                W.B.update_cred ~old ~new_ >>
                String.send ("OK", "text/plain")
              with Error e ->
                String.send ("Error: " ^ explain_error e, "text/plain")
            ) >>= (fun x -> return @@ cast_unknown_content_kind x)
            else (
              forbidden ()
            )
          | _ -> forbidden ()
        )

      let election_vote =
        (if_eligible can_read
           (fun user () ->
             Eliom_reference.unset ballot >>
             let cont () () =
               Redirection.send
                 (Eliom_service.preapply
                    election_vote (W.election.e_params.e_uuid, ()))
             in
             Eliom_reference.set Web_services.cont cont >>
             let uuid_s = Uuidm.to_string W.election.e_params.e_uuid in
             Redirection.send
               (Eliom_service.preapply
                  (Eliom_service.static_dir_with_params
                     ~get_params:(Eliom_parameter.string "election_url") ())
                  (["static"; "vote.html"],
                   "../elections/" ^ uuid_s ^ "/"))
           )
        )

      let election_cast_confirm () () =
        match_lwt Eliom_reference.get ballot with
        | Some the_ballot ->
          begin
            Eliom_reference.unset ballot >>
            match_lwt W.S.get_user () with
            | Some u ->
              let b = check_acl W.metadata.e_voters u in
              if b then (
                let record = Web_auth.string_of_user u, now () in
                lwt result =
                  try_lwt
                    lwt hash = W.B.cast the_ballot record in
                    return (`Valid hash)
                  with Error e -> return (`Error e)
                in
                Eliom_reference.unset ballot >>
                Eliom_reference.set cast_confirmed (Some result) >>
                Redirection.send
                  (Eliom_service.preapply
                     election_home (W.election.e_params.e_uuid, ()))
              ) else forbidden ()
            | None -> forbidden ()
          end
        | None -> fail_http 404

      let ballot_received user =
        let can_vote = can_vote W.metadata user in
        T.cast_confirmation (module W) ~can_vote ()

      let election_cast =
        (if_eligible can_read
           (fun user () ->
             let cont () () =
               Redirection.send
                 (Eliom_service.preapply
                    election_cast (W.election.e_params.e_uuid, ()))
             in
             Eliom_reference.set Web_services.cont cont >>
             match_lwt Eliom_reference.get ballot with
             | Some _ -> ballot_received user >>= Html5.send
             | None -> T.cast_raw (module W) () >>= Html5.send
           )
        )

      let election_cast_post =
        (if_eligible can_read
           (fun user (ballot_raw, ballot_file) ->
             lwt the_ballot = match ballot_raw, ballot_file with
               | Some ballot, None -> return ballot
               | None, Some fi ->
                 let fname = fi.Ocsigen_extensions.tmp_filename in
                 Lwt_stream.to_string (Lwt_io.chars_of_file fname)
               | _, _ -> fail_http 400
             in
             let cont () () =
               Redirection.send
                 (Eliom_service.preapply
                    Web_services.election_cast (W.election.e_params.e_uuid, ()))
             in
             Eliom_reference.set Web_services.cont cont >>
             Eliom_reference.set ballot (Some the_ballot) >>
             match user with
             | None ->
                Redirection.send
                  (Eliom_service.preapply
                     Web_services.election_login
                     ((W.election.e_params.e_uuid, ()), None))
             | Some u -> cont () ()
           )
        )

      let election_pretty_ballots start () =
        lwt user = W.S.get_user () in
        if can_read W.metadata user then (
          lwt res, _ =
            W.B.Ballots.fold
              (fun h _ (accu, i) ->
               if i >= start && i < start+50 then
                 return (h :: accu, i+1)
               else return (accu, i+1)
              ) ([], 1)
          in T.pretty_ballots (module W) res () >>= Html5.send
        ) else forbidden ()

      let election_pretty_ballot hash () =
        lwt user = W.S.get_user () in
        if can_read W.metadata user then (
          lwt ballot =
            W.B.Ballots.fold
              (fun h b accu ->
               if h = hash then return (Some b) else return accu
              ) None
          in
          match ballot with
          | None -> fail_http 404
          | Some b ->
             String.send (b, "application/json") >>=
             (fun x -> return @@ cast_unknown_content_kind x)
        ) else forbidden ()

    end

  end

end
