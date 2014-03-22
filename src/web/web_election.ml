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
open Web_signatures
open Lwt
open Util
open Serializable_builtin_t
open Serializable_t
open Web_serializable_t
open Web_common

let can_read m user =
  match m.e_readers with
  | None -> false
  | Some acls ->
    match user with
    | None -> List.mem `Any acls (* readers can be anonymous *)
    | Some u -> check_acl (Some acls) u.user_user

let can_vote m user =
  match m.e_voters with
  | None -> false
  | Some acls ->
    match user with
    | None -> false (* voters must log in *)
    | Some u -> check_acl (Some acls) u.user_user

module type REGISTRATION = sig
  module W : WEB_ELECTION
  module Register (S : SITE_SERVICES) (T : ELECTION_TEMPLATES) : EMPTY
end

let make {raw_election; metadata; featured; params_fname; public_keys_fname} =

  let e_fingerprint = sha256_b64 raw_election in
  let wrapped_params = Serializable_j.params_of_string
    Serializable_j.read_ff_pubkey raw_election
  in
  let {ffpk_g = g; ffpk_p = p; ffpk_q = q; ffpk_y = y} = wrapped_params.e_public_key in
  let group = {g; p; q} in
  let e_params = { wrapped_params with e_public_key = y } in

  let module R : REGISTRATION = struct

    module W : WEB_ELECTION = struct
      module G = (val Election.finite_field group : Election.FF_GROUP)
      module M = MakeLwtRandom(struct let rng = make_rng () end)
      module E = Election.MakeElection(G)(M)

      let election = {e_params; e_pks = None; e_fingerprint}
      let metadata = metadata

      let public_keys_fname = public_keys_fname
      let params_fname = params_fname
      let featured = featured

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
              Printf.sprintf
                "Injecting credentials for %s"
                (Uuidm.to_string e_params.e_uuid)
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
          let voting_open =
            let compare a b =
              let open CalendarLib.Fcalendar.Precise in
              match a, b with
              | Some a, Some b -> compare (fst a) (fst b)
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

      open Eliom_service
      open Eliom_parameter

      module S : ELECTION_SERVICES = struct

        let base_path = ["elections"; Uuidm.to_string election.e_params.e_uuid]
        let make_path x = base_path @ x
        let root = make_path [""]

        let home = service
          ~path:root
          ~get_params:unit
          ()

        let election_dir = service
          ~path:root
          ~priority:(-1)
          ~get_params:(suffix (election_file "file"))
          ()

        let election_booth = static_dir_with_params
          ~get_params:(string "election_url")
          ()

        let booth_path = ["booth"; "vote.html"]

        let root_rel_to_booth = root
          |> Eliom_uri.reconstruct_relative_url_path booth_path
          |> String.concat "/"

        let booth =
          preapply election_booth (booth_path, root_rel_to_booth)

        let election_update_credential = service
          ~path:(make_path ["update-cred"])
          ~get_params:unit
          ()

        let election_update_credential_post = post_service
          ~fallback:election_update_credential
          ~post_params:(string "old_credential" ** string "new_credential")
          ()

        let election_vote = service
          ~path:(make_path ["vote"])
          ~get_params:unit
          ()

        let election_cast = service
          ~path:(make_path ["cast"])
          ~get_params:unit
          ()

        let election_cast_post = post_service
          ~fallback:election_cast
          ~post_params:(opt (string "encrypted_vote") ** opt (file "encrypted_vote_file"))
          ()

      end

    end

    module Register (S : SITE_SERVICES) (T : ELECTION_TEMPLATES) : EMPTY = struct
      open Eliom_registration

      let ballot = Eliom_reference.eref
        ~scope:Eliom_common.default_session_scope
        (None : string option)

      let if_eligible get_user acl f () x =
        lwt user = get_user () in
        if acl metadata user then
          f user x
        else
          forbidden ()

      let () = Html5.register ~service:W.S.home
        (if_eligible S.get_logged_user can_read
           (fun user () ->
             let module X = struct let s = W.S.home end in
             let x = (module X : SAVED_SERVICE) in
             Eliom_reference.set S.saved_service x >>
             T.home ~user ()
           )
        )

      let f_raw user () =
        return params_fname

      let f_keys user () =
        return public_keys_fname

      let f_creds user () =
        lwt creds = W.B.extract_creds () in
        let s = SSet.fold (fun x accu ->
          (fun () -> return (Ocsigen_stream.of_string (x^"\n"))) :: accu
        ) creds [] in
        return (List.rev s, "text/plain")

      let f_ballots user () =
        (* TODO: streaming *)
        lwt ballots = W.B.Ballots.fold (fun _ x xs ->
          return ((x^"\n")::xs)
        ) [] in
        let s = List.map (fun b () ->
          return (Ocsigen_stream.of_string b)
        ) ballots in
        return (s, "application/json")

      let f_records user () =
        match user with
        | Some u ->
          if metadata.e_owner = Some u.user_user then (
            (* TODO: streaming *)
            lwt ballots = W.B.Records.fold (fun u (d, _) xs ->
              let x = Printf.sprintf "%s %S\n"
                (Serializable_builtin_j.string_of_datetime d) u
              in return (x::xs)
            ) [] in
            let s = List.map (fun b () ->
              return (Ocsigen_stream.of_string b)
            ) ballots in
            return (s, "text/plain")
          ) else (
            forbidden ()
          )
        | _ -> forbidden ()

      let handle_pseudo_file u f =
        let open Eliom_registration in
        let file f =
          if_eligible S.get_logged_user can_read f u () >>=
          File.send ~content_type:"application/json"
        and stream f =
          if_eligible S.get_logged_user can_read f u () >>=
          Streamlist.send >>=
          (fun x -> return (cast_unknown_content_kind x))
        in
        match f with
        | ESRaw -> file f_raw
        | ESKeys -> file f_keys
        | ESCreds -> stream f_creds
        | ESBallots -> stream f_ballots
        | ESRecords -> stream f_records

      let () = Any.register
        ~service:W.S.election_dir
        (fun f () ->
          let module X = struct
            let s = Eliom_service.preapply W.S.election_dir f
          end in
          let x = (module X : SAVED_SERVICE) in
          Eliom_reference.set S.saved_service x >>
          handle_pseudo_file () f
        )

      let () = Html5.register
        ~service:W.S.election_update_credential
        (fun uuid () ->
          lwt user = S.get_logged_user () in
          match user with
          | Some u ->
            if metadata.e_owner = Some u.user_user then (
              T.update_credential ()
            ) else (
              forbidden ()
            )
          | _ -> forbidden ()
        )

      let () = String.register
        ~service:W.S.election_update_credential_post
        (fun uuid (old, new_) ->
          lwt user = S.get_logged_user () in
          match user with
          | Some u ->
            if metadata.e_owner = Some u.user_user then (
              try_lwt
                W.B.update_cred ~old ~new_ >>
                return ("OK", "text/plain")
              with Error e ->
                return ("Error: " ^ explain_error e, "text/plain")
            ) else (
              forbidden ()
            )
          | _ -> forbidden ()
        )

      let () = Redirection.register
        ~service:W.S.election_vote
        (if_eligible S.get_logged_user can_read
           (fun user () ->
             Eliom_reference.unset ballot >>
             let module X = struct let s = W.S.election_vote end in
             let x = (module X : SAVED_SERVICE) in
             Eliom_reference.set S.saved_service x >>
             return W.S.booth
           )
        )

      let do_cast uuid () =
        match_lwt Eliom_reference.get ballot with
        | Some the_ballot ->
          begin
            Eliom_reference.unset ballot >>
            match_lwt S.get_logged_user () with
            | Some u ->
              let b = check_acl metadata.e_voters u.user_user in
              if b then (
                let record =
                  Auth_common.string_of_user u.user_user,
                  (CalendarLib.Fcalendar.Precise.now (), None)
                in
                lwt result =
                  try_lwt
                    lwt hash = W.B.cast the_ballot record in
                    return (`Valid hash)
                  with Error e -> return (`Error e)
                in
                Eliom_reference.unset ballot >>
                T.cast_confirmed ~result ()
              ) else forbidden ()
            | None -> forbidden ()
          end
        | None -> fail_http 404

      let ballot_received uuid user =
        let confirm () =
          let service = Eliom_service.post_coservice
            ~csrf_safe:true
            ~csrf_scope:Eliom_common.default_session_scope
            ~fallback:W.S.election_cast
            ~post_params:Eliom_parameter.unit
            ()
          in
          let () = Html5.register
            ~service
            ~scope:Eliom_common.default_session_scope
            do_cast
          in service
        in
        let can_vote = can_vote metadata user in
        T.cast_confirmation ~confirm ~user ~can_vote ()

      let () = Html5.register
        ~service:W.S.election_cast
        (if_eligible S.get_logged_user can_read
           (fun user () ->
             let uuid = W.election.e_params.e_uuid in
             let module X = struct let s = W.S.election_cast end in
             let x = (module X : SAVED_SERVICE) in
             Eliom_reference.set S.saved_service x >>
             match_lwt Eliom_reference.get ballot with
             | Some _ -> ballot_received uuid user
             | None -> T.cast_raw ()
           )
        )

      let () = Redirection.register
        ~service:W.S.election_cast_post
        (if_eligible S.get_logged_user can_read
           (fun user (ballot_raw, ballot_file) ->
             lwt the_ballot = match ballot_raw, ballot_file with
               | Some ballot, None -> return ballot
               | None, Some fi ->
                 let fname = fi.Ocsigen_extensions.tmp_filename in
                 Lwt_stream.to_string (Lwt_io.chars_of_file fname)
               | _, _ -> fail_http 400
             in
             let module X : SAVED_SERVICE = struct
               let uuid = W.election.e_params.e_uuid
               let s = W.S.election_cast
             end in
             let x = (module X : SAVED_SERVICE) in
             Eliom_reference.set S.saved_service x >>
             Eliom_reference.set ballot (Some the_ballot) >>
             match user with
             | None -> return (Eliom_service.preapply S.login None)
             | Some u -> S.cont ()
           )
        )

    end

  end in

  (module R : REGISTRATION)
