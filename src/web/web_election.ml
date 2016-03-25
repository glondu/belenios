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

open Lwt
open Platform
open Serializable_j
open Signatures
open Common
open Web_serializable_j
open Web_signatures
open Web_common

let ( / ) = Filename.concat

module Make (D : ELECTION_DATA) (M : RANDOM with type 'a t = 'a Lwt.t) : WEB_ELECTION = struct

    let uuid = Uuidm.to_string D.election.e_params.e_uuid

    module G = D.G
    module E = Election.MakeElection (G) (M)

    module B : WEB_BALLOT_BOX = struct

      let uuid_u = underscorize uuid
      let ballots_table = Ocsipersist.open_table ("ballots_" ^ uuid_u)
      let records_table = Ocsipersist.open_table ("records_" ^ uuid_u)
      let cred_table = Ocsipersist.open_table ("creds_" ^ uuid_u)

      let inject_cred cred =
        try_lwt
          lwt _ = Ocsipersist.find cred_table cred in
          failwith "trying to add duplicate credential"
        with Not_found ->
          Ocsipersist.add cred_table cred None

      let send_confirmation_email user email hash =
        let title = D.election.e_params.e_name in
        let subject = "Your vote for election " ^ title in
        let x = (D.election.e_params.e_uuid, ()) in
        let url1 = Eliom_uri.make_string_uri ~absolute:true
          ~service:Web_services.election_pretty_ballots x |> rewrite_prefix
        in
        let url2 = Eliom_uri.make_string_uri ~absolute:true
          ~service:Web_services.election_home x |> rewrite_prefix
        in
        let body = Mail_templates.confirmation user title hash url1 url2 in
        send_email email subject body

      let do_cast rawballot (user, date) =
        let voters = Lwt_io.lines_of_file (!spool_dir / uuid / "voters.txt") in
        lwt voters = Lwt_stream.to_list voters in
        lwt email, login =
          let rec loop = function
            | x :: xs ->
               let email, login = split_identity x in
               if login = user.user_name then return (email, login) else loop xs
            | [] -> fail UnauthorizedVoter
          in loop voters
        in
        let user = string_of_user user in
        lwt state = Web_persist.get_election_state uuid in
        let voting_open = state = `Open in
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
            lwt x = Ocsipersist.find records_table user in
            Lwt.return (Some x)
          with Not_found -> Lwt.return None
        in
        match old_cred, old_record with
          | None, None ->
            (* first vote *)
            if E.check_ballot D.election ballot then (
              let hash = sha256_b64 rawballot in
              Ocsipersist.add cred_table credential (Some hash) >>
              Ocsipersist.add ballots_table hash rawballot >>
              Ocsipersist.add records_table user (date, credential) >>
              send_confirmation_email login email hash >>
              return hash
            ) else (
              fail ProofCheck
            )
          | Some h, Some (_, old_credential) ->
            (* revote *)
            if credential = old_credential then (
              if E.check_ballot D.election ballot then (
                Ocsipersist.remove ballots_table h >>
                let hash = sha256_b64 rawballot in
                Ocsipersist.add cred_table credential (Some hash) >>
                Ocsipersist.add ballots_table hash rawballot >>
                Ocsipersist.add records_table user (date, credential) >>
                send_confirmation_email login email hash >>
                return hash
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
        Lwt_io.(with_file ~mode:Output (!spool_dir / uuid / string_of_election_file f))

      let do_write_ballots () =
        do_write ESBallots (fun oc ->
          Ocsipersist.iter_step (fun _ x ->
            Lwt_io.write_line oc x
          ) ballots_table
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
          ) records_table
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

      let compute_encrypted_tally () =
        lwt num_tallied, tally =
          Ocsipersist.fold_step
            (fun _ rawballot (n, accu) ->
              let ballot = ballot_of_string G.read rawballot in
              let ciphertext = E.extract_ciphertext ballot in
              return (n + 1, E.combine_ciphertexts accu ciphertext))
            ballots_table (0, E.neutral_ciphertext D.election)
        in
        let tally = string_of_encrypted_tally G.write tally in
        Lwt_mutex.with_lock mutex (fun () ->
          do_write ESETally (fun oc ->
            Lwt_io.write oc tally
          )
        ) >>
        return (num_tallied, sha256_b64 tally, tally)

    end

end
