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

open Lwt
open Platform
open Serializable_builtin_t
open Serializable_j
open Signatures
open Common
open Web_serializable_j
open Web_signatures
open Web_common

let ( / ) = Filename.concat

module Make (E : ELECTION with type 'a m = 'a Lwt.t) : WEB_BALLOT_BOX = struct

    let uuid = E.election.e_params.e_uuid

    module G = E.G

      let uuid_u = underscorize uuid
      let ballots_table = Ocsipersist.open_table ("ballots_" ^ uuid_u)
      let records_table = Ocsipersist.open_table ("records_" ^ uuid_u)
      let cred_table = Ocsipersist.open_table ("creds_" ^ uuid_u)

      let inject_cred cred =
        try%lwt
          let%lwt _ = Ocsipersist.find cred_table cred in
          failwith "trying to add duplicate credential"
        with Not_found ->
          Ocsipersist.add cred_table cred None

      let send_confirmation_email user email hash =
        let title = E.election.e_params.e_name in
        let uuid = E.election.e_params.e_uuid in
        let%lwt metadata = Web_persist.get_election_metadata uuid in
        let x = (uuid, ()) in
        let url1 = Eliom_uri.make_string_uri ~absolute:true
          ~service:Web_services.election_pretty_ballots x |> rewrite_prefix
        in
        let url2 = Eliom_uri.make_string_uri ~absolute:true
          ~service:Web_services.election_home x |> rewrite_prefix
        in
        let%lwt language = Eliom_reference.get Web_state.language in
        let module L = (val Web_i18n.get_lang language) in
        let subject = Printf.sprintf L.mail_confirmation_subject title in
        let contact = Web_templates.contact_footer metadata L.please_contact in
        let body = Printf.sprintf L.mail_confirmation user title hash url1 url2 contact in
        send_email email subject body

      let do_cast rawballot (user, date) =
        let%lwt voters = read_file ~uuid "voters.txt" in
        let%lwt email, login =
          let rec loop = function
            | x :: xs ->
               let email, login = split_identity x in
               if login = user.user_name then return (email, login) else loop xs
            | [] -> fail UnauthorizedVoter
          in loop (match voters with Some xs -> xs | None -> [])
        in
        let user = string_of_user user in
        let%lwt state = Web_persist.get_election_state uuid in
        let voting_open = state = `Open in
        if not voting_open then fail ElectionClosed else return () >>
        if String.contains rawballot '\n' then (
          fail (Serialization (Invalid_argument "multiline ballot"))
        ) else return () >>
        let%lwt ballot =
          try Lwt.return (ballot_of_string G.read rawballot)
          with e -> fail (Serialization e)
        in
        let%lwt credential =
          match ballot.signature with
            | Some s -> Lwt.return (G.to_string s.s_public_key)
            | None -> fail MissingCredential
        in
        let%lwt old_cred =
          try%lwt Ocsipersist.find cred_table credential
          with Not_found -> fail InvalidCredential
        and old_record =
          try%lwt
            let%lwt x = Ocsipersist.find records_table user in
            Lwt.return (Some x)
          with Not_found -> Lwt.return None
        in
        match old_cred, old_record with
          | None, None ->
            (* first vote *)
            let%lwt b = Lwt_preemptive.detach E.check_ballot ballot in
            if b then (
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
              let%lwt b = Lwt_preemptive.detach E.check_ballot ballot in
              if b then (
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
        match%lwt Ocsipersist.fold_step (fun k v x ->
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
        Lwt_io.(with_file ~mode:Output (!spool_dir / raw_string_of_uuid uuid / string_of_election_file f))

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
          let%lwt r = do_cast rawballot (user, date) in
          do_write_ballots () >>
          do_write_records () >>
          return r
        )

      let update_cred ~old ~new_ =
        Lwt_mutex.with_lock mutex (fun () ->
          let%lwt r = do_update_cred ~old ~new_ in
          do_write_creds () >> return r
        )

      let update_files () =
        Lwt_mutex.with_lock mutex (fun () ->
          do_write_ballots () >>
          do_write_records () >>
          do_write_creds ()
        )

      let compute_encrypted_tally () =
        let%lwt num_tallied, tally =
          Ocsipersist.fold_step
            (fun _ rawballot (n, accu) ->
              let ballot = ballot_of_string G.read rawballot in
              let ciphertext = E.extract_ciphertext ballot in
              return (n + 1, E.combine_ciphertexts accu ciphertext))
            ballots_table (0, E.neutral_ciphertext ())
        in
        let tally = string_of_encrypted_tally G.write tally in
        Lwt_mutex.with_lock mutex (fun () ->
          do_write ESETally (fun oc ->
            Lwt_io.write oc tally
          )
        ) >>
        return (num_tallied, sha256_b64 tally, tally)

end
