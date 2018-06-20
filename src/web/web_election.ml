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
open Serializable_j
open Signatures
open Common
open Web_serializable_j
open Web_signatures
open Web_common

module Make (E : ELECTION with type 'a m = 'a Lwt.t) : WEB_BALLOT_BOX = struct

    let uuid = E.election.e_params.e_uuid

    module G = E.G

      let send_confirmation_email revote user email hash =
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
        let revote = if revote then L.this_vote_replaces else "" in
        let body = Printf.sprintf L.mail_confirmation user title hash revote url1 url2 contact in
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
          try%lwt Web_persist.find_credential_mapping uuid credential
          with Not_found -> fail InvalidCredential
        and old_record =
          Web_persist.find_extended_record uuid user
        in
        match old_cred, old_record with
          | None, None ->
            (* first vote *)
            let%lwt b = Lwt_preemptive.detach E.check_ballot ballot in
            if b then (
              let%lwt hash = Web_persist.add_ballot uuid rawballot in
              Web_persist.add_credential_mapping uuid credential (Some hash) >>
              Web_persist.add_extended_record uuid user (date, credential) >>
              send_confirmation_email false login email hash >>
              return hash
            ) else (
              fail ProofCheck
            )
          | Some h, Some (_, old_credential) ->
            (* revote *)
            if credential = old_credential then (
              let%lwt b = Lwt_preemptive.detach E.check_ballot ballot in
              if b then (
                let%lwt hash = Web_persist.replace_ballot uuid h rawballot in
                Web_persist.add_credential_mapping uuid credential (Some hash) >>
                Web_persist.add_extended_record uuid user (date, credential) >>
                send_confirmation_email true login email hash >>
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

      let mutex = Lwt_mutex.create ()

      let cast rawballot (user, date) =
        Lwt_mutex.with_lock mutex (fun () ->
          do_cast rawballot (user, date)
        )

end
