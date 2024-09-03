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

open Lwt
open Lwt.Syntax
open Belenios
open Belenios_server_core
open Web_common

module Make
    (X : Pages_sig.S)
    (Site_common : Site_common_sig.S)
    (Site_admin : Site_admin_sig.S) =
struct
  open X
  open Web_services
  open Site_common
  open Eliom_service
  open Eliom_registration

  let get_preferred_gettext () = Web_i18n.get_preferred_gettext "voter"

  (* Make sure this module is loaded after Site_admin *)
  let _ignored = Site_admin.data_policy_loop

  let () =
    Redirection.register ~service:election_home_dir (fun uuid () ->
        return (Redirection (preapply ~service:election_home (uuid, ()))))

  let () =
    Any.register ~service:election_home (fun (uuid, ()) () ->
        let@ s = Storage.with_transaction in
        let@ election = with_election s uuid in
        let* x = Eliom_reference.get Web_state.cast_confirmed in
        let* () = Web_state.discard () in
        match x with
        | Some result ->
            Pages_voter.cast_confirmed election ~result () >>= Html.send
        | None ->
            make_absolute_string_uri ~fragment:(Uuid.unwrap uuid) ~service:apps
              "election"
            |> String_redirection.send)

  let () =
    Any.register ~service:election_cast (fun uuid () ->
        let@ s = Storage.with_transaction in
        let@ election = with_election s uuid in
        Pages_voter.cast_raw election () >>= Html.send)

  let submit_ballot ballot =
    let ballot = Stdlib.String.trim ballot in
    let* () = Eliom_reference.set Web_state.ballot (Some ballot) in
    redir_preapply election_submit_ballot_check () ()

  let () =
    Any.register ~service:election_submit_ballot (fun () ballot ->
        submit_ballot ballot)

  let () =
    Any.register ~service:election_submit_ballot_file (fun () ballot ->
        let* ballot = exhaust_file ballot in
        submit_ballot ballot)

  let () =
    Any.register ~service:election_submit_ballot_check (fun () () ->
        let* l = get_preferred_gettext () in
        let open (val l) in
        let* ballot = Eliom_reference.get Web_state.ballot in
        match ballot with
        | None ->
            Pages_common.generic_page ~title:(s_ "Cookies are blocked")
              (s_ "Your browser seems to block cookies. Please enable them.")
              ()
            >>= Html.send
        | Some rawballot -> (
            match Election.election_uuid_of_string_ballot rawballot with
            | exception _ ->
                Pages_common.generic_page ~title:(s_ "Error")
                  (s_ "Ill-formed ballot") ()
                >>= Html.send
            | uuid ->
                let@ s = Storage.with_transaction in
                let@ precast_data cont =
                  Lwt.try_bind
                    (fun () -> Web_persist.precast_ballot s uuid ~rawballot)
                    (function
                      | Ok x -> cont x
                      | Error e ->
                          let msg =
                            Printf.sprintf
                              (f_ "Your ballot is rejected because %s.")
                              (explain_error l (CastError e))
                          in
                          Pages_common.generic_page ~title:(s_ "Error") msg ()
                          >>= Html.send)
                    (function
                      | Election_not_found _ -> (
                          let* election = Spool.get s uuid Spool.draft in
                          match election with
                          | Some _ -> redir_preapply election_draft uuid ()
                          | None ->
                              let msg = s_ "Unknown election" in
                              Pages_common.generic_page ~title:(s_ "Error") msg
                                ()
                              >>= Html.send)
                      | e -> Lwt.reraise e)
                in
                let* () =
                  Eliom_reference.set Web_state.precast_data (Some precast_data)
                in
                redir_preapply election_login ((uuid, ()), None) ()))

  let send_confirmation_email s uuid revote user recipient weight hash =
    let@ election =
      Public_archive.with_election s uuid ~fallback:(fun () ->
          Lwt.fail (Election_not_found (uuid, "send_confirmation_email")))
    in
    let open (val election) in
    let title = template.t_name in
    let* metadata = Web_persist.get_election_metadata s uuid in
    let x = (uuid, ()) in
    let url1 =
      make_absolute_string_uri ~service:Web_services.election_pretty_ballots x
    in
    let url2 = get_election_home_url uuid in
    let* l = get_preferred_gettext () in
    let open (val l) in
    let subject = Printf.sprintf (f_ "Your vote for election %s") title in
    let body =
      Mails_voter.mail_confirmation l user title weight hash revote url1 url2
        metadata.e_contact
    in
    Lwt.catch
      (fun () ->
        let* () =
          send_email (MailConfirmation uuid) ~recipient ~subject ~body
        in
        Lwt.return true)
      (fun _ -> Lwt.return false)

  let () =
    Any.register ~service:election_cast_confirm (fun uuid () ->
        let@ s = Storage.with_transaction in
        let@ election = with_election s uuid in
        let* ballot = Eliom_reference.get Web_state.ballot in
        let* precast_data = Eliom_reference.get Web_state.precast_data in
        match (ballot, precast_data) with
        | None, _ | _, None ->
            Pages_voter.lost_ballot s election () >>= Html.send
        | Some rawballot, Some precast_data -> (
            let* () = Eliom_reference.unset Web_state.ballot in
            let* user = Web_state.get_election_user uuid in
            match user with
            | None -> forbidden ()
            | Some user ->
                let* () = Eliom_reference.unset Web_state.election_user in
                let* result =
                  Lwt.catch
                    (fun () ->
                      let* hash =
                        Api_elections.cast_ballot send_confirmation_email s uuid
                          ~rawballot ~user ~precast_data
                      in
                      return (Ok hash))
                    (function
                      | BeleniosWebError e -> return (Error e) | e -> Lwt.fail e)
                in
                let* () =
                  Eliom_reference.set Web_state.cast_confirmed (Some result)
                in
                redir_preapply election_home (uuid, ()) ()))

  let () =
    Any.register ~service:election_pretty_ballots (fun (uuid, ()) () ->
        let@ s = Storage.with_transaction in
        let@ election = with_election s uuid in
        Pages_voter.pretty_ballots s election >>= Html.send)

  let () =
    Any.register ~service:election_pretty_ballot (fun ((uuid, ()), hash) () ->
        let@ s = Storage.with_transaction in
        let* ballot = Public_archive.get_ballot_by_hash s uuid hash in
        match ballot with
        | None -> fail_http `Not_found
        | Some b ->
            String.send (b, "application/json") >>= fun x ->
            return @@ cast_unknown_content_kind x)

  let content_type_of_file = function ESRecords | ESVoters -> "text/plain"

  let handle_pseudo_file uuid f site_user =
    let@ s = Storage.with_transaction in
    let module S = (val s) in
    let* confidential = match f with ESRecords | ESVoters -> return true in
    let* allowed =
      if confidential then
        let* metadata = Web_persist.get_election_metadata s uuid in
        match site_user with
        | Some (_, a, _) when Accounts.check a metadata.e_owners -> return_true
        | _ -> return_false
      else return_true
    in
    if allowed then
      let content_type = content_type_of_file f in
      let return_string = function
        | Some x ->
            let* x = String.send (x, content_type) in
            return @@ cast_unknown_content_kind x
        | None -> fail_http `Not_found
      in
      let ( !? ) x = x >>= return_string in
      match f with
      | ESVoters -> !?(S.get (Election (uuid, Voters)))
      | ESRecords -> !?(S.get (Election (uuid, Records)))
    else forbidden ()

  let () =
    Any.register ~service:election_dir (fun (uuid, f) () ->
        let* site_user = Eliom_reference.get Web_state.site_user in
        handle_pseudo_file uuid f site_user)
end
