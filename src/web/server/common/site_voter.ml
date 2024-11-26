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
    (Web_auth : Web_auth_sig.S)
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
        return
          (Redirection (preapply ~service:election_home_redirect (uuid, ()))))

  let () =
    Any.register ~service:election_home_redirect (fun (uuid, ()) () ->
        let* () = Web_state.discard () in
        make_absolute_string_uri ~fragment:(Uuid.unwrap uuid) ~service:apps
          "election"
        |> String_redirection.send)

  let () =
    Any.register ~service:election_login_done (fun (uuid, state) () ->
        let result = Web_auth.State.get_result ~state in
        match result with
        | Some result ->
            let page =
              Printf.ksprintf Pages_common.html_js_exec
                {|
                  belenios.init({root: "../"});
                  belenios.finalizeLogin(%s);
                |}
                (Belenios_api.Serializable_j.string_of_cast_result result)
            in
            Html.send page
        | None ->
            let page =
              make_absolute_string_uri ~fragment:(Uuid.unwrap uuid)
                ~service:apps "election"
            in
            String_redirection.send page)

  let send_confirmation_email s uuid confirmation =
    let@ election =
      Public_archive.with_election s uuid ~fallback:(fun () ->
          Lwt.fail (Election_not_found (uuid, "send_confirmation_email")))
    in
    let open (val election) in
    let title = template.t_name in
    let* metadata = Web_persist.get_election_metadata s uuid in
    let url2 = get_election_home_url uuid in
    let url1 = url2 ^ "/ballots" in
    let* l = get_preferred_gettext () in
    let open (val l) in
    let subject = Printf.sprintf (f_ "Your vote for election %s") title in
    let body =
      Mails_voter.mail_confirmation l election confirmation url1 url2
        metadata.e_contact
    in
    Lwt.catch
      (fun () ->
        let* () =
          send_email (MailConfirmation uuid) ~recipient:confirmation.recipient
            ~subject ~body
        in
        Lwt.return true)
      (fun _ -> Lwt.return false)

  let () =
    Any.register ~service:election_cast_confirm (fun state () ->
        let@ env cont =
          let x = Web_auth.State.get ~state in
          match x with Some x -> cont x | None -> fail_http `Forbidden
        in
        let uuid = env.uuid in
        let@ s = Storage.with_transaction in
        let@ election = with_election s uuid in
        match env.state with
        | None -> Pages_voter.lost_ballot s election () >>= Html.send
        | Some { ballot; precast_data; _ } -> (
            match env.user with
            | None -> forbidden ()
            | Some user ->
                let* result =
                  Lwt.catch
                    (fun () ->
                      let* hash =
                        Api_elections.cast_ballot send_confirmation_email s uuid
                          election ~ballot ~user ~precast_data
                      in
                      return (`Ok hash))
                    (function
                      | BeleniosWebError e -> return (`Error e)
                      | e -> Lwt.fail e)
                in
                let () = Web_auth.State.set_result ~state result in
                redir_preapply election_login_done (uuid, state) ()))
end
