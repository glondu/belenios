(**************************************************************************)
(*                                BELENIOS                                *)
(*                                                                        *)
(*  Copyright © 2012-2021 Inria                                           *)
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
open Belenios_platform
open Belenios_core
open Belenios
open Platform
open Serializable_builtin_t
open Serializable_j
open Common
open Web_serializable_builtin_t
open Web_serializable_j
open Web_common

module Make (X : Pages_sig.S) (Site_common : Site_common_sig.S) (Site_admin : Site_admin_sig.S) = struct

  open X
  open Web_services
  open Site_common

  module PString = String

  open Eliom_service
  open Eliom_registration

  let ( / ) = Filename.concat

  let get_preferred_gettext () = Web_i18n.get_preferred_gettext "voter"

  (* Make sure this module is loaded after Site_admin *)
  let _ignored = Site_admin.data_policy_loop

  let () =
    Redirection.register ~service:election_home_dir
      (fun uuid () ->
        return (Redirection (preapply ~service:election_home (uuid, ())))
      )

  let () =
    Any.register ~service:election_home
      (fun (uuid, ()) () ->
        let@ election = with_election uuid in
        let* x = Eliom_reference.get Web_state.cast_confirmed in
        let* () = Web_state.discard () in
        (match x with
         | Some result ->
            Pages_voter.cast_confirmed election ~result () >>= Html.send
         | None ->
            let* state = Web_persist.get_election_state uuid in
            Pages_voter.election_home election state () >>= Html.send
        )
      )

  let () =
    Any.register ~service:booth_v1
      (fun () () ->
        let* () = Eliom_reference.unset Web_state.ballot in
        Pages_voter.booth () >>= Html.send)

  let () =
    Any.register ~service:election_cast
      (fun uuid () ->
        let@ election = with_election uuid in
        Pages_voter.cast_raw election () >>= Html.send
      )

  let submit_ballot ballot =
    let ballot = PString.trim ballot in
    let* () = Eliom_reference.set Web_state.ballot (Some ballot) in
    redir_preapply election_submit_ballot_check () ()

  let () =
    Any.register ~service:election_submit_ballot
      (fun () ballot -> submit_ballot ballot)

  let () =
    Any.register ~service:election_submit_ballot_file
      (fun () ballot ->
        let* ballot =
          let fname = ballot.Ocsigen_extensions.tmp_filename in
          Lwt_stream.to_string (Lwt_io.chars_of_file fname)
        in
        submit_ballot ballot
      )

  let () =
    Any.register ~service:election_submit_ballot_check
      (fun () () ->
        let* ballot = Eliom_reference.get Web_state.ballot in
        match ballot with
        | None ->
           let* l = get_preferred_gettext () in
           let open (val l) in
           Pages_common.generic_page ~title:(s_ "Cookies are blocked") (s_ "Your browser seems to block cookies. Please enable them.") ()
           >>= Html.send
        | Some ballot ->
           match Election.election_uuid_of_string_ballot ballot with
           | exception _ ->
              Pages_common.generic_page ~title:"Error" "Ill-formed ballot" () >>= Html.send
           | uuid ->
              let* election = Web_persist.get_draft_election uuid in
              match election with
              | Some _ -> redir_preapply election_draft uuid ()
              | None -> redir_preapply election_login ((uuid, ()), None) ()
      )

  let send_confirmation_email uuid revote user recipient weight hash =
    let* election =
      let* election = find_election uuid in
      match election with
      | Some election -> return election
      | None ->
         let msg =
           Printf.sprintf "send_confirmation_email: %s not found"
             (raw_string_of_uuid uuid)
         in
         Lwt.fail (Failure msg)
    in
    let open (val election) in
    let title = election.e_name in
    let* metadata = Web_persist.get_election_metadata uuid in
    let x = (uuid, ()) in
    let url1 = Eliom_uri.make_string_uri ~absolute:true
                 ~service:Web_services.election_pretty_ballots x |> rewrite_prefix
    in
    let url2 = Eliom_uri.make_string_uri ~absolute:true
                 ~service:Web_services.election_home x |> rewrite_prefix
    in
    let* l = get_preferred_gettext () in
    let open (val l) in
    let subject = Printf.sprintf (f_ "Your vote for election %s") title in
    let body = Mails_voter.mail_confirmation l user title weight hash revote url1 url2 metadata.e_contact in
    Lwt.catch
      (fun () ->
        let* () = send_email (MailConfirmation uuid) ~recipient ~subject ~body in
        Lwt.return true)
      (fun _ -> Lwt.return false)

  let cast_ballot election ~rawballot ~user =
    let module W = (val election : Site_common_sig.ELECTION_LWT) in
    let uuid = W.election.e_uuid in
    let* voters = read_file ~uuid "voters.txt" in
    let voters = match voters with Some xs -> xs | None -> [] in
    let* email, login, weight =
      let rec loop = function
        | x :: xs ->
           let email, login, weight = split_identity x in
           if PString.lowercase_ascii login = PString.lowercase_ascii user.user_name then return (email, login, weight) else loop xs
        | [] -> fail UnauthorizedVoter
      in loop voters
    in
    let show_weight =
      List.exists
        (fun x ->
          let _, _, weight = split_identity_opt x in
          weight <> None
        ) voters
    in
    let oweight = if show_weight then Some weight else None in
    let user = string_of_user user in
    let* state = Web_persist.get_election_state uuid in
    let voting_open = state = `Open in
    let* () = if not voting_open then fail ElectionClosed else return_unit in
    let* r = Web_persist.cast_ballot election ~rawballot ~user ~weight (now ()) in
    match r with
    | Ok (hash, revote) ->
       let* success = send_confirmation_email uuid revote login email oweight hash in
       let () =
         if revote then
           Printf.ksprintf Ocsigen_messages.accesslog
             "Someone revoted in election %s" (raw_string_of_uuid uuid)
       in
       return (hash, weight, success)
    | Error e ->
       fail (CastError e)

  let () =
    Any.register ~service:election_cast_fallback
      (fun uuid () ->
        let@ election = with_election uuid in
        let* ballot = Eliom_reference.get Web_state.ballot in
        (match ballot with
         | Some b -> Pages_voter.cast_confirmation election (sha256_b64 b) () >>= Html.send
         | None -> Pages_voter.lost_ballot election () >>= Html.send
        )
      )

  let () =
    Any.register ~service:election_cast_confirm
      (fun uuid () ->
        let@ election = with_election uuid in
        let* ballot = Eliom_reference.get Web_state.ballot in
        match ballot with
        | None ->
           Pages_voter.lost_ballot election () >>= Html.send
        | Some rawballot ->
           let* () = Eliom_reference.unset Web_state.ballot in
           let* user = Web_state.get_election_user uuid in
           match user with
           | None -> forbidden ()
           | Some user ->
              let* () = Eliom_reference.unset Web_state.election_user in
              let* result =
                Lwt.catch
                  (fun () ->
                    let* hash = cast_ballot election ~rawballot ~user in
                    return (Ok hash)
                  )
                  (function
                   | BeleniosWebError e -> return (Error e)
                   | e -> Lwt.fail e
                  )
              in
              let* () = Eliom_reference.set Web_state.cast_confirmed (Some result) in
              redir_preapply election_home (uuid, ()) ()
      )

  let () =
    Any.register ~service:election_pretty_ballots
      (fun (uuid, ()) () ->
        let@ election = with_election uuid in
        Pages_voter.pretty_ballots election >>= Html.send
      )

  let () =
    Any.register ~service:election_pretty_ballot
      (fun ((uuid, ()), hash) () ->
        let* ballot = Web_persist.get_ballot_by_hash uuid hash in
        match ballot with
        | None -> fail_http `Not_found
        | Some b ->
           String.send (b, "application/json") >>=
             (fun x -> return @@ cast_unknown_content_kind x))

  let handle_method uuid question f =
    let* l = get_preferred_gettext () in
    let open (val l) in
    let@ election = with_election uuid in
    let open (val election) in
    let questions = election.e_questions in
    if 0 <= question && question < Array.length questions then (
      match questions.(question) with
      | Question.NonHomomorphic (q, extra) ->
         f l q extra
           (fun continuation ->
             let* result = Web_persist.get_election_result uuid in
             match result with
             | Some result ->
                let result = election_result_of_string G.read read_result result in
                (match (result.result :> raw_result).(question) with
                 | RNonHomomorphic ballots -> continuation ballots
                 | _ -> failwith "handle_method"
                )
             | None ->
                Pages_common.generic_page ~title:(s_ "Error")
                  (s_ "The result of this election is not available.") ()
                >>= Html.send ~code:404
           )
      | Question.Homomorphic _ ->
         Pages_common.generic_page ~title:(s_ "Error")
           (s_ "This question is homomorphic, this method cannot be applied to its result.")
           ()
         >>= Html.send ~code:403
    ) else (
      Pages_common.generic_page ~title:(s_ "Error")
        (s_ "Invalid index for question.") ()
      >>= Html.send ~code:404
    )

  let () =
    Any.register ~service:method_schulze
      (fun (uuid, question) () ->
        handle_method uuid question
          (fun _ q extra continuation ->
            continuation
              (fun ballots ->
                let nchoices = Array.length q.Question_nh_t.q_answers in
                let blank_allowed =
                  match Question.get_counting_method extra with
                  | `Schulze o -> o.schulze_extra_blank
                  | _ -> false
                in
                let schulze = Schulze.compute ~nchoices ~blank_allowed ballots in
                Pages_voter.schulze q schulze >>= Html.send
              )
          )
      )

  let () =
    Any.register ~service:method_mj
      (fun (uuid, (question, ngrades)) () ->
        handle_method uuid question
          (fun l q extra continuation ->
            let open (val l : Belenios_ui.I18n.GETTEXT) in
            match ngrades with
            | None ->
               Pages_voter.majority_judgment_select uuid question
               >>= Html.send
            | Some ngrades ->
               if ngrades > 0 then (
                 let blank_allowed =
                   match Question.get_counting_method extra with
                   | `MajorityJudgment o -> o.mj_extra_blank
                   | _ -> false
                 in
                 continuation
                   (fun ballots ->
                     let nchoices = Array.length q.Question_nh_t.q_answers in
                     let mj = Majority_judgment.compute ~nchoices ~ngrades ~blank_allowed ballots in
                     Pages_voter.majority_judgment q mj >>= Html.send
                   )
               ) else (
                 Pages_common.generic_page ~title:(s_ "Error")
                   (s_ "The number of grades is invalid.") ()
                 >>= Html.send ~code:400
               )
          )
      )

  let () =
    Any.register ~service:method_stv
      (fun (uuid, (question, nseats)) () ->
        handle_method uuid question
          (fun l q _ continuation ->
            let open (val l : Belenios_ui.I18n.GETTEXT) in
            match nseats with
            | None ->
               Pages_voter.stv_select uuid question
               >>= Html.send
            | Some nseats ->
               if nseats > 0 then (
                 continuation
                   (fun ballots ->
                     let stv = Stv.compute ~nseats ballots in
                     Pages_voter.stv q stv >>= Html.send
                   )
               ) else (
                 Pages_common.generic_page ~title:(s_ "Error")
                   (s_ "The number of seats is invalid.") ()
                 >>= Html.send ~code:400
               )
          )
      )

  let content_type_of_file = function
    | ESRaw -> "application/json; charset=utf-8"
    | ESTrustees | ESETally | ESResult -> "application/json"
    | ESBallots | ESShuffles -> "text/plain" (* should be "application/json-seq", but we don't use RS *)
    | ESCreds | ESRecords | ESVoters -> "text/plain"

  let handle_pseudo_file uuid f site_user =
    let* confidential =
      match f with
      | ESRaw | ESTrustees | ESBallots | ESETally | ESCreds | ESShuffles -> return false
      | ESRecords | ESVoters -> return true
      | ESResult ->
         let* hidden = Web_persist.get_election_result_hidden uuid in
         match hidden with
         | None -> return false
         | Some _ -> return true
    in
    let* allowed =
      if confidential then (
        let* metadata = Web_persist.get_election_metadata uuid in
        match site_user with
        | Some x when (match metadata.e_owner with None -> false | Some o -> Accounts.check x o) -> return_true
        | _ -> return_false
      ) else return_true
    in
    if allowed then (
      let content_type = content_type_of_file f in
      File.send ~content_type (!Web_config.spool_dir / raw_string_of_uuid uuid / string_of_election_file f)
    ) else forbidden ()

  let () =
    Any.register ~service:election_dir
      (fun (uuid, f) () ->
        let* site_user = Eliom_reference.get Web_state.site_user in
        handle_pseudo_file uuid f site_user)

end
