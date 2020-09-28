(**************************************************************************)
(*                                BELENIOS                                *)
(*                                                                        *)
(*  Copyright © 2012-2020 Inria                                           *)
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
open Belenios_platform
open Belenios
open Platform
open Serializable_builtin_t
open Serializable_j
open Signatures
open Common
open Web_serializable_builtin_t
open Web_serializable_j
open Web_common
open Web_services
open Site_common

module PString = String

open Eliom_service
open Eliom_registration

let ( / ) = Filename.concat

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
       match%lwt find_election uuid with
       | None -> election_not_found ()
       | Some w ->
         let%lwt () = Eliom_reference.unset Web_state.ballot in
         (match%lwt Eliom_reference.get Web_state.cast_confirmed with
          | Some result ->
             let%lwt () = Eliom_reference.unset Web_state.cast_confirmed in
             Pages_voter.cast_confirmed w ~result () >>= Html.send
          | None ->
             let%lwt state = Web_persist.get_election_state uuid in
             Pages_voter.election_home w state () >>= Html.send
         )
    )

let () =
  Any.register ~service:election_vote
    (fun () () ->
      let%lwt () = Eliom_reference.unset Web_state.ballot in
      Pages_voter.booth () >>= Html.send)

let () =
  Any.register ~service:election_cast
    (fun uuid () ->
      match%lwt find_election uuid with
      | Some w -> Pages_voter.cast_raw w () >>= Html.send
      | None -> election_not_found ()
    )

let submit_ballot ballot =
  let ballot = PString.trim ballot in
  let%lwt () = Eliom_reference.set Web_state.ballot (Some ballot) in
  redir_preapply election_submit_ballot_check () ()

let () =
  Any.register ~service:election_submit_ballot
    (fun () ballot -> submit_ballot ballot)

let () =
  Any.register ~service:election_submit_ballot_file
    (fun () ballot ->
      let%lwt ballot =
        let fname = ballot.Ocsigen_extensions.tmp_filename in
        Lwt_stream.to_string (Lwt_io.chars_of_file fname)
      in
      submit_ballot ballot
    )

let () =
  Any.register ~service:election_submit_ballot_check
    (fun () () ->
      match%lwt Eliom_reference.get Web_state.ballot with
      | None ->
         let%lwt l = Web_i18n.get_preferred_gettext () in
         let open (val l) in
         Pages_common.generic_page ~title:(s_ "Cookies are blocked") (s_ "Your browser seems to block cookies. Please enable them.") ()
         >>= Html.send
      | Some ballot ->
         match ballot_of_string Yojson.Safe.read_json ballot with
         | exception _ ->
            Pages_common.generic_page ~title:"Error" "Ill-formed ballot" () >>= Html.send
         | ballot ->
            let uuid = ballot.election_uuid in
            match%lwt Web_persist.get_draft_election uuid with
            | Some _ -> redir_preapply election_draft uuid ()
            | None -> redir_preapply election_login ((uuid, ()), None) ()
    )

let send_confirmation_email uuid revote user email hash =
  let%lwt election =
    match%lwt find_election uuid with
    | Some election -> return election
    | None ->
       let msg =
         Printf.sprintf "send_confirmation_email: %s not found"
           (raw_string_of_uuid uuid)
       in
       Lwt.fail (Failure msg)
  in
  let title = election.e_params.e_name in
  let%lwt metadata = Web_persist.get_election_metadata uuid in
  let x = (uuid, ()) in
  let url1 = Eliom_uri.make_string_uri ~absolute:true
    ~service:Web_services.election_pretty_ballots x |> rewrite_prefix
  in
  let url2 = Eliom_uri.make_string_uri ~absolute:true
    ~service:Web_services.election_home x |> rewrite_prefix
  in
  let%lwt l = Web_i18n.get_preferred_gettext () in
  let open (val l) in
  let subject = Printf.sprintf (f_ "Your vote for election %s") title in
  let body = Pages_voter.mail_confirmation l user title hash revote url1 url2 metadata in
  send_email email subject body

let cast_ballot uuid ~rawballot ~user =
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
  let%lwt () = if not voting_open then fail ElectionClosed else return_unit in
  match%lwt Web_persist.cast_ballot uuid ~rawballot ~user (now ()) with
  | Ok (hash, revote) ->
     let%lwt () = send_confirmation_email uuid revote login email hash in
     return hash
  | Error e ->
     let msg = match e with
       | ECastWrongCredential -> Some "attempted to revote with already used credential"
       | ECastRevoteNotAllowed -> Some "attempted to revote using a new credential"
       | ECastReusedCredential -> Some "attempted to vote with already used credential"
       | _ -> None
     in
     let%lwt () = match msg with
       | Some msg -> security_log (fun () -> user ^ " " ^ msg)
       | None -> return_unit
     in
     fail (CastError e)

let () =
  Any.register ~service:election_cast_fallback
    (fun uuid () ->
      match%lwt find_election uuid with
      | Some w ->
         (match%lwt Eliom_reference.get Web_state.ballot with
          | Some b -> Pages_voter.cast_confirmation w (sha256_b64 b) () >>= Html.send
          | None -> Pages_voter.lost_ballot w () >>= Html.send
         )
      | None -> election_not_found ()
    )

let () =
  Any.register ~service:election_cast_confirm
    (fun uuid () ->
      match%lwt Eliom_reference.get Web_state.ballot with
      | None ->
         (match%lwt find_election uuid with
          | Some w -> Pages_voter.lost_ballot w () >>= Html.send
          | None -> election_not_found ()
         )
      | Some rawballot ->
         let%lwt () = Eliom_reference.unset Web_state.ballot in
         match%lwt Web_state.get_election_user uuid with
         | None -> forbidden ()
         | Some user ->
            let%lwt () = Eliom_reference.unset Web_state.election_user in
            let%lwt result =
              match%lwt cast_ballot uuid ~rawballot ~user with
              | hash -> return (Ok hash)
              | exception BeleniosWebError e -> return (Error e)
            in
            let%lwt () = Eliom_reference.set Web_state.cast_confirmed (Some result) in
            redir_preapply election_home (uuid, ()) ()
    )

let () =
  Any.register ~service:election_pretty_ballots
    (fun (uuid, ()) () ->
      match%lwt find_election uuid with
      | Some w ->
         let%lwt ballots = Web_persist.get_ballot_hashes uuid in
         let%lwt result = Web_persist.get_election_result uuid in
         Pages_voter.pretty_ballots w ballots result () >>= Html.send
      | None -> election_not_found ()
    )

let () =
  Any.register ~service:election_pretty_ballot
    (fun ((uuid, ()), hash) () ->
      let%lwt ballot = Web_persist.get_ballot_by_hash uuid hash in
      match ballot with
      | None -> fail_http 404
      | Some b ->
         String.send (b, "application/json") >>=
           (fun x -> return @@ cast_unknown_content_kind x))

let () =
  Any.register ~service:method_schulze
    (fun (uuid, question) () ->
      let%lwt l = Web_i18n.get_preferred_gettext () in
      let open (val l) in
      match%lwt find_election uuid with
      | None -> election_not_found ()
      | Some election ->
         let questions = election.e_params.e_questions in
         if 0 <= question && question < Array.length questions then (
           match questions.(question) with
           | Question.NonHomomorphic q ->
              let nchoices = Array.length q.Question_nh_t.q_answers in
              (match%lwt Web_persist.get_election_result uuid with
               | Some result ->
                  let ballots =
                    (Shape.to_shape_array result.result).(question)
                    |> Shape.to_shape_array
                    |> Array.map Shape.to_array
                  in
                  let schulze = Schulze.compute ~nchoices ballots in
                  Pages_voter.schulze q schulze >>= Html.send
               | None ->
                  Pages_common.generic_page ~title:(s_ "Error")
                    (s_ "The result of this election is not available.")
                  () >>= Html.send ~code:404
              )
           | Question.Homomorphic _ ->
              Pages_common.generic_page ~title:(s_ "Error")
                (s_ "This question is homomorphic, the Schulze method cannot be applied to its result.")
                () >>= Html.send ~code:403
         ) else (
           Pages_common.generic_page ~title:(s_ "Error")
             (s_ "Invalid index for question.")
             () >>= Html.send ~code:404
         )
    )

let content_type_of_file = function
  | ESRaw -> "application/json; charset=utf-8"
  | ESTrustees | ESETally | ESResult -> "application/json"
  | ESBallots | ESShuffles -> "text/plain" (* should be "application/json-seq", but we don't use RS *)
  | ESCreds | ESRecords | ESVoters -> "text/plain"

let handle_pseudo_file uuid f site_user =
  let%lwt confidential =
    match f with
    | ESRaw | ESTrustees | ESBallots | ESETally | ESCreds | ESShuffles -> return false
    | ESRecords | ESVoters -> return true
    | ESResult ->
       match%lwt Web_persist.get_election_result_hidden uuid with
       | None -> return false
       | Some _ -> return true
  in
  let%lwt () =
    if confidential then (
      let%lwt metadata = Web_persist.get_election_metadata uuid in
      match site_user with
      | Some u when metadata.e_owner = Some u -> return ()
      | _ -> forbidden ()
    ) else return ()
  in
  let content_type = content_type_of_file f in
  File.send ~content_type (!Web_config.spool_dir / raw_string_of_uuid uuid / string_of_election_file f)

let () =
  Any.register ~service:election_dir
    (fun (uuid, f) () ->
     let%lwt site_user = Eliom_reference.get Web_state.site_user in
     handle_pseudo_file uuid f site_user)
