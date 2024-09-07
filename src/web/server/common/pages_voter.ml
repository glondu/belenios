(**************************************************************************)
(*                                BELENIOS                                *)
(*                                                                        *)
(*  Copyright © 2012-2023 Inria                                           *)
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

open Lwt.Syntax
open Belenios
open Belenios_server_core
open Web_common
open Eliom_content.Html.F

module Make
    (Web_state : Web_state_sig.S)
    (Web_i18n : Web_i18n_sig.S)
    (Web_services : Web_services_sig.S)
    (Pages_common : Pages_common_sig.S) =
struct
  open Web_services
  open Pages_common

  let get_preferred_gettext () = Web_i18n.get_preferred_gettext "voter"

  let progress_responsive_step5 l =
    let open (val l : Belenios_ui.I18n.GETTEXT) in
    div
      ~a:[ a_class [ "progress" ]; a_style "padding-top: 0;" ]
      [
        div ~a:[ a_class [ "progress__step-separator" ] ] [];
        div
          ~a:[ a_class [ "progress__step"; "progress__step--done" ] ]
          [
            div
              ~a:[ a_class [ "progress__step__dot-container"; "line-right" ] ]
              [ div ~a:[ a_class [ "progress__step__dot" ] ] [ txt "" ] ];
            span
              ~a:[ a_class [ "progress__step__title" ] ]
              [ txt (s_ "Input credential") ];
            span
              ~a:
                [
                  a_class [ "progress__step__short-title" ];
                  a_title (s_ "Input credential");
                ]
              [ txt (s_ "Step 1") ];
          ];
        div ~a:[ a_class [ "progress__step-separator" ] ] [];
        div
          ~a:[ a_class [ "progress__step"; "progress__step--done" ] ]
          [
            div
              ~a:
                [
                  a_class
                    [
                      "progress__step__dot-container"; "line-left"; "line-right";
                    ];
                ]
              [ div ~a:[ a_class [ "progress__step__dot" ] ] [ txt "" ] ];
            span
              ~a:[ a_class [ "progress__step__title" ] ]
              [ txt (s_ "Answer to questions") ];
            span
              ~a:
                [
                  a_class [ "progress__step__short-title" ];
                  a_title (s_ "Answer to questions");
                ]
              [ txt (s_ "Step 2") ];
          ];
        div ~a:[ a_class [ "progress__step-separator" ] ] [];
        div
          ~a:[ a_class [ "progress__step"; "progress__step--done" ] ]
          [
            div
              ~a:
                [
                  a_class
                    [
                      "progress__step__dot-container"; "line-left"; "line-right";
                    ];
                ]
              [ div ~a:[ a_class [ "progress__step__dot" ] ] [ txt "" ] ];
            span
              ~a:[ a_class [ "progress__step__title" ] ]
              [ txt (s_ "Review and encrypt") ];
            span
              ~a:
                [
                  a_class [ "progress__step__short-title" ];
                  a_title (s_ "Review and encrypt");
                ]
              [ txt (s_ "Step 3") ];
          ];
        div ~a:[ a_class [ "progress__step-separator" ] ] [];
        div
          ~a:[ a_class [ "progress__step"; "progress__step--done" ] ]
          [
            div
              ~a:
                [
                  a_class
                    [
                      "progress__step__dot-container"; "line-left"; "line-right";
                    ];
                ]
              [ div ~a:[ a_class [ "progress__step__dot" ] ] [ txt "" ] ];
            span
              ~a:[ a_class [ "progress__step__title" ] ]
              [ txt (s_ "Authenticate") ];
            span
              ~a:
                [
                  a_class [ "progress__step__short-title" ];
                  a_title (s_ "Authenticate");
                ]
              [ txt (s_ "Step 4") ];
          ];
        div ~a:[ a_class [ "progress__step-separator" ] ] [];
        div
          ~a:[ a_class [ "progress__step"; "progress__step--current" ] ]
          [
            div
              ~a:[ a_class [ "progress__step__dot-container"; "line-left" ] ]
              [ div ~a:[ a_class [ "progress__step__dot" ] ] [ txt "" ] ];
            span
              ~a:[ a_class [ "progress__step__title" ] ]
              [ txt (s_ "Confirm") ];
            span
              ~a:
                [
                  a_class [ "progress__step__short-title" ];
                  a_title (s_ "Confirm");
                ]
              [ txt (s_ "Step 5") ];
          ];
        div ~a:[ a_class [ "progress__step-separator" ] ] [];
      ]

  let lost_ballot s election () =
    let* l = get_preferred_gettext () in
    let open (val l) in
    let open (val election : Site_common_sig.ELECTION) in
    let title = template.t_name in
    let full_title = title in
    let* metadata = Web_persist.get_election_metadata s uuid in
    let you_must_restart =
      match get_booth_index metadata.e_booth_version with
      | Some i ->
          let (Booth service) = fst Web_services.booths.(i) in
          let hash =
            Netencoding.Url.mk_url_encoded_parameters
              [ ("uuid", Uuid.unwrap uuid) ]
          in
          div
            [
              txt (s_ "If you want to vote, you must ");
              make_a_with_hash ~service:(service ()) ~hash
                (s_ "start from the beginning");
              txt ".";
            ]
      | None -> txt ""
    in
    let content =
      [
        div
          [
            b [ txt (s_ "Warning:") ];
            txt " ";
            txt (s_ "Your vote was not recorded!");
          ];
        you_must_restart;
        div
          [
            a ~service:Web_services.election_home
              [ txt (s_ "Go back to election") ]
              (uuid, ());
          ];
      ]
    in
    base ~full_title ~title ~content ~uuid ()

  let cast_confirmed election ~result () =
    let* l = get_preferred_gettext () in
    let open (val l) in
    let open (val election : Site_common_sig.ELECTION) in
    let name = template.t_name in
    let result, snippet, step_title =
      match result with
      | Ok
          ({ user; hash; revote; weight; email; _ } :
            Belenios_api.Serializable_t.confirmation) ->
          let this_is_a_revote =
            if revote then span [ txt @@ s_ "This is a revote."; txt " " ]
            else txt ""
          in
          let your_weight_is =
            match weight with
            | Some weight ->
                span
                  [
                    txt
                      (Printf.sprintf (f_ "Your weight is %s.")
                         (Weight.to_string weight));
                    txt " ";
                  ]
            | None -> txt ""
          in
          let ballot_box =
            let href =
              Xml.uri_of_string (get_election_home_url uuid ^ "/ballots")
            in
            Eliom_content.Html.F.Raw.a
              ~a:[ a_href href ]
              [ txt (s_ "ballot box") ]
          in
          ( [
              txt (s_ " as user ");
              em [ txt user ];
              txt (s_ " has been accepted.");
              txt " ";
              this_is_a_revote;
              your_weight_is;
              txt (s_ "Your smart ballot tracker is ");
              b ~a:[ a_id "ballot_tracker" ] [ txt @@ Hash.to_b64 hash ];
              txt ". ";
              txt (s_ "You can check its presence in the ");
              ballot_box;
              txt (s_ " anytime during the election.");
              txt
                (if email then s_ " A confirmation e-mail has been sent to you."
                 else "");
            ],
            read_snippet ~lang !Web_config.success_snippet,
            s_ "Thank you for voting!" )
      | Error e ->
          ( [
              txt (s_ " is rejected, because ");
              txt (Web_common.explain_error l e);
              txt ".";
            ],
            Lwt.return (txt ""),
            s_ "FAIL!" )
    in
    let* snippet = snippet in
    let content =
      [
        progress_responsive_step5 l;
        div ~a:[ a_class [ "current_step" ] ] [ txt step_title ];
        p ([ txt (s_ "Your ballot for "); em [ markup name ] ] @ result);
        snippet;
        p
          [
            a ~service:Web_services.election_home
              [ txt (s_ "Go back to election") ]
              (uuid, ());
          ];
      ]
    in
    let title = name in
    let full_title = name in
    base ~full_title ~title ~content ~uuid ()
end
