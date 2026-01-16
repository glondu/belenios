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

open Belenios

module type BASE = sig
  module Xml : Xml_sigs.NoWrap
  module Svg : Svg_sigs.Make(Xml).T
  module Html : Html_sigs.Make(Xml)(Svg).T

  val uris : Belenios_web_api.configuration_uris
end

module Make (Base : BASE) = struct
  open Base
  open Base.Html

  module Uris = struct
    let home = Xml.uri_of_string uris.home
    let logo = Xml.uri_of_string uris.logo
    let belenios = Xml.uri_of_string uris.belenios
    let source_code = Xml.uri_of_string uris.source_code
    let tos = Xml.uri_of_string uris.tos
  end

  let a_aria_label = Unsafe.string_attrib "aria-label"
  let a_aria_hidden = Unsafe.string_attrib "aria-hidden" "true"

  let footer_fragment l ?administer ?footer ?extra_footer ~restricted_mode () =
    let open (val l : I18n.GETTEXT) in
    let restricted_mode =
      if restricted_mode then span [ txt @@ s_ "Restricted mode"; txt ". " ]
      else txt ""
    in
    let administer =
      match administer with None -> [] | Some x -> [ txt " "; x; txt "." ]
    in
    let get = function None -> [] | Some x -> [ x ] in
    List.flatten
      [
        get footer;
        [
          txt (s_ "Powered by ");
          a ~a:[ a_href Uris.belenios ] [ txt "Belenios" ];
          Belenios.Version.(Printf.ksprintf txt " %s (%s). " version build);
          restricted_mode;
          a ~a:[ a_href Uris.source_code ] [ txt (s_ "Get the source code") ];
          txt ". ";
          a ~a:[ a_href Uris.tos ] [ txt (s_ "Terms of service") ];
          txt ".";
        ];
        administer;
        get extra_footer;
      ]

  let lang_box l =
    let open (val l : I18n.GETTEXT) in
    let lang = if lang = Language.devel then Language.default else lang in
    let langs =
      List.map
        (fun (l, x) ->
          let selected = if l = lang then [ a_selected () ] else [] in
          option ~a:(a_value (Language.unwrap l) :: selected) (txt x))
        Language.available
    in
    let form =
      div
        ~a:[ a_id "lang_form" ]
        [
          txt @@ s_ "Language:"; txt " "; select ~a:[ a_id "lang_select" ] langs;
        ]
    in
    Lwt.return
    @@ div
         ~a:[ a_class [ "lang_box" ] ]
         [
           form;
           div
             ~a:[ a_class [ "contribute-to-translations" ] ]
             [
               txt "(";
               a
                 ~a:[ a_href (Xml.uri_of_string Links.translation) ]
                 [ txt @@ s_ "Wish to help with translations?" ];
               txt ")";
             ];
         ]

  let base_body l ~full_title ~content ?administer ?(login_box = txt "")
      ?(warning = txt "") ?(lang_box = txt "") ?(footer = txt "")
      ?(extra_footer = txt "") ?sticky_footer ?(restricted_mode = false) () =
    let open (val l : I18n.GETTEXT) in
    [
      div
        ~a:[ a_id "vote-app" ]
        [
          div
            ~a:[ a_class [ "page" ] ]
            [
              div
                ~a:[ a_id "header"; a_class [ "page-header" ] ]
                [
                  div
                    ~a:[ a_class [ "page-header__logo" ] ]
                    [
                      a
                        ~a:
                          [
                            a_href Uris.home;
                            a_aria_label (s_ "Election server");
                          ]
                        [
                          img
                            ~a:
                              [
                                a_class [ "page-header__logo__image" ];
                                a_aria_hidden;
                              ]
                            ~alt:(s_ "Election server") ~src:Uris.logo ();
                        ];
                    ];
                  div
                    ~a:[ a_class [ "page-header__titles" ] ]
                    [
                      h1
                        ~a:
                          [
                            a_class [ "page-header__titles__election-name" ];
                            a_id "election_name";
                          ]
                        [ full_title ];
                      p
                        ~a:
                          [
                            a_class
                              [ "page-header__titles__election-description" ];
                            a_id "election_description";
                          ]
                        [ txt "" ];
                      (* no description provided? *)
                    ];
                  div
                    ~a:
                      [ a_class [ "page-header__right" ]; a_id "election_logo" ]
                    [];
                ];
              warning;
              login_box;
              div
                ~a:[ a_class [ "page-body" ] ]
                [
                  div
                    ~a:[ a_id "main" ]
                    [ lang_box; div content; div ~a:[ a_class [ "clear" ] ] [] ];
                ];
              div
                ~a:[ a_class [ "page-footer" ] ]
                (footer_fragment l ?administer ~footer ~extra_footer
                   ~restricted_mode ());
            ];
        ];
      (match sticky_footer with
      | Some sticky_footer ->
          div ~a:[ a_class [ "sticky-footer" ] ] [ sticky_footer ]
      | None -> txt "");
    ]

  let markup =
    let module M = Markup_light.Make (Base) in
    M.markup

  let confirmation_fragment l ~snippet ~progress election result =
    let open (val l : I18n.GETTEXT) in
    let open (val election : Belenios.Election.ELECTION) in
    let name = template.t_name in
    let result, step_title =
      match result with
      | `Ok
          ({ recipient; name; hash; revote; weight; email } :
            Belenios_messages.confirmation) ->
          let this_is_a_revote =
            if revote then
              span
                [ txt @@ s_ "This vote replaces any previous vote."; txt " " ]
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
              Printf.sprintf "%selection#%s/ballots" uris.home
                (Uuid.unwrap uuid)
            in
            a ~a:[ a_href @@ Xml.uri_of_string href ] [ txt (s_ "ballot box") ]
          in
          ( [
              txt (s_ " as user ");
              em [ txt @@ Option.value ~default:recipient.name name ];
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
            s_ "Thank you for voting!" )
      | `Error e ->
          ( [
              txt (s_ " is rejected, because ");
              txt (Confirmation.explain_cast_error l e);
              txt ".";
            ],
            s_ "FAIL!" )
    in
    [
      progress;
      div ~a:[ a_class [ "current_step" ] ] [ txt step_title ];
      div ([ txt (s_ "Your ballot for "); em [ markup name ] ] @ result);
      snippet;
      div
        [
          a
            ~a:
              [
                a_href @@ Xml.uri_of_string
                @@ Printf.sprintf "%selection#%s" uris.home (Uuid.unwrap uuid);
              ]
            [ txt (s_ "Go back to election") ];
        ];
    ]
end
