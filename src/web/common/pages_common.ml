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

module type BASE = sig
  module Xml : Xml_sigs.NoWrap
  module Svg : Svg_sigs.Make(Xml).T
  module Html : Html_sigs.Make(Xml)(Svg).T

  val uris : Belenios_api.Serializable_t.configuration_uris
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

  let base_body l ~full_title ~content ~administer ?(login_box = txt "")
      ?(warning = txt "") ?(lang_box = txt "") ?(footer = txt "")
      ?(extra_footer = txt "") ?sticky_footer ?(restricted_mode = false) () =
    let open (val l : I18n.GETTEXT) in
    let restricted_mode =
      if restricted_mode then span [ txt @@ s_ "Restricted mode"; txt ". " ]
      else txt ""
    in
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
                  div ~a:[ a_class [ "page-header__right" ] ] [ login_box ];
                ];
              div
                ~a:[ a_class [ "page-body" ] ]
                [ warning; div ~a:[ a_id "main" ] [ lang_box; div content ] ];
              div
                ~a:[ a_class [ "page-footer" ] ]
                [
                  footer;
                  txt (s_ "Powered by ");
                  a ~a:[ a_href Uris.belenios ] [ txt "Belenios" ];
                  Belenios.Version.(
                    Printf.ksprintf txt " %s (%s). " version build);
                  restricted_mode;
                  a
                    ~a:[ a_href Uris.source_code ]
                    [ txt (s_ "Get the source code") ];
                  txt ". ";
                  a ~a:[ a_href Uris.tos ] [ txt (s_ "Terms of service") ];
                  txt ". ";
                  administer;
                  txt ".";
                  extra_footer;
                ];
            ];
        ];
      (match sticky_footer with
      | Some sticky_footer ->
          div ~a:[ a_class [ "sticky-footer" ] ] [ sticky_footer ]
      | None -> txt "");
    ]
end
