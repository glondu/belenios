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

module type S = sig
  val direct_a :
    ?target:string ->
    string ->
    string ->
    [> [> Html_types.txt ] Html_types.a ] Eliom.Content.Html.elt

  val raw_a :
    service:
      ( 'a,
        unit,
        Eliom.Service.get,
        'b,
        'c,
        'd,
        'e,
        [< `WithSuffix | `WithoutSuffix ],
        'f,
        unit,
        'g )
      Eliom.Service.t ->
    ?a:[< Html_types.a_attrib > `Href ] Eliom.Content.Html.attrib list ->
    'h Eliom.Content.Html.elt list ->
    'a ->
    [> 'h Html_types.a ] Eliom.Content.Html.elt

  val api_a :
    (uuid -> _ Belenios_web_api.Endpoints.t) ->
    uuid ->
    'a Eliom.Content.Html.elt list ->
    [> 'a Html_types.a ] Eliom.Content.Html.elt

  val make_a_with_hash :
    service:
      ( unit,
        unit,
        Eliom.Service.get,
        'a,
        'b,
        'c,
        'd,
        [< `WithSuffix | `WithoutSuffix ],
        'e,
        unit,
        'f )
      Eliom.Service.t ->
    ?hash:string ->
    string ->
    [> [> Html_types.txt ] Html_types.a ] Eliom.Content.Html.elt

  val a_mailto :
    ?dest:string ->
    subject:string ->
    body:string ->
    string ->
    [> [> Html_types.txt ] Html_types.a ] Eliom.Content.Html.elt

  val raw_textarea :
    ?rows:int ->
    ?cols:int ->
    string ->
    string ->
    [> `Textarea ] Eliom.Content.Html.elt

  val static : string list -> Eliom.Content.Xml.uri

  val read_snippet :
    ?default:([> Html_types.div_content_fun ] as 'a) Eliom.Content.Html.elt ->
    lang:lang ->
    string option ->
    'a Eliom.Content.Html.elt Lwt.t

  val base :
    title:string ->
    ?full_title:string ->
    ?login_box:Html_types.div_content_fun Eliom.Content.Html.elt ->
    ?lang_box:Html_types.div_content_fun Eliom.Content.Html.elt ->
    content:[< Html_types.div_content_fun ] Eliom.Content.Html.elt list ->
    ?footer:Html_types.div_content_fun Eliom.Content.Html.elt ->
    ?sticky_footer:Html_types.div_content_fun Eliom.Content.Html.elt ->
    ?uuid:uuid ->
    ?static:bool ->
    ?redirect:string ->
    unit ->
    [> Html_types.html ] Eliom.Content.Html.elt Lwt.t

  val confirmation_fragment :
    (module Belenios_ui.I18n.GETTEXT) ->
    snippet:'a Eliom.Content.Html.elt ->
    progress:'a Eliom.Content.Html.elt ->
    Belenios.Election.t ->
    Belenios_web_api.cast_result ->
    ([> Html_types.div_content_fun ] as 'a) Eliom.Content.Html.elt list

  val lang_box :
    Web_common.site_cont_path ->
    [> Html_types.div ] Eliom.Content.Html.elt Lwt.t

  val generic_page :
    title:string ->
    ?uri:Eliom.Content.Xml.uri ->
    ?service:
      ( unit,
        unit,
        Eliom.Service.get,
        'a,
        'b,
        'c,
        'd,
        [< `WithSuffix | `WithoutSuffix ],
        'e,
        unit,
        Eliom.Service.non_ocaml )
      Eliom.Service.t ->
    string ->
    unit ->
    [> `Html ] Eliom.Content.Html.F.elt Lwt.t

  val login_title : [ `Site | `Election ] -> string -> string Lwt.t

  val login_choose :
    string list ->
    (string ->
    ( unit,
      unit,
      Eliom.Service.get,
      'a,
      'b,
      'c,
      'd,
      [< `WithSuffix | `WithoutSuffix ],
      'e,
      unit,
      Eliom.Service.non_ocaml )
    Eliom.Service.t) ->
    unit ->
    [> `Html ] Eliom.Content.Html.F.elt Lwt.t

  val login_dummy :
    [ `Site | `Election ] ->
    [ `Username | `Address ] ->
    state:string ->
    [> Html_types.div ] Eliom.Content.Html.F.elt Lwt.t

  val login_email :
    [ `Site | `Election ] ->
    [ `Username | `Address ] ->
    state:string ->
    [> Html_types.div ] Eliom.Content.Html.F.elt Lwt.t

  val login_password :
    [ `Site | `Election ] ->
    [ `Username | `Address ] ->
    service:string ->
    allowsignups:bool ->
    state:string ->
    [> Html_types.div ] Eliom.Content.Html.F.elt Lwt.t

  val login_failed :
    service:
      ( unit,
        unit,
        Eliom.Service.get,
        'a,
        'b,
        'c,
        'd,
        [< `WithSuffix | `WithoutSuffix ],
        'e,
        unit,
        Eliom.Service.non_ocaml )
      Eliom.Service.t ->
    unit ->
    [> `Html ] Eliom.Content.Html.F.elt Lwt.t

  val email_login :
    ?lang:lang ->
    ?address:(string, unit) result ->
    ?code:string ->
    state:string ->
    [ `Site | `Election ] ->
    [> `Html ] Eliom.Content.Html.F.elt Lwt.t

  val signup_captcha_img : string -> [> Html_types.img ] Eliom.Content.Html.elt

  val format_captcha_error :
    (module Belenios_ui.I18n.GETTEXT) ->
    Web_common.captcha_error option ->
    [> `Div | `PCDATA ] Eliom.Content.Html.elt

  val login_email_captcha :
    state:string ->
    Web_common.captcha_error option ->
    string ->
    string ->
    [> Html_types.div ] Eliom.Content.Html.elt Lwt.t

  val login_email_not_now :
    unit -> [> Html_types.div ] Eliom.Content.Html.elt Lwt.t

  val authentication_impossible :
    unit -> [> Html_types.html ] Eliom.Content.Html.F.elt Lwt.t

  val html_redirection :
    string -> [> Html_types.html ] Eliom.Content.Html.F.elt Lwt.t

  val html_js_exec : string -> [> Html_types.html ] Eliom.Content.Html.F.elt
end
