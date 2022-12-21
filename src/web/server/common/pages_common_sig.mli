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

module type S = sig

  val direct_a :
    ?target:string -> string -> string ->
    [> [> Html_types.txt ] Html_types.a ] Eliom_content.Html.elt

  val raw_a :
    service:('a, unit, Eliom_service.get, 'b, 'c, 'd, 'e,
             [< `WithSuffix | `WithoutSuffix ], 'f, unit, 'g)
      Eliom_service.t ->
    ?a:[< Html_types.a_attrib > `Href ] Eliom_content.Html.attrib list ->
    'h Eliom_content.Html.elt list ->
    'a -> [> 'h Html_types.a ] Eliom_content.Html.elt

  val make_a_with_hash :
    service:(unit, unit, Eliom_service.get, 'a, 'b, 'c, 'd,
             [< `WithSuffix | `WithoutSuffix ], 'e, unit, 'f)
      Eliom_service.t ->
    ?hash:string ->
    ?style:string -> string ->
    [> [> Html_types.txt ] Html_types.a ] Eliom_content.Html.elt

  val a_mailto :
    ?dest:string ->
    subject:string ->
    body:string -> string ->
    [> [> Html_types.txt ] Html_types.a ] Eliom_content.Html.elt

  val raw_textarea :
    ?rows:int -> ?cols:int ->
    string -> string -> [> `Textarea] Eliom_content.Html.elt

  val static : string -> Eliom_content.Xml.uri

  val read_snippet :
    ?default:([> Html_types.div_content_fun ] as 'a) Eliom_content.Html.elt ->
    lang:string -> string option -> 'a Eliom_content.Html.elt Lwt.t

  val base :
    title:string ->
    ?full_title:string ->
    ?login_box:[< Html_types.div_content_fun > `Div `PCDATA ]
      Eliom_content.Html.elt ->
    ?lang_box:[< Html_types.div_content_fun > `Div ]
      Eliom_content.Html.elt ->
    content:[< Html_types.div_content_fun ] Eliom_content.Html.elt
      list ->
    ?footer:[< Html_types.div_content_fun > `A `Div `PCDATA ]
      Eliom_content.Html.elt ->
    ?uuid:Web_serializable_t.uuid ->
    ?static:bool ->
    unit -> [> Html_types.html ] Eliom_content.Html.elt Lwt.t

  val lang_box :
    Web_common.site_cont -> [> Html_types.div ] Eliom_content.Html.elt Lwt.t

  val generic_page :
    title:string ->
    ?service:(unit, unit, Eliom_service.get, 'a, 'b, 'c, 'd,
              [< `WithSuffix | `WithoutSuffix ], 'e, unit,
              Eliom_service.non_ocaml)
      Eliom_service.t ->
    string -> unit -> [> `Html ] Eliom_content.Html.F.elt Lwt.t

  val login_title : [`Site | `Election] -> string -> string Lwt.t

  val login_choose :
    string list ->
    (string -> (unit, unit, Eliom_service.get, 'a, 'b, 'c, 'd,
                [< `WithSuffix | `WithoutSuffix ], 'e, unit,
                Eliom_service.non_ocaml)
                 Eliom_service.t) ->
    unit -> [> `Html ] Eliom_content.Html.F.elt Lwt.t

  val login_dummy : [`Site | `Election] -> [`Username | `Address] -> state:string -> [> Html_types.div ] Eliom_content.Html.F.elt Lwt.t
  val login_email : [`Site | `Election] -> [`Username | `Address] -> state:string -> [> Html_types.div ] Eliom_content.Html.F.elt Lwt.t
  val login_password : [`Site | `Election] -> [`Username | `Address] -> service:string -> allowsignups:bool -> state:string -> [> Html_types.div ] Eliom_content.Html.F.elt Lwt.t

  val login_failed :
    service:(unit, unit, Eliom_service.get, 'a, 'b, 'c, 'd,
             [< `WithSuffix | `WithoutSuffix ], 'e, unit,
             Eliom_service.non_ocaml)
      Eliom_service.t ->
    unit -> [> `Html ] Eliom_content.Html.F.elt Lwt.t

  val email_login : [`Site | `Election] -> [> `Html ] Eliom_content.Html.F.elt Lwt.t
  val email_email : address:string -> code:string -> (string * string) Lwt.t

  val signup_captcha_img : string -> [> Html_types.img ] Eliom_content.Html.elt
  val format_captcha_error :
    (module Belenios_ui.I18n.GETTEXT) -> Web_common.captcha_error option ->
    [> `Div | `PCDATA ] Eliom_content.Html.elt

  val login_email_captcha :
    state:string ->
    Web_common.captcha_error option ->
    string -> string ->
    [> Html_types.div ] Eliom_content.Html.elt Lwt.t

  val login_email_not_now :
    unit -> [> Html_types.div ] Eliom_content.Html.elt Lwt.t

  val authentication_impossible :
    unit -> [> Html_types.html ] Eliom_content.Html.F.elt Lwt.t

end
