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

val direct_a :
  string -> string ->
  [> [> Html_types.txt ] Html_types.a ] Eliom_content.Html.elt

val make_button :
  service:(unit, unit, Eliom_service.get, 'a, 'b, 'c, 'd,
           [< `WithSuffix | `WithoutSuffix ], 'e, unit, 'f)
    Eliom_service.t ->
  ?hash:string ->
  ?style:string ->
  disabled:bool -> string -> [> `Button] Eliom_content.Html.elt

val make_a_with_hash :
  service:(unit, unit, Eliom_service.get, 'a, 'b, 'c, 'd,
           [< `WithSuffix | `WithoutSuffix ], 'e, unit, 'f)
    Eliom_service.t ->
  ?hash:string ->
  ?style:string -> string ->
  [> [> Html_types.txt ] Html_types.a ] Eliom_content.Html.elt

val a_mailto :
  dest:string ->
  subject:string ->
  body:string -> string ->
  [> [> Html_types.txt ] Html_types.a ] Eliom_content.Html.elt

val raw_textarea :
  ?rows:int -> ?cols:int ->
  string -> string -> [> `Textarea] Eliom_content.Html.elt

val static : string -> Eliom_content.Xml.uri

val format_user :
  site:bool ->
  Web_serializable_t.user ->
  [> Html_types.em ] Eliom_content.Html.elt

val base :
  title:string ->
  ?login_box:[< Html_types.div_content_fun > `Div `H1 ]
    Eliom_content.Html.elt ->
  ?lang_box:[< Html_types.div_content_fun > `Div ]
    Eliom_content.Html.elt ->
  content:[< Html_types.div_content_fun ] Eliom_content.Html.elt
    list ->
  ?footer:[< Html_types.div_content_fun > `A `Div `PCDATA ]
    Eliom_content.Html.elt ->
  ?uuid:Web_serializable_t.uuid ->
  unit -> [> Html_types.html ] Eliom_content.Html.elt Lwt.t

val responsive_base :
  title:string ->
  ?login_box:[< Html_types.div_content_fun > `Div `H1 ]
    Eliom_content.Html.elt ->
  ?lang_box:[< Html_types.div_content_fun > `Div ]
    Eliom_content.Html.elt ->
  content:[< Html_types.div_content_fun ] Eliom_content.Html.elt
    list ->
  ?footer:[< Html_types.div_content_fun > `A `Div `PCDATA ]
    Eliom_content.Html.elt ->
  ?uuid:Web_serializable_t.uuid ->
  unit -> [> Html_types.html ] Eliom_content.Html.elt Lwt.t

val lang_box :
  (module Web_i18n_sig.GETTEXT) -> Web_common.site_cont ->
  [> Html_types.div ] Eliom_content.Html.elt

val generic_page :
  title:string ->
  ?service:(unit, unit, Eliom_service.get, 'a, 'b, 'c, 'd,
            [< `WithSuffix | `WithoutSuffix ], 'e, unit,
            Eliom_service.non_ocaml)
    Eliom_service.t ->
  string -> unit -> [> `Html ] Eliom_content.Html.F.elt Lwt.t

val login_choose :
  string list ->
  (string -> (unit, unit, Eliom_service.get, 'a, 'b, 'c, 'd,
              [< `WithSuffix | `WithoutSuffix ], 'e, unit,
              Eliom_service.non_ocaml)
               Eliom_service.t) ->
  unit -> [> `Html ] Eliom_content.Html.F.elt Lwt.t

val login_dummy : state:string -> [> `Html ] Eliom_content.Html.F.elt Lwt.t
val login_password : service:string -> allowsignups:bool -> state:string -> [> `Html ] Eliom_content.Html.F.elt Lwt.t

val login_failed :
  service:(unit, unit, Eliom_service.get, 'a, 'b, 'c, 'd,
                    [< `WithSuffix | `WithoutSuffix ], 'e, unit,
                    Eliom_service.non_ocaml)
                   Eliom_service.t ->
  unit -> [> `Html ] Eliom_content.Html.F.elt Lwt.t
