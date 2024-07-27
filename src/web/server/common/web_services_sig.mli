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

open Belenios_server_core

module type S = sig
  val set_rewrite_prefix : src:string -> dst:string -> unit

  val make_absolute_string_uri :
    ?fragment:string ->
    service:
      ( 'a,
        unit,
        Eliom_service.get,
        _,
        _,
        _,
        _,
        [< `WithSuffix | `WithoutSuffix ],
        _,
        unit,
        _ )
      Eliom_service.t ->
    'a ->
    string

  val uuid_and_token :
    ( uuid * string,
      [ `WithoutSuffix ],
      [ `One of uuid ] Eliom_parameter.param_name
      * [ `One of string ] Eliom_parameter.param_name )
    Eliom_service.params

  val banner :
    ( string,
      unit,
      Eliom_service.get,
      Eliom_service.att,
      Eliom_service.non_co,
      Eliom_service.non_ext,
      Eliom_service.reg,
      [ `WithoutSuffix ],
      [ `One of string ] Eliom_parameter.param_name,
      unit,
      Eliom_service.non_ocaml )
    Eliom_service.t

  val home :
    ( unit,
      unit,
      Eliom_service.get,
      Eliom_service.att,
      Eliom_service.non_co,
      Eliom_service.non_ext,
      Eliom_service.reg,
      [ `WithoutSuffix ],
      unit,
      unit,
      Eliom_service.non_ocaml )
    Eliom_service.t

  val apps :
    ( string,
      unit,
      Eliom_service.get,
      Eliom_service.att,
      Eliom_service.non_co,
      Eliom_service.non_ext,
      Eliom_service.reg,
      [ `WithSuffix ],
      [ `One of string ] Eliom_parameter.param_name,
      unit,
      Eliom_service.non_ocaml )
    Eliom_service.t

  val admin_basic :
    unit ->
    ( unit,
      unit,
      Eliom_service.get,
      Eliom_service.att,
      Eliom_service.non_co,
      Eliom_service.non_ext,
      Eliom_service.non_reg,
      [ `WithoutSuffix ],
      unit,
      unit,
      Eliom_service.non_ocaml )
    Eliom_service.t

  val admin_new :
    unit ->
    ( unit,
      unit,
      Eliom_service.get,
      Eliom_service.att,
      Eliom_service.non_co,
      Eliom_service.non_ext,
      Eliom_service.non_reg,
      [ `WithoutSuffix ],
      unit,
      unit,
      Eliom_service.non_ocaml )
    Eliom_service.t

  val make_admin_new_uri : uuid option -> Eliom_content.Xml.uri

  val make_trustee_link :
    uuid ->
    [< `Generate of string | `Decrypt of string | `Shuffle of string | `Check ] ->
    string

  val make_credauth_link : uuid -> [< `Generate of string ] -> string

  val privacy_notice_accept :
    ( unit,
      Web_common.privacy_cont,
      Eliom_service.post,
      Eliom_service.non_att,
      Eliom_service.co,
      Eliom_service.non_ext,
      Eliom_service.reg,
      [ `WithoutSuffix ],
      unit,
      [ `One of Web_common.privacy_cont ] Eliom_parameter.param_name,
      Eliom_service.non_ocaml )
    Eliom_service.t

  val site_login :
    ( string option * Web_common.site_cont,
      unit,
      Eliom_service.get,
      Eliom_service.att,
      Eliom_service.non_co,
      Eliom_service.non_ext,
      Eliom_service.reg,
      [ `WithoutSuffix ],
      [ `One of string ] Eliom_parameter.param_name
      * [ `One of Web_common.site_cont ] Eliom_parameter.param_name,
      unit,
      Eliom_service.non_ocaml )
    Eliom_service.t

  val logout :
    ( Web_common.site_cont,
      unit,
      Eliom_service.get,
      Eliom_service.att,
      Eliom_service.non_co,
      Eliom_service.non_ext,
      Eliom_service.reg,
      [ `WithoutSuffix ],
      [ `One of Web_common.site_cont ] Eliom_parameter.param_name,
      unit,
      Eliom_service.non_ocaml )
    Eliom_service.t

  val source_code :
    ( unit,
      unit,
      Eliom_service.get,
      Eliom_service.att,
      Eliom_service.non_co,
      Eliom_service.non_ext,
      Eliom_service.reg,
      [ `WithoutSuffix ],
      unit,
      unit,
      Eliom_service.non_ocaml )
    Eliom_service.t

  val logo :
    ( unit,
      unit,
      Eliom_service.get,
      Eliom_service.att,
      Eliom_service.non_co,
      Eliom_service.non_ext,
      Eliom_service.reg,
      [ `WithoutSuffix ],
      unit,
      unit,
      Eliom_service.non_ocaml )
    Eliom_service.t

  val favicon :
    ( unit,
      unit,
      Eliom_service.get,
      Eliom_service.att,
      Eliom_service.non_co,
      Eliom_service.non_ext,
      Eliom_service.reg,
      [ `WithoutSuffix ],
      unit,
      unit,
      Eliom_service.non_ocaml )
    Eliom_service.t

  val sealing :
    ( unit,
      unit,
      Eliom_service.get,
      Eliom_service.att,
      Eliom_service.non_co,
      Eliom_service.non_ext,
      Eliom_service.reg,
      [ `WithoutSuffix ],
      unit,
      unit,
      Eliom_service.non_ocaml )
    Eliom_service.t

  val election_home_dir :
    ( uuid,
      unit,
      Eliom_service.get,
      Eliom_service.att,
      Eliom_service.non_co,
      Eliom_service.non_ext,
      Eliom_service.reg,
      [ `WithSuffix ],
      [ `One of uuid ] Eliom_parameter.param_name,
      unit,
      Eliom_service.non_ocaml )
    Eliom_service.t

  val election_home_redirect :
    ( uuid * unit,
      unit,
      Eliom_service.get,
      Eliom_service.att,
      Eliom_service.non_co,
      Eliom_service.non_ext,
      Eliom_service.reg,
      [ `WithSuffix ],
      [ `One of uuid ] Eliom_parameter.param_name
      * [ `One of unit ] Eliom_parameter.param_name,
      unit,
      Eliom_service.non_ocaml )
    Eliom_service.t

  val set_consent :
    ( Web_common.site_cont,
      unit,
      Eliom_service.get,
      Eliom_service.non_att,
      Eliom_service.co,
      Eliom_service.non_ext,
      Eliom_service.reg,
      [ `WithoutSuffix ],
      [ `One of Web_common.site_cont ] Eliom_parameter.param_name,
      unit,
      Eliom_service.non_ocaml )
    Eliom_service.t

  val election_admin :
    ( uuid,
      unit,
      Eliom_service.get,
      Eliom_service.att,
      Eliom_service.non_co,
      Eliom_service.non_ext,
      Eliom_service.reg,
      [ `WithoutSuffix ],
      [ `One of uuid ] Eliom_parameter.param_name,
      unit,
      Eliom_service.non_ocaml )
    Eliom_service.t

  val election_regenpwd :
    ( uuid,
      unit,
      Eliom_service.get,
      Eliom_service.att,
      Eliom_service.non_co,
      Eliom_service.non_ext,
      Eliom_service.reg,
      [ `WithoutSuffix ],
      [ `One of uuid ] Eliom_parameter.param_name,
      unit,
      Eliom_service.non_ocaml )
    Eliom_service.t

  val election_regenpwd_post :
    ( uuid,
      string,
      Eliom_service.post,
      Eliom_service.att,
      Eliom_service.co,
      Eliom_service.non_ext,
      Eliom_service.reg,
      [ `WithoutSuffix ],
      [ `One of uuid ] Eliom_parameter.param_name,
      [ `One of string ] Eliom_parameter.param_name,
      Eliom_service.non_ocaml )
    Eliom_service.t

  val election_login :
    ( string,
      unit,
      Eliom_service.get,
      Eliom_service.att,
      Eliom_service.non_co,
      Eliom_service.non_ext,
      Eliom_service.reg,
      [ `WithoutSuffix ],
      [ `One of string ] Eliom_parameter.param_name,
      unit,
      Eliom_service.non_ocaml )
    Eliom_service.t

  val election_login_done :
    ( uuid * string,
      unit,
      Eliom_service.get,
      Eliom_service.att,
      Eliom_service.non_co,
      Eliom_service.non_ext,
      Eliom_service.reg,
      [ `WithoutSuffix ],
      [ `One of uuid ] Eliom_parameter.param_name
      * [ `One of string ] Eliom_parameter.param_name,
      unit,
      Eliom_service.non_ocaml )
    Eliom_service.t

  val election_open :
    ( uuid,
      unit,
      Eliom_service.post,
      Eliom_service.att,
      Eliom_service.co,
      Eliom_service.non_ext,
      Eliom_service.reg,
      [ `WithoutSuffix ],
      [ `One of uuid ] Eliom_parameter.param_name,
      unit,
      Eliom_service.non_ocaml )
    Eliom_service.t

  val election_close :
    ( uuid,
      unit,
      Eliom_service.post,
      Eliom_service.att,
      Eliom_service.co,
      Eliom_service.non_ext,
      Eliom_service.reg,
      [ `WithoutSuffix ],
      [ `One of uuid ] Eliom_parameter.param_name,
      unit,
      Eliom_service.non_ocaml )
    Eliom_service.t

  val election_hide_result :
    ( uuid,
      string,
      Eliom_service.post,
      Eliom_service.att,
      Eliom_service.co,
      Eliom_service.non_ext,
      Eliom_service.reg,
      [ `WithoutSuffix ],
      [ `One of uuid ] Eliom_parameter.param_name,
      [ `One of string ] Eliom_parameter.param_name,
      Eliom_service.non_ocaml )
    Eliom_service.t

  val election_show_result :
    ( uuid,
      unit,
      Eliom_service.post,
      Eliom_service.att,
      Eliom_service.co,
      Eliom_service.non_ext,
      Eliom_service.reg,
      [ `WithoutSuffix ],
      [ `One of uuid ] Eliom_parameter.param_name,
      unit,
      Eliom_service.non_ocaml )
    Eliom_service.t

  val election_auto_post :
    ( uuid,
      string * string,
      Eliom_service.post,
      Eliom_service.att,
      Eliom_service.co,
      Eliom_service.non_ext,
      Eliom_service.reg,
      [ `WithoutSuffix ],
      [ `One of uuid ] Eliom_parameter.param_name,
      [ `One of string ] Eliom_parameter.param_name
      * [ `One of string ] Eliom_parameter.param_name,
      Eliom_service.non_ocaml )
    Eliom_service.t

  val election_delete :
    ( uuid,
      unit,
      Eliom_service.post,
      Eliom_service.att,
      Eliom_service.co,
      Eliom_service.non_ext,
      Eliom_service.reg,
      [ `WithoutSuffix ],
      [ `One of uuid ] Eliom_parameter.param_name,
      unit,
      Eliom_service.non_ocaml )
    Eliom_service.t

  val booth_v2 :
    unit ->
    ( unit,
      unit,
      Eliom_service.get,
      Eliom_service.att,
      Eliom_service.non_co,
      Eliom_service.non_ext,
      Eliom_service.non_reg,
      [ `WithoutSuffix ],
      unit,
      unit,
      Eliom_service.non_ocaml )
    Eliom_service.t

  type booth =
    | Booth :
        (unit ->
        ( unit,
          unit,
          Eliom_service.get,
          Eliom_service.att,
          Eliom_service.non_co,
          Eliom_service.non_ext,
          'reg,
          [ `WithoutSuffix ],
          unit,
          unit,
          Eliom_service.non_ocaml )
        Eliom_service.t)
        -> booth

  val booths : (booth * string) array

  val election_submit_ballot :
    ( unit,
      string,
      Eliom_service.post,
      Eliom_service.att,
      Eliom_service.non_co,
      Eliom_service.non_ext,
      Eliom_service.reg,
      [ `WithoutSuffix ],
      unit,
      [ `One of string ] Eliom_parameter.param_name,
      Eliom_service.non_ocaml )
    Eliom_service.t

  val election_cast_confirm :
    ( string,
      unit,
      Eliom_service.get,
      Eliom_service.att,
      Eliom_service.non_co,
      Eliom_service.non_ext,
      Eliom_service.reg,
      [ `WithoutSuffix ],
      [ `One of string ] Eliom_parameter.param_name,
      unit,
      Eliom_service.non_ocaml )
    Eliom_service.t

  val election_pretty_records :
    ( uuid * unit,
      unit,
      Eliom_service.get,
      Eliom_service.att,
      Eliom_service.non_co,
      Eliom_service.non_ext,
      Eliom_service.reg,
      [ `WithSuffix ],
      [ `One of uuid ] Eliom_parameter.param_name
      * [ `One of unit ] Eliom_parameter.param_name,
      unit,
      Eliom_service.non_ocaml )
    Eliom_service.t

  val election_project_result :
    ( (uuid * unit) * int,
      unit,
      Eliom_service.get,
      Eliom_service.att,
      Eliom_service.non_co,
      Eliom_service.non_ext,
      Eliom_service.reg,
      [ `WithSuffix ],
      ([ `One of uuid ] Eliom_parameter.param_name
      * [ `One of unit ] Eliom_parameter.param_name)
      * [ `One of int ] Eliom_parameter.param_name,
      unit,
      Eliom_service.non_ocaml )
    Eliom_service.t

  val election_missing_voters :
    ( uuid * unit,
      unit,
      Eliom_service.get,
      Eliom_service.att,
      Eliom_service.non_co,
      Eliom_service.non_ext,
      Eliom_service.reg,
      [ `WithSuffix ],
      [ `One of uuid ] Eliom_parameter.param_name
      * [ `One of unit ] Eliom_parameter.param_name,
      unit,
      Eliom_service.non_ocaml )
    Eliom_service.t

  val election_download_archive :
    ( uuid * unit,
      unit,
      Eliom_service.get,
      Eliom_service.att,
      Eliom_service.non_co,
      Eliom_service.non_ext,
      Eliom_service.reg,
      [ `WithSuffix ],
      [ `One of uuid ] Eliom_parameter.param_name
      * [ `One of unit ] Eliom_parameter.param_name,
      unit,
      Eliom_service.non_ocaml )
    Eliom_service.t

  val election_compute_encrypted_tally :
    ( uuid,
      unit,
      Eliom_service.post,
      Eliom_service.att,
      Eliom_service.co,
      Eliom_service.non_ext,
      Eliom_service.reg,
      [ `WithoutSuffix ],
      [ `One of uuid ] Eliom_parameter.param_name,
      unit,
      Eliom_service.non_ocaml )
    Eliom_service.t

  val election_shuffle_link :
    ( uuid * string,
      unit,
      Eliom_service.get,
      Eliom_service.att,
      Eliom_service.non_co,
      Eliom_service.non_ext,
      Eliom_service.reg,
      [ `WithSuffix ],
      [ `One of uuid ] Eliom_parameter.param_name
      * [ `One of string ] Eliom_parameter.param_name,
      unit,
      Eliom_service.non_ocaml )
    Eliom_service.t

  val election_shuffler_select :
    ( unit,
      uuid * string,
      Eliom_service.post,
      Eliom_service.non_att,
      Eliom_service.co,
      Eliom_service.non_ext,
      Eliom_service.reg,
      [ `WithoutSuffix ],
      unit,
      [ `One of uuid ] Eliom_parameter.param_name
      * [ `One of string ] Eliom_parameter.param_name,
      Eliom_service.non_ocaml )
    Eliom_service.t

  val election_shuffler_skip_confirm :
    ( unit,
      uuid * string,
      Eliom_service.post,
      Eliom_service.non_att,
      Eliom_service.co,
      Eliom_service.non_ext,
      Eliom_service.reg,
      [ `WithoutSuffix ],
      unit,
      [ `One of uuid ] Eliom_parameter.param_name
      * [ `One of string ] Eliom_parameter.param_name,
      Eliom_service.non_ocaml )
    Eliom_service.t

  val election_shuffler_skip :
    ( unit,
      uuid * string,
      Eliom_service.post,
      Eliom_service.non_att,
      Eliom_service.co,
      Eliom_service.non_ext,
      Eliom_service.reg,
      [ `WithoutSuffix ],
      unit,
      [ `One of uuid ] Eliom_parameter.param_name
      * [ `One of string ] Eliom_parameter.param_name,
      Eliom_service.non_ocaml )
    Eliom_service.t

  val election_decrypt :
    ( uuid,
      unit,
      Eliom_service.post,
      Eliom_service.att,
      Eliom_service.co,
      Eliom_service.non_ext,
      Eliom_service.reg,
      [ `WithoutSuffix ],
      [ `One of uuid ] Eliom_parameter.param_name,
      unit,
      Eliom_service.non_ocaml )
    Eliom_service.t

  val election_tally_trustees :
    ( uuid * string,
      unit,
      Eliom_service.get,
      Eliom_service.att,
      Eliom_service.non_co,
      Eliom_service.non_ext,
      Eliom_service.reg,
      [ `WithSuffix ],
      [ `One of uuid ] Eliom_parameter.param_name
      * [ `One of string ] Eliom_parameter.param_name,
      unit,
      Eliom_service.non_ocaml )
    Eliom_service.t

  val election_tally_release :
    ( uuid,
      unit,
      Eliom_service.post,
      Eliom_service.att,
      Eliom_service.co,
      Eliom_service.non_ext,
      Eliom_service.reg,
      [ `WithoutSuffix ],
      [ `One of uuid ] Eliom_parameter.param_name,
      unit,
      Eliom_service.non_ocaml )
    Eliom_service.t

  val dummy_post :
    ( unit,
      string * string,
      Eliom_service.post,
      Eliom_service.non_att,
      Eliom_service.co,
      Eliom_service.non_ext,
      Eliom_service.reg,
      [ `WithoutSuffix ],
      unit,
      [ `One of string ] Eliom_parameter.param_name
      * [ `One of string ] Eliom_parameter.param_name,
      Eliom_service.non_ocaml )
    Eliom_service.t

  val email_post :
    ( unit,
      string * string,
      Eliom_service.post,
      Eliom_service.non_att,
      Eliom_service.co,
      Eliom_service.non_ext,
      Eliom_service.reg,
      [ `WithoutSuffix ],
      unit,
      [ `One of string ] Eliom_parameter.param_name
      * [ `One of string ] Eliom_parameter.param_name,
      Eliom_service.non_ocaml )
    Eliom_service.t

  val email_election_login :
    ( unit,
      unit,
      Eliom_service.get,
      Eliom_service.non_att,
      Eliom_service.co,
      Eliom_service.non_ext,
      Eliom_service.reg,
      [ `WithoutSuffix ],
      unit,
      unit,
      Eliom_service.non_ocaml )
    Eliom_service.t

  val email_captcha_post :
    ( unit,
      string * (string * (string * string)),
      Eliom_service.post,
      Eliom_service.non_att,
      Eliom_service.co,
      Eliom_service.non_ext,
      Eliom_service.reg,
      [ `WithoutSuffix ],
      unit,
      [ `One of string ] Eliom_parameter.param_name
      * ([ `One of string ] Eliom_parameter.param_name
        * ([ `One of string ] Eliom_parameter.param_name
          * [ `One of string ] Eliom_parameter.param_name)),
      Eliom_service.non_ocaml )
    Eliom_service.t

  val email_login_post :
    ( unit,
      string,
      Eliom_service.post,
      Eliom_service.non_att,
      Eliom_service.co,
      Eliom_service.non_ext,
      Eliom_service.reg,
      [ `WithoutSuffix ],
      unit,
      [ `One of string ] Eliom_parameter.param_name,
      Eliom_service.non_ocaml )
    Eliom_service.t

  val password_post :
    ( unit,
      string * (string * string),
      Eliom_service.post,
      Eliom_service.non_att,
      Eliom_service.co,
      Eliom_service.non_ext,
      Eliom_service.reg,
      [ `WithoutSuffix ],
      unit,
      [ `One of string ] Eliom_parameter.param_name
      * ([ `One of string ] Eliom_parameter.param_name
        * [ `One of string ] Eliom_parameter.param_name),
      Eliom_service.non_ocaml )
    Eliom_service.t

  val set_language :
    ( string * Web_common.site_cont,
      unit,
      Eliom_service.get,
      Eliom_service.non_att,
      Eliom_service.co,
      Eliom_service.non_ext,
      Eliom_service.reg,
      [ `WithoutSuffix ],
      [ `One of string ] Eliom_parameter.param_name
      * [ `One of Web_common.site_cont ] Eliom_parameter.param_name,
      unit,
      Eliom_service.non_ocaml )
    Eliom_service.t

  val signup_captcha :
    ( string,
      unit,
      Eliom_service.get,
      Eliom_service.att,
      Eliom_service.non_co,
      Eliom_service.non_ext,
      Eliom_service.reg,
      [ `WithoutSuffix ],
      [ `One of string ] Eliom_parameter.param_name,
      unit,
      Eliom_service.non_ocaml )
    Eliom_service.t

  val signup_captcha_post :
    ( string,
      string * (string * string),
      Eliom_service.post,
      Eliom_service.att,
      Eliom_service.co,
      Eliom_service.non_ext,
      Eliom_service.reg,
      [ `WithoutSuffix ],
      [ `One of string ] Eliom_parameter.param_name,
      [ `One of string ] Eliom_parameter.param_name
      * ([ `One of string ] Eliom_parameter.param_name
        * [ `One of string ] Eliom_parameter.param_name),
      Eliom_service.non_ocaml )
    Eliom_service.t

  val signup_captcha_img :
    ( string,
      unit,
      Eliom_service.get,
      Eliom_service.att,
      Eliom_service.non_co,
      Eliom_service.non_ext,
      Eliom_service.reg,
      [ `WithoutSuffix ],
      [ `One of string ] Eliom_parameter.param_name,
      unit,
      Eliom_service.non_ocaml )
    Eliom_service.t

  val signup_login_post :
    ( unit,
      string,
      Eliom_service.post,
      Eliom_service.non_att,
      Eliom_service.co,
      Eliom_service.non_ext,
      Eliom_service.reg,
      [ `WithoutSuffix ],
      unit,
      [ `One of string ] Eliom_parameter.param_name,
      Eliom_service.non_ocaml )
    Eliom_service.t

  val signup :
    ( unit,
      unit,
      Eliom_service.get,
      Eliom_service.att,
      Eliom_service.non_co,
      Eliom_service.non_ext,
      Eliom_service.reg,
      [ `WithoutSuffix ],
      unit,
      unit,
      Eliom_service.non_ocaml )
    Eliom_service.t

  val signup_post :
    ( unit,
      string * (string * string),
      Eliom_service.post,
      Eliom_service.att,
      Eliom_service.co,
      Eliom_service.non_ext,
      Eliom_service.reg,
      [ `WithoutSuffix ],
      unit,
      [ `One of string ] Eliom_parameter.param_name
      * ([ `One of string ] Eliom_parameter.param_name
        * [ `One of string ] Eliom_parameter.param_name),
      Eliom_service.non_ocaml )
    Eliom_service.t

  val changepw_captcha :
    ( string,
      unit,
      Eliom_service.get,
      Eliom_service.att,
      Eliom_service.non_co,
      Eliom_service.non_ext,
      Eliom_service.reg,
      [ `WithoutSuffix ],
      [ `One of string ] Eliom_parameter.param_name,
      unit,
      Eliom_service.non_ocaml )
    Eliom_service.t

  val changepw_captcha_post :
    ( string,
      string * (string * (string * string)),
      Eliom_service.post,
      Eliom_service.att,
      Eliom_service.co,
      Eliom_service.non_ext,
      Eliom_service.reg,
      [ `WithoutSuffix ],
      [ `One of string ] Eliom_parameter.param_name,
      [ `One of string ] Eliom_parameter.param_name
      * ([ `One of string ] Eliom_parameter.param_name
        * ([ `One of string ] Eliom_parameter.param_name
          * [ `One of string ] Eliom_parameter.param_name)),
      Eliom_service.non_ocaml )
    Eliom_service.t

  val changepw_post :
    ( unit,
      string * string,
      Eliom_service.post,
      Eliom_service.att,
      Eliom_service.co,
      Eliom_service.non_ext,
      Eliom_service.reg,
      [ `WithoutSuffix ],
      unit,
      [ `One of string ] Eliom_parameter.param_name
      * [ `One of string ] Eliom_parameter.param_name,
      Eliom_service.non_ocaml )
    Eliom_service.t

  val compute_fingerprint :
    ( unit,
      unit,
      Eliom_service.get,
      Eliom_service.att,
      Eliom_service.non_co,
      Eliom_service.non_ext,
      Eliom_service.reg,
      [ `WithoutSuffix ],
      unit,
      unit,
      Eliom_service.non_ocaml )
    Eliom_service.t

  val set_email_post :
    ( unit,
      string,
      Eliom_service.post,
      Eliom_service.att,
      Eliom_service.co,
      Eliom_service.non_ext,
      Eliom_service.reg,
      [ `WithoutSuffix ],
      unit,
      [ `One of string ] Eliom_parameter.param_name,
      Eliom_service.non_ocaml )
    Eliom_service.t

  val set_email_confirm :
    ( unit,
      string,
      Eliom_service.post,
      Eliom_service.att,
      Eliom_service.co,
      Eliom_service.non_ext,
      Eliom_service.reg,
      [ `WithoutSuffix ],
      unit,
      [ `One of string ] Eliom_parameter.param_name,
      Eliom_service.non_ocaml )
    Eliom_service.t

  val sudo :
    ( unit,
      unit,
      Eliom_service.get,
      Eliom_service.att,
      Eliom_service.non_co,
      Eliom_service.non_ext,
      Eliom_service.reg,
      [ `WithoutSuffix ],
      unit,
      unit,
      Eliom_service.non_ocaml )
    Eliom_service.t

  val sudo_post :
    ( unit,
      string * string,
      Eliom_service.post,
      Eliom_service.att,
      Eliom_service.co,
      Eliom_service.non_ext,
      Eliom_service.reg,
      [ `WithoutSuffix ],
      unit,
      [ `One of string ] Eliom_parameter.param_name
      * [ `One of string ] Eliom_parameter.param_name,
      Eliom_service.non_ocaml )
    Eliom_service.t

  val account :
    ( unit,
      unit,
      Eliom_service.get,
      Eliom_service.att,
      Eliom_service.non_co,
      Eliom_service.non_ext,
      Eliom_service.reg,
      [ `WithoutSuffix ],
      unit,
      unit,
      Eliom_service.non_ocaml )
    Eliom_service.t

  val account_post :
    ( unit,
      string,
      Eliom_service.post,
      Eliom_service.att,
      Eliom_service.co,
      Eliom_service.non_ext,
      Eliom_service.reg,
      [ `WithoutSuffix ],
      unit,
      [ `One of string ] Eliom_parameter.param_name,
      Eliom_service.non_ocaml )
    Eliom_service.t

  val api_token :
    ( unit,
      unit,
      Eliom_service.get,
      Eliom_service.att,
      Eliom_service.non_co,
      Eliom_service.non_ext,
      Eliom_service.reg,
      [ `WithoutSuffix ],
      unit,
      unit,
      Eliom_service.non_ocaml )
    Eliom_service.t
end
