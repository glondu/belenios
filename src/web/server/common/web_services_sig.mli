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
  val set_rewrite_prefix : src:string -> dst:string -> unit

  val make_absolute_string_uri :
    ?fragment:string ->
    service:
      ( 'a,
        unit,
        Eliom.Service.get,
        _,
        _,
        _,
        _,
        [< `WithSuffix | `WithoutSuffix ],
        _,
        unit,
        _ )
      Eliom.Service.t ->
    'a ->
    string

  val uuid_and_token :
    ( uuid * string,
      [ `WithoutSuffix ],
      [ `One of uuid ] Eliom.Parameter.param_name
      * [ `One of string ] Eliom.Parameter.param_name )
    Eliom.Service.params

  val banner :
    ( string,
      unit,
      Eliom.Service.get,
      Eliom.Service.att,
      Eliom.Service.non_co,
      Eliom.Service.non_ext,
      Eliom.Service.reg,
      [ `WithoutSuffix ],
      [ `One of string ] Eliom.Parameter.param_name,
      unit,
      Eliom.Service.non_ocaml )
    Eliom.Service.t

  val home :
    ( unit,
      unit,
      Eliom.Service.get,
      Eliom.Service.att,
      Eliom.Service.non_co,
      Eliom.Service.non_ext,
      Eliom.Service.reg,
      [ `WithoutSuffix ],
      unit,
      unit,
      Eliom.Service.non_ocaml )
    Eliom.Service.t

  val apps :
    ( string,
      unit,
      Eliom.Service.get,
      Eliom.Service.att,
      Eliom.Service.non_co,
      Eliom.Service.non_ext,
      Eliom.Service.reg,
      [ `WithSuffix ],
      [ `One of string ] Eliom.Parameter.param_name,
      unit,
      Eliom.Service.non_ocaml )
    Eliom.Service.t

  val admin_basic :
    unit ->
    ( unit,
      unit,
      Eliom.Service.get,
      Eliom.Service.att,
      Eliom.Service.non_co,
      Eliom.Service.non_ext,
      Eliom.Service.non_reg,
      [ `WithoutSuffix ],
      unit,
      unit,
      Eliom.Service.non_ocaml )
    Eliom.Service.t

  val make_admin_link : uuid option -> string

  val make_trustee_link :
    uuid ->
    [< `Generate of string | `Decrypt of string | `Shuffle of string | `Check ] ->
    string

  val make_credauth_link : uuid -> [< `Generate of string ] -> string

  val privacy_notice_accept :
    ( unit,
      Web_common.privacy_cont,
      Eliom.Service.post,
      Eliom.Service.non_att,
      Eliom.Service.co,
      Eliom.Service.non_ext,
      Eliom.Service.reg,
      [ `WithoutSuffix ],
      unit,
      [ `One of Web_common.privacy_cont ] Eliom.Parameter.param_name,
      Eliom.Service.non_ocaml )
    Eliom.Service.t

  val site_login :
    ( string option * Web_common.site_cont,
      unit,
      Eliom.Service.get,
      Eliom.Service.att,
      Eliom.Service.non_co,
      Eliom.Service.non_ext,
      Eliom.Service.reg,
      [ `WithoutSuffix ],
      [ `One of string ] Eliom.Parameter.param_name
      * [ `One of Web_common.site_cont ] Eliom.Parameter.param_name,
      unit,
      Eliom.Service.non_ocaml )
    Eliom.Service.t

  val logout :
    ( Web_common.site_cont,
      unit,
      Eliom.Service.get,
      Eliom.Service.att,
      Eliom.Service.non_co,
      Eliom.Service.non_ext,
      Eliom.Service.reg,
      [ `WithoutSuffix ],
      [ `One of Web_common.site_cont ] Eliom.Parameter.param_name,
      unit,
      Eliom.Service.non_ocaml )
    Eliom.Service.t

  val source_code :
    ( unit,
      unit,
      Eliom.Service.get,
      Eliom.Service.att,
      Eliom.Service.non_co,
      Eliom.Service.non_ext,
      Eliom.Service.reg,
      [ `WithoutSuffix ],
      unit,
      unit,
      Eliom.Service.non_ocaml )
    Eliom.Service.t

  val extra_css :
    ( unit,
      unit,
      Eliom.Service.get,
      Eliom.Service.att,
      Eliom.Service.non_co,
      Eliom.Service.non_ext,
      Eliom.Service.reg,
      [ `WithoutSuffix ],
      unit,
      unit,
      Eliom.Service.non_ocaml )
    Eliom.Service.t

  val logo :
    ( unit,
      unit,
      Eliom.Service.get,
      Eliom.Service.att,
      Eliom.Service.non_co,
      Eliom.Service.non_ext,
      Eliom.Service.reg,
      [ `WithoutSuffix ],
      unit,
      unit,
      Eliom.Service.non_ocaml )
    Eliom.Service.t

  val favicon :
    ( unit,
      unit,
      Eliom.Service.get,
      Eliom.Service.att,
      Eliom.Service.non_co,
      Eliom.Service.non_ext,
      Eliom.Service.reg,
      [ `WithoutSuffix ],
      unit,
      unit,
      Eliom.Service.non_ocaml )
    Eliom.Service.t

  val sealing :
    ( unit,
      unit,
      Eliom.Service.get,
      Eliom.Service.att,
      Eliom.Service.non_co,
      Eliom.Service.non_ext,
      Eliom.Service.reg,
      [ `WithoutSuffix ],
      unit,
      unit,
      Eliom.Service.non_ocaml )
    Eliom.Service.t

  val election_home_dir :
    ( uuid,
      unit,
      Eliom.Service.get,
      Eliom.Service.att,
      Eliom.Service.non_co,
      Eliom.Service.non_ext,
      Eliom.Service.reg,
      [ `WithSuffix ],
      [ `One of uuid ] Eliom.Parameter.param_name,
      unit,
      Eliom.Service.non_ocaml )
    Eliom.Service.t

  val election_home_redirect :
    ( uuid * unit,
      unit,
      Eliom.Service.get,
      Eliom.Service.att,
      Eliom.Service.non_co,
      Eliom.Service.non_ext,
      Eliom.Service.reg,
      [ `WithSuffix ],
      [ `One of uuid ] Eliom.Parameter.param_name
      * [ `One of unit ] Eliom.Parameter.param_name,
      unit,
      Eliom.Service.non_ocaml )
    Eliom.Service.t

  val set_consent :
    ( Web_common.site_cont,
      unit,
      Eliom.Service.get,
      Eliom.Service.non_att,
      Eliom.Service.co,
      Eliom.Service.non_ext,
      Eliom.Service.reg,
      [ `WithoutSuffix ],
      [ `One of Web_common.site_cont ] Eliom.Parameter.param_name,
      unit,
      Eliom.Service.non_ocaml )
    Eliom.Service.t

  val election_login :
    ( string,
      unit,
      Eliom.Service.get,
      Eliom.Service.att,
      Eliom.Service.non_co,
      Eliom.Service.non_ext,
      Eliom.Service.reg,
      [ `WithoutSuffix ],
      [ `One of string ] Eliom.Parameter.param_name,
      unit,
      Eliom.Service.non_ocaml )
    Eliom.Service.t

  val election_login_done :
    ( uuid * string,
      unit,
      Eliom.Service.get,
      Eliom.Service.att,
      Eliom.Service.non_co,
      Eliom.Service.non_ext,
      Eliom.Service.reg,
      [ `WithoutSuffix ],
      [ `One of uuid ] Eliom.Parameter.param_name
      * [ `One of string ] Eliom.Parameter.param_name,
      unit,
      Eliom.Service.non_ocaml )
    Eliom.Service.t

  val booth_v2 :
    unit ->
    ( unit,
      unit,
      Eliom.Service.get,
      Eliom.Service.att,
      Eliom.Service.non_co,
      Eliom.Service.non_ext,
      Eliom.Service.non_reg,
      [ `WithoutSuffix ],
      unit,
      unit,
      Eliom.Service.non_ocaml )
    Eliom.Service.t

  type booth =
    | Booth :
        (unit ->
        ( unit,
          unit,
          Eliom.Service.get,
          Eliom.Service.att,
          Eliom.Service.non_co,
          Eliom.Service.non_ext,
          'reg,
          [ `WithoutSuffix ],
          unit,
          unit,
          Eliom.Service.non_ocaml )
        Eliom.Service.t)
        -> booth

  val booths : (booth * string) array

  val election_cast_confirm :
    ( string,
      unit,
      Eliom.Service.get,
      Eliom.Service.att,
      Eliom.Service.non_co,
      Eliom.Service.non_ext,
      Eliom.Service.reg,
      [ `WithoutSuffix ],
      [ `One of string ] Eliom.Parameter.param_name,
      unit,
      Eliom.Service.non_ocaml )
    Eliom.Service.t

  val dummy_post :
    ( unit,
      string * string,
      Eliom.Service.post,
      Eliom.Service.non_att,
      Eliom.Service.co,
      Eliom.Service.non_ext,
      Eliom.Service.reg,
      [ `WithoutSuffix ],
      unit,
      [ `One of string ] Eliom.Parameter.param_name
      * [ `One of string ] Eliom.Parameter.param_name,
      Eliom.Service.non_ocaml )
    Eliom.Service.t

  val email_post :
    ( unit,
      string * string,
      Eliom.Service.post,
      Eliom.Service.non_att,
      Eliom.Service.co,
      Eliom.Service.non_ext,
      Eliom.Service.reg,
      [ `WithoutSuffix ],
      unit,
      [ `One of string ] Eliom.Parameter.param_name
      * [ `One of string ] Eliom.Parameter.param_name,
      Eliom.Service.non_ocaml )
    Eliom.Service.t

  val email_election_login :
    ( string,
      unit,
      Eliom.Service.get,
      Eliom.Service.non_att,
      Eliom.Service.co,
      Eliom.Service.non_ext,
      Eliom.Service.reg,
      [ `WithoutSuffix ],
      [ `One of string ] Eliom.Parameter.param_name,
      unit,
      Eliom.Service.non_ocaml )
    Eliom.Service.t

  val email_captcha_post :
    ( unit,
      string * (string * (string * string)),
      Eliom.Service.post,
      Eliom.Service.non_att,
      Eliom.Service.co,
      Eliom.Service.non_ext,
      Eliom.Service.reg,
      [ `WithoutSuffix ],
      unit,
      [ `One of string ] Eliom.Parameter.param_name
      * ([ `One of string ] Eliom.Parameter.param_name
        * ([ `One of string ] Eliom.Parameter.param_name
          * [ `One of string ] Eliom.Parameter.param_name)),
      Eliom.Service.non_ocaml )
    Eliom.Service.t

  val email_login_post :
    ( unit,
      string * string,
      Eliom.Service.post,
      Eliom.Service.non_att,
      Eliom.Service.co,
      Eliom.Service.non_ext,
      Eliom.Service.reg,
      [ `WithoutSuffix ],
      unit,
      [ `One of string ] Eliom.Parameter.param_name
      * [ `One of string ] Eliom.Parameter.param_name,
      Eliom.Service.non_ocaml )
    Eliom.Service.t

  val email_login_link :
    ( string * string,
      unit,
      Eliom.Service.get,
      Eliom.Service.att,
      Eliom.Service.non_co,
      Eliom.Service.non_ext,
      Eliom.Service.reg,
      [ `WithSuffix ],
      [ `One of string ] Eliom.Parameter.param_name
      * [ `One of string ] Eliom.Parameter.param_name,
      unit,
      Eliom.Service.non_ocaml )
    Eliom.Service.t

  val password_post :
    ( unit,
      string * (string * string),
      Eliom.Service.post,
      Eliom.Service.non_att,
      Eliom.Service.co,
      Eliom.Service.non_ext,
      Eliom.Service.reg,
      [ `WithoutSuffix ],
      unit,
      [ `One of string ] Eliom.Parameter.param_name
      * ([ `One of string ] Eliom.Parameter.param_name
        * [ `One of string ] Eliom.Parameter.param_name),
      Eliom.Service.non_ocaml )
    Eliom.Service.t

  val set_language :
    ( string * Web_common.site_cont,
      unit,
      Eliom.Service.get,
      Eliom.Service.non_att,
      Eliom.Service.co,
      Eliom.Service.non_ext,
      Eliom.Service.reg,
      [ `WithoutSuffix ],
      [ `One of string ] Eliom.Parameter.param_name
      * [ `One of Web_common.site_cont ] Eliom.Parameter.param_name,
      unit,
      Eliom.Service.non_ocaml )
    Eliom.Service.t

  val signup_captcha :
    ( string,
      unit,
      Eliom.Service.get,
      Eliom.Service.att,
      Eliom.Service.non_co,
      Eliom.Service.non_ext,
      Eliom.Service.reg,
      [ `WithoutSuffix ],
      [ `One of string ] Eliom.Parameter.param_name,
      unit,
      Eliom.Service.non_ocaml )
    Eliom.Service.t

  val signup_captcha_post :
    ( string,
      string * (string * string),
      Eliom.Service.post,
      Eliom.Service.att,
      Eliom.Service.co,
      Eliom.Service.non_ext,
      Eliom.Service.reg,
      [ `WithoutSuffix ],
      [ `One of string ] Eliom.Parameter.param_name,
      [ `One of string ] Eliom.Parameter.param_name
      * ([ `One of string ] Eliom.Parameter.param_name
        * [ `One of string ] Eliom.Parameter.param_name),
      Eliom.Service.non_ocaml )
    Eliom.Service.t

  val signup_captcha_img :
    ( string,
      unit,
      Eliom.Service.get,
      Eliom.Service.att,
      Eliom.Service.non_co,
      Eliom.Service.non_ext,
      Eliom.Service.reg,
      [ `WithoutSuffix ],
      [ `One of string ] Eliom.Parameter.param_name,
      unit,
      Eliom.Service.non_ocaml )
    Eliom.Service.t

  val signup_login_post :
    ( unit,
      string,
      Eliom.Service.post,
      Eliom.Service.non_att,
      Eliom.Service.co,
      Eliom.Service.non_ext,
      Eliom.Service.reg,
      [ `WithoutSuffix ],
      unit,
      [ `One of string ] Eliom.Parameter.param_name,
      Eliom.Service.non_ocaml )
    Eliom.Service.t

  val signup :
    ( unit,
      unit,
      Eliom.Service.get,
      Eliom.Service.att,
      Eliom.Service.non_co,
      Eliom.Service.non_ext,
      Eliom.Service.reg,
      [ `WithoutSuffix ],
      unit,
      unit,
      Eliom.Service.non_ocaml )
    Eliom.Service.t

  val signup_post :
    ( unit,
      string * (string * string),
      Eliom.Service.post,
      Eliom.Service.att,
      Eliom.Service.co,
      Eliom.Service.non_ext,
      Eliom.Service.reg,
      [ `WithoutSuffix ],
      unit,
      [ `One of string ] Eliom.Parameter.param_name
      * ([ `One of string ] Eliom.Parameter.param_name
        * [ `One of string ] Eliom.Parameter.param_name),
      Eliom.Service.non_ocaml )
    Eliom.Service.t

  val changepw_captcha :
    ( string,
      unit,
      Eliom.Service.get,
      Eliom.Service.att,
      Eliom.Service.non_co,
      Eliom.Service.non_ext,
      Eliom.Service.reg,
      [ `WithoutSuffix ],
      [ `One of string ] Eliom.Parameter.param_name,
      unit,
      Eliom.Service.non_ocaml )
    Eliom.Service.t

  val changepw_captcha_post :
    ( string,
      string * (string * (string * string)),
      Eliom.Service.post,
      Eliom.Service.att,
      Eliom.Service.co,
      Eliom.Service.non_ext,
      Eliom.Service.reg,
      [ `WithoutSuffix ],
      [ `One of string ] Eliom.Parameter.param_name,
      [ `One of string ] Eliom.Parameter.param_name
      * ([ `One of string ] Eliom.Parameter.param_name
        * ([ `One of string ] Eliom.Parameter.param_name
          * [ `One of string ] Eliom.Parameter.param_name)),
      Eliom.Service.non_ocaml )
    Eliom.Service.t

  val changepw_post :
    ( unit,
      string * string,
      Eliom.Service.post,
      Eliom.Service.att,
      Eliom.Service.co,
      Eliom.Service.non_ext,
      Eliom.Service.reg,
      [ `WithoutSuffix ],
      unit,
      [ `One of string ] Eliom.Parameter.param_name
      * [ `One of string ] Eliom.Parameter.param_name,
      Eliom.Service.non_ocaml )
    Eliom.Service.t

  val compute_fingerprint :
    ( unit,
      unit,
      Eliom.Service.get,
      Eliom.Service.att,
      Eliom.Service.non_co,
      Eliom.Service.non_ext,
      Eliom.Service.reg,
      [ `WithoutSuffix ],
      unit,
      unit,
      Eliom.Service.non_ocaml )
    Eliom.Service.t

  val set_email_post :
    ( unit,
      string,
      Eliom.Service.post,
      Eliom.Service.att,
      Eliom.Service.co,
      Eliom.Service.non_ext,
      Eliom.Service.reg,
      [ `WithoutSuffix ],
      unit,
      [ `One of string ] Eliom.Parameter.param_name,
      Eliom.Service.non_ocaml )
    Eliom.Service.t

  val set_email_confirm :
    ( unit,
      string,
      Eliom.Service.post,
      Eliom.Service.att,
      Eliom.Service.co,
      Eliom.Service.non_ext,
      Eliom.Service.reg,
      [ `WithoutSuffix ],
      unit,
      [ `One of string ] Eliom.Parameter.param_name,
      Eliom.Service.non_ocaml )
    Eliom.Service.t

  val sudo :
    ( unit,
      unit,
      Eliom.Service.get,
      Eliom.Service.att,
      Eliom.Service.non_co,
      Eliom.Service.non_ext,
      Eliom.Service.reg,
      [ `WithoutSuffix ],
      unit,
      unit,
      Eliom.Service.non_ocaml )
    Eliom.Service.t

  val sudo_post :
    ( unit,
      string * string,
      Eliom.Service.post,
      Eliom.Service.att,
      Eliom.Service.co,
      Eliom.Service.non_ext,
      Eliom.Service.reg,
      [ `WithoutSuffix ],
      unit,
      [ `One of string ] Eliom.Parameter.param_name
      * [ `One of string ] Eliom.Parameter.param_name,
      Eliom.Service.non_ocaml )
    Eliom.Service.t

  val api_token :
    ( unit,
      unit,
      Eliom.Service.get,
      Eliom.Service.att,
      Eliom.Service.non_co,
      Eliom.Service.non_ext,
      Eliom.Service.reg,
      [ `WithoutSuffix ],
      unit,
      unit,
      Eliom.Service.non_ocaml )
    Eliom.Service.t

  val connect_login :
    ( string * string,
      unit,
      Eliom.Service.get,
      Eliom.Service.att,
      Eliom.Service.non_co,
      Eliom.Service.non_ext,
      Eliom.Service.reg,
      [ `WithoutSuffix ],
      [ `One of string ] Eliom.Parameter.param_name
      * [ `One of string ] Eliom.Parameter.param_name,
      unit,
      Eliom.Service.non_ocaml )
    Eliom.Service.t

  val connect_consent :
    ( string * string,
      unit,
      Eliom.Service.post,
      Eliom.Service.att,
      Eliom.Service.co,
      Eliom.Service.non_ext,
      Eliom.Service.reg,
      [ `WithoutSuffix ],
      [ `One of string ] Eliom.Parameter.param_name
      * [ `One of string ] Eliom.Parameter.param_name,
      unit,
      Eliom.Service.non_ocaml )
    Eliom.Service.t

  val connect_validate :
    ( string,
      unit,
      Eliom.Service.get,
      Eliom.Service.att,
      Eliom.Service.non_co,
      Eliom.Service.non_ext,
      Eliom.Service.reg,
      [ `WithoutSuffix ],
      [ `One of string ] Eliom.Parameter.param_name,
      unit,
      Eliom.Service.non_ocaml )
    Eliom.Service.t
end
