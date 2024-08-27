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

  val election_draft_new :
    ( unit,
      string option * (string option * string option),
      Eliom_service.post,
      Eliom_service.att,
      Eliom_service.co,
      Eliom_service.non_ext,
      Eliom_service.reg,
      [ `WithoutSuffix ],
      unit,
      [ `Radio of string ] Eliom_parameter.param_name
      * ([ `Radio of string ] Eliom_parameter.param_name
        * [ `One of string ] Eliom_parameter.param_name),
      Eliom_service.non_ocaml )
    Eliom_service.t

  val election_draft_pre :
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

  val election_draft :
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

  val election_draft_questions :
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

  val election_draft_questions_post :
    ( uuid,
      string * int,
      Eliom_service.post,
      Eliom_service.att,
      Eliom_service.co,
      Eliom_service.non_ext,
      Eliom_service.reg,
      [ `WithoutSuffix ],
      [ `One of uuid ] Eliom_parameter.param_name,
      [ `One of string ] Eliom_parameter.param_name
      * [ `One of int ] Eliom_parameter.param_name,
      Eliom_service.non_ocaml )
    Eliom_service.t

  val election_draft_description :
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

  val election_draft_languages :
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

  val election_draft_contact :
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

  val election_draft_admin_name :
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

  val election_draft_voters :
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

  val election_draft_voters_add :
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

  val election_draft_voters_remove :
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

  val election_draft_voters_remove_all :
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

  val election_draft_voters_passwd :
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

  val election_draft_trustee_add :
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

  val election_draft_trustee_del :
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

  val election_draft_credential_authority :
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

  val election_draft_set_credential_authority :
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

  val election_draft_credentials :
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

  val election_draft_credentials_server :
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

  val election_draft_credentials_get :
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

  val election_draft_trustees :
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

  val election_draft_trustee :
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

  val election_draft_threshold_trustees :
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

  val election_draft_threshold_trustee :
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

  val election_draft_threshold_set :
    ( uuid,
      int,
      Eliom_service.post,
      Eliom_service.att,
      Eliom_service.co,
      Eliom_service.non_ext,
      Eliom_service.reg,
      [ `WithoutSuffix ],
      [ `One of uuid ] Eliom_parameter.param_name,
      [ `One of int ] Eliom_parameter.param_name,
      Eliom_service.non_ocaml )
    Eliom_service.t

  val election_draft_threshold_trustee_add :
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

  val election_draft_threshold_trustee_del :
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

  val election_draft_confirm :
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

  val election_draft_prebilling :
    ( string * Web_common.site_cont,
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

  val election_draft_postbilling :
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

  val election_draft_create :
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

  val election_draft_destroy :
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

  val election_draft_auth_genpwd :
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

  val election_draft_import :
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

  val election_draft_import_post :
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

  val election_draft_import_trustees :
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

  val election_draft_import_trustees_post :
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

  val election_home :
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
    ( (uuid * unit) * string option,
      unit,
      Eliom_service.get,
      Eliom_service.att,
      Eliom_service.non_co,
      Eliom_service.non_ext,
      Eliom_service.reg,
      [ `WithSuffix ],
      ([ `One of uuid ] Eliom_parameter.param_name
      * [ `One of unit ] Eliom_parameter.param_name)
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

  val election_cast :
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

  val election_submit_ballot_file :
    ( unit,
      Ocsigen_multipart.file_info,
      Eliom_service.post,
      Eliom_service.att,
      Eliom_service.non_co,
      Eliom_service.non_ext,
      Eliom_service.reg,
      [ `WithoutSuffix ],
      unit,
      [ `One of Ocsigen_multipart.file_info ] Eliom_parameter.param_name,
      Eliom_service.non_ocaml )
    Eliom_service.t

  val election_submit_ballot_check :
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

  val election_cast_confirm :
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

  val election_pretty_ballots :
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

  val election_pretty_ballot :
    ( (uuid * unit) * string,
      unit,
      Eliom_service.get,
      Eliom_service.att,
      Eliom_service.non_co,
      Eliom_service.non_ext,
      Eliom_service.reg,
      [ `WithSuffix ],
      ([ `One of uuid ] Eliom_parameter.param_name
      * [ `One of unit ] Eliom_parameter.param_name)
      * [ `One of string ] Eliom_parameter.param_name,
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

  val election_dir :
    ( uuid * Web_common.election_file,
      unit,
      Eliom_service.get,
      Eliom_service.att,
      Eliom_service.non_co,
      Eliom_service.non_ext,
      Eliom_service.reg,
      [ `WithSuffix ],
      [ `One of uuid ] Eliom_parameter.param_name
      * [ `One of Web_common.election_file ] Eliom_parameter.param_name,
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

  val method_schulze :
    ( uuid * int,
      unit,
      Eliom_service.get,
      Eliom_service.att,
      Eliom_service.non_co,
      Eliom_service.non_ext,
      Eliom_service.reg,
      [ `WithoutSuffix ],
      [ `One of uuid ] Eliom_parameter.param_name
      * [ `One of int ] Eliom_parameter.param_name,
      unit,
      Eliom_service.non_ocaml )
    Eliom_service.t

  val method_mj :
    ( uuid * (int * int option),
      unit,
      Eliom_service.get,
      Eliom_service.att,
      Eliom_service.non_co,
      Eliom_service.non_ext,
      Eliom_service.reg,
      [ `WithoutSuffix ],
      [ `One of uuid ] Eliom_parameter.param_name
      * ([ `One of int ] Eliom_parameter.param_name
        * [ `One of int ] Eliom_parameter.param_name),
      unit,
      Eliom_service.non_ocaml )
    Eliom_service.t

  val method_stv :
    ( uuid * (int * int option),
      unit,
      Eliom_service.get,
      Eliom_service.att,
      Eliom_service.non_co,
      Eliom_service.non_ext,
      Eliom_service.reg,
      [ `WithoutSuffix ],
      [ `One of uuid ] Eliom_parameter.param_name
      * ([ `One of int ] Eliom_parameter.param_name
        * [ `One of int ] Eliom_parameter.param_name),
      unit,
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
