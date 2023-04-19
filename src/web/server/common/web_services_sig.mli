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

module type S = sig
  val uuid_and_token :
    ( Web_serializable_t.uuid * string,
      [ `WithoutSuffix ],
      [ `One of Web_serializable_t.uuid ] Eliom_parameter.param_name
      * [ `One of string ] Eliom_parameter.param_name )
    Eliom_service.params

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

  val admin :
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
    ( Web_serializable_t.uuid,
      unit,
      Eliom_service.get,
      Eliom_service.att,
      Eliom_service.non_co,
      Eliom_service.non_ext,
      Eliom_service.reg,
      [ `WithoutSuffix ],
      [ `One of Web_serializable_t.uuid ] Eliom_parameter.param_name,
      unit,
      Eliom_service.non_ocaml )
    Eliom_service.t

  val election_draft_questions :
    ( Web_serializable_t.uuid,
      unit,
      Eliom_service.get,
      Eliom_service.att,
      Eliom_service.non_co,
      Eliom_service.non_ext,
      Eliom_service.reg,
      [ `WithoutSuffix ],
      [ `One of Web_serializable_t.uuid ] Eliom_parameter.param_name,
      unit,
      Eliom_service.non_ocaml )
    Eliom_service.t

  val election_draft_questions_post :
    ( Web_serializable_t.uuid,
      string * int,
      Eliom_service.post,
      Eliom_service.att,
      Eliom_service.co,
      Eliom_service.non_ext,
      Eliom_service.reg,
      [ `WithoutSuffix ],
      [ `One of Web_serializable_t.uuid ] Eliom_parameter.param_name,
      [ `One of string ] Eliom_parameter.param_name
      * [ `One of int ] Eliom_parameter.param_name,
      Eliom_service.non_ocaml )
    Eliom_service.t

  val election_draft_preview :
    ( Web_serializable_t.uuid * unit,
      unit,
      Eliom_service.get,
      Eliom_service.att,
      Eliom_service.non_co,
      Eliom_service.non_ext,
      Eliom_service.reg,
      [ `WithSuffix ],
      [ `One of Web_serializable_t.uuid ] Eliom_parameter.param_name
      * [ `One of unit ] Eliom_parameter.param_name,
      unit,
      Eliom_service.non_ocaml )
    Eliom_service.t

  val election_draft_description :
    ( Web_serializable_t.uuid,
      string * string,
      Eliom_service.post,
      Eliom_service.att,
      Eliom_service.co,
      Eliom_service.non_ext,
      Eliom_service.reg,
      [ `WithoutSuffix ],
      [ `One of Web_serializable_t.uuid ] Eliom_parameter.param_name,
      [ `One of string ] Eliom_parameter.param_name
      * [ `One of string ] Eliom_parameter.param_name,
      Eliom_service.non_ocaml )
    Eliom_service.t

  val election_draft_languages :
    ( Web_serializable_t.uuid,
      string,
      Eliom_service.post,
      Eliom_service.att,
      Eliom_service.co,
      Eliom_service.non_ext,
      Eliom_service.reg,
      [ `WithoutSuffix ],
      [ `One of Web_serializable_t.uuid ] Eliom_parameter.param_name,
      [ `One of string ] Eliom_parameter.param_name,
      Eliom_service.non_ocaml )
    Eliom_service.t

  val election_draft_contact :
    ( Web_serializable_t.uuid,
      string,
      Eliom_service.post,
      Eliom_service.att,
      Eliom_service.co,
      Eliom_service.non_ext,
      Eliom_service.reg,
      [ `WithoutSuffix ],
      [ `One of Web_serializable_t.uuid ] Eliom_parameter.param_name,
      [ `One of string ] Eliom_parameter.param_name,
      Eliom_service.non_ocaml )
    Eliom_service.t

  val election_draft_admin_name :
    ( Web_serializable_t.uuid,
      string,
      Eliom_service.post,
      Eliom_service.att,
      Eliom_service.co,
      Eliom_service.non_ext,
      Eliom_service.reg,
      [ `WithoutSuffix ],
      [ `One of Web_serializable_t.uuid ] Eliom_parameter.param_name,
      [ `One of string ] Eliom_parameter.param_name,
      Eliom_service.non_ocaml )
    Eliom_service.t

  val election_draft_voters :
    ( Web_serializable_t.uuid,
      unit,
      Eliom_service.get,
      Eliom_service.att,
      Eliom_service.non_co,
      Eliom_service.non_ext,
      Eliom_service.reg,
      [ `WithoutSuffix ],
      [ `One of Web_serializable_t.uuid ] Eliom_parameter.param_name,
      unit,
      Eliom_service.non_ocaml )
    Eliom_service.t

  val election_draft_voters_add :
    ( Web_serializable_t.uuid,
      string,
      Eliom_service.post,
      Eliom_service.att,
      Eliom_service.co,
      Eliom_service.non_ext,
      Eliom_service.reg,
      [ `WithoutSuffix ],
      [ `One of Web_serializable_t.uuid ] Eliom_parameter.param_name,
      [ `One of string ] Eliom_parameter.param_name,
      Eliom_service.non_ocaml )
    Eliom_service.t

  val election_draft_voters_remove :
    ( Web_serializable_t.uuid,
      string,
      Eliom_service.post,
      Eliom_service.att,
      Eliom_service.co,
      Eliom_service.non_ext,
      Eliom_service.reg,
      [ `WithoutSuffix ],
      [ `One of Web_serializable_t.uuid ] Eliom_parameter.param_name,
      [ `One of string ] Eliom_parameter.param_name,
      Eliom_service.non_ocaml )
    Eliom_service.t

  val election_draft_voters_remove_all :
    ( Web_serializable_t.uuid,
      unit,
      Eliom_service.post,
      Eliom_service.att,
      Eliom_service.co,
      Eliom_service.non_ext,
      Eliom_service.reg,
      [ `WithoutSuffix ],
      [ `One of Web_serializable_t.uuid ] Eliom_parameter.param_name,
      unit,
      Eliom_service.non_ocaml )
    Eliom_service.t

  val election_draft_voters_passwd :
    ( Web_serializable_t.uuid,
      string,
      Eliom_service.post,
      Eliom_service.att,
      Eliom_service.co,
      Eliom_service.non_ext,
      Eliom_service.reg,
      [ `WithoutSuffix ],
      [ `One of Web_serializable_t.uuid ] Eliom_parameter.param_name,
      [ `One of string ] Eliom_parameter.param_name,
      Eliom_service.non_ocaml )
    Eliom_service.t

  val election_draft_trustee_add :
    ( Web_serializable_t.uuid,
      string * string,
      Eliom_service.post,
      Eliom_service.att,
      Eliom_service.co,
      Eliom_service.non_ext,
      Eliom_service.reg,
      [ `WithoutSuffix ],
      [ `One of Web_serializable_t.uuid ] Eliom_parameter.param_name,
      [ `One of string ] Eliom_parameter.param_name
      * [ `One of string ] Eliom_parameter.param_name,
      Eliom_service.non_ocaml )
    Eliom_service.t

  val election_draft_trustee_del :
    ( Web_serializable_t.uuid,
      string,
      Eliom_service.post,
      Eliom_service.att,
      Eliom_service.co,
      Eliom_service.non_ext,
      Eliom_service.reg,
      [ `WithoutSuffix ],
      [ `One of Web_serializable_t.uuid ] Eliom_parameter.param_name,
      [ `One of string ] Eliom_parameter.param_name,
      Eliom_service.non_ocaml )
    Eliom_service.t

  val election_draft_credential_authority :
    ( Web_serializable_t.uuid,
      unit,
      Eliom_service.get,
      Eliom_service.att,
      Eliom_service.non_co,
      Eliom_service.non_ext,
      Eliom_service.reg,
      [ `WithoutSuffix ],
      [ `One of Web_serializable_t.uuid ] Eliom_parameter.param_name,
      unit,
      Eliom_service.non_ocaml )
    Eliom_service.t

  val election_draft_set_credential_authority :
    ( Web_serializable_t.uuid,
      string,
      Eliom_service.post,
      Eliom_service.att,
      Eliom_service.co,
      Eliom_service.non_ext,
      Eliom_service.reg,
      [ `WithoutSuffix ],
      [ `One of Web_serializable_t.uuid ] Eliom_parameter.param_name,
      [ `One of string ] Eliom_parameter.param_name,
      Eliom_service.non_ocaml )
    Eliom_service.t

  val election_draft_credentials :
    ( Web_serializable_t.uuid * string,
      unit,
      Eliom_service.get,
      Eliom_service.att,
      Eliom_service.non_co,
      Eliom_service.non_ext,
      Eliom_service.reg,
      [ `WithSuffix ],
      [ `One of Web_serializable_t.uuid ] Eliom_parameter.param_name
      * [ `One of string ] Eliom_parameter.param_name,
      unit,
      Eliom_service.non_ocaml )
    Eliom_service.t

  val election_draft_credentials_static :
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

  val election_draft_credentials_post :
    ( Web_serializable_t.uuid * string,
      string,
      Eliom_service.post,
      Eliom_service.att,
      Eliom_service.non_co,
      Eliom_service.non_ext,
      Eliom_service.reg,
      [ `WithoutSuffix ],
      [ `One of Web_serializable_t.uuid ] Eliom_parameter.param_name
      * [ `One of string ] Eliom_parameter.param_name,
      [ `One of string ] Eliom_parameter.param_name,
      Eliom_service.non_ocaml )
    Eliom_service.t

  val election_draft_credentials_post_file :
    ( Web_serializable_t.uuid * string,
      Ocsigen_multipart.file_info,
      Eliom_service.post,
      Eliom_service.att,
      Eliom_service.non_co,
      Eliom_service.non_ext,
      Eliom_service.reg,
      [ `WithoutSuffix ],
      [ `One of Web_serializable_t.uuid ] Eliom_parameter.param_name
      * [ `One of string ] Eliom_parameter.param_name,
      [ `One of Ocsigen_multipart.file_info ] Eliom_parameter.param_name,
      Eliom_service.non_ocaml )
    Eliom_service.t

  val election_draft_credentials_server :
    ( Web_serializable_t.uuid,
      unit,
      Eliom_service.post,
      Eliom_service.att,
      Eliom_service.co,
      Eliom_service.non_ext,
      Eliom_service.reg,
      [ `WithoutSuffix ],
      [ `One of Web_serializable_t.uuid ] Eliom_parameter.param_name,
      unit,
      Eliom_service.non_ocaml )
    Eliom_service.t

  val election_draft_credentials_get :
    ( Web_serializable_t.uuid,
      unit,
      Eliom_service.get,
      Eliom_service.att,
      Eliom_service.non_co,
      Eliom_service.non_ext,
      Eliom_service.reg,
      [ `WithoutSuffix ],
      [ `One of Web_serializable_t.uuid ] Eliom_parameter.param_name,
      unit,
      Eliom_service.non_ocaml )
    Eliom_service.t

  val election_draft_trustees :
    ( Web_serializable_t.uuid,
      unit,
      Eliom_service.get,
      Eliom_service.att,
      Eliom_service.non_co,
      Eliom_service.non_ext,
      Eliom_service.reg,
      [ `WithoutSuffix ],
      [ `One of Web_serializable_t.uuid ] Eliom_parameter.param_name,
      unit,
      Eliom_service.non_ocaml )
    Eliom_service.t

  val election_draft_trustee :
    ( Web_serializable_t.uuid * string,
      unit,
      Eliom_service.get,
      Eliom_service.att,
      Eliom_service.non_co,
      Eliom_service.non_ext,
      Eliom_service.reg,
      [ `WithSuffix ],
      [ `One of Web_serializable_t.uuid ] Eliom_parameter.param_name
      * [ `One of string ] Eliom_parameter.param_name,
      unit,
      Eliom_service.non_ocaml )
    Eliom_service.t

  val election_draft_trustee_static :
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

  val election_draft_trustee_post :
    ( Web_serializable_t.uuid * string,
      string,
      Eliom_service.post,
      Eliom_service.att,
      Eliom_service.non_co,
      Eliom_service.non_ext,
      Eliom_service.reg,
      [ `WithoutSuffix ],
      [ `One of Web_serializable_t.uuid ] Eliom_parameter.param_name
      * [ `One of string ] Eliom_parameter.param_name,
      [ `One of string ] Eliom_parameter.param_name,
      Eliom_service.non_ocaml )
    Eliom_service.t

  val election_draft_threshold_trustees :
    ( Web_serializable_t.uuid,
      unit,
      Eliom_service.get,
      Eliom_service.att,
      Eliom_service.non_co,
      Eliom_service.non_ext,
      Eliom_service.reg,
      [ `WithoutSuffix ],
      [ `One of Web_serializable_t.uuid ] Eliom_parameter.param_name,
      unit,
      Eliom_service.non_ocaml )
    Eliom_service.t

  val election_draft_threshold_trustee :
    ( Web_serializable_t.uuid * string,
      unit,
      Eliom_service.get,
      Eliom_service.att,
      Eliom_service.non_co,
      Eliom_service.non_ext,
      Eliom_service.reg,
      [ `WithSuffix ],
      [ `One of Web_serializable_t.uuid ] Eliom_parameter.param_name
      * [ `One of string ] Eliom_parameter.param_name,
      unit,
      Eliom_service.non_ocaml )
    Eliom_service.t

  val election_draft_threshold_trustee_static :
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

  val election_draft_threshold_trustee_post :
    ( Web_serializable_t.uuid * string,
      string,
      Eliom_service.post,
      Eliom_service.att,
      Eliom_service.non_co,
      Eliom_service.non_ext,
      Eliom_service.reg,
      [ `WithoutSuffix ],
      [ `One of Web_serializable_t.uuid ] Eliom_parameter.param_name
      * [ `One of string ] Eliom_parameter.param_name,
      [ `One of string ] Eliom_parameter.param_name,
      Eliom_service.non_ocaml )
    Eliom_service.t

  val election_draft_threshold_set :
    ( Web_serializable_t.uuid,
      int,
      Eliom_service.post,
      Eliom_service.att,
      Eliom_service.co,
      Eliom_service.non_ext,
      Eliom_service.reg,
      [ `WithoutSuffix ],
      [ `One of Web_serializable_t.uuid ] Eliom_parameter.param_name,
      [ `One of int ] Eliom_parameter.param_name,
      Eliom_service.non_ocaml )
    Eliom_service.t

  val election_draft_threshold_trustee_add :
    ( Web_serializable_t.uuid,
      string * string,
      Eliom_service.post,
      Eliom_service.att,
      Eliom_service.co,
      Eliom_service.non_ext,
      Eliom_service.reg,
      [ `WithoutSuffix ],
      [ `One of Web_serializable_t.uuid ] Eliom_parameter.param_name,
      [ `One of string ] Eliom_parameter.param_name
      * [ `One of string ] Eliom_parameter.param_name,
      Eliom_service.non_ocaml )
    Eliom_service.t

  val election_draft_threshold_trustee_del :
    ( Web_serializable_t.uuid,
      string,
      Eliom_service.post,
      Eliom_service.att,
      Eliom_service.co,
      Eliom_service.non_ext,
      Eliom_service.reg,
      [ `WithoutSuffix ],
      [ `One of Web_serializable_t.uuid ] Eliom_parameter.param_name,
      [ `One of string ] Eliom_parameter.param_name,
      Eliom_service.non_ocaml )
    Eliom_service.t

  val election_draft_confirm :
    ( Web_serializable_t.uuid,
      unit,
      Eliom_service.get,
      Eliom_service.att,
      Eliom_service.non_co,
      Eliom_service.non_ext,
      Eliom_service.reg,
      [ `WithoutSuffix ],
      [ `One of Web_serializable_t.uuid ] Eliom_parameter.param_name,
      unit,
      Eliom_service.non_ocaml )
    Eliom_service.t

  val election_draft_create :
    ( Web_serializable_t.uuid,
      unit,
      Eliom_service.post,
      Eliom_service.att,
      Eliom_service.co,
      Eliom_service.non_ext,
      Eliom_service.reg,
      [ `WithoutSuffix ],
      [ `One of Web_serializable_t.uuid ] Eliom_parameter.param_name,
      unit,
      Eliom_service.non_ocaml )
    Eliom_service.t

  val election_draft_destroy :
    ( Web_serializable_t.uuid,
      unit,
      Eliom_service.post,
      Eliom_service.att,
      Eliom_service.co,
      Eliom_service.non_ext,
      Eliom_service.reg,
      [ `WithoutSuffix ],
      [ `One of Web_serializable_t.uuid ] Eliom_parameter.param_name,
      unit,
      Eliom_service.non_ocaml )
    Eliom_service.t

  val election_draft_auth_genpwd :
    ( Web_serializable_t.uuid,
      unit,
      Eliom_service.post,
      Eliom_service.att,
      Eliom_service.co,
      Eliom_service.non_ext,
      Eliom_service.reg,
      [ `WithoutSuffix ],
      [ `One of Web_serializable_t.uuid ] Eliom_parameter.param_name,
      unit,
      Eliom_service.non_ocaml )
    Eliom_service.t

  val election_draft_import :
    ( Web_serializable_t.uuid,
      unit,
      Eliom_service.get,
      Eliom_service.att,
      Eliom_service.non_co,
      Eliom_service.non_ext,
      Eliom_service.reg,
      [ `WithoutSuffix ],
      [ `One of Web_serializable_t.uuid ] Eliom_parameter.param_name,
      unit,
      Eliom_service.non_ocaml )
    Eliom_service.t

  val election_draft_import_post :
    ( Web_serializable_t.uuid,
      string,
      Eliom_service.post,
      Eliom_service.att,
      Eliom_service.co,
      Eliom_service.non_ext,
      Eliom_service.reg,
      [ `WithoutSuffix ],
      [ `One of Web_serializable_t.uuid ] Eliom_parameter.param_name,
      [ `One of string ] Eliom_parameter.param_name,
      Eliom_service.non_ocaml )
    Eliom_service.t

  val election_draft_import_trustees :
    ( Web_serializable_t.uuid,
      unit,
      Eliom_service.get,
      Eliom_service.att,
      Eliom_service.non_co,
      Eliom_service.non_ext,
      Eliom_service.reg,
      [ `WithoutSuffix ],
      [ `One of Web_serializable_t.uuid ] Eliom_parameter.param_name,
      unit,
      Eliom_service.non_ocaml )
    Eliom_service.t

  val election_draft_import_trustees_post :
    ( Web_serializable_t.uuid,
      string,
      Eliom_service.post,
      Eliom_service.att,
      Eliom_service.co,
      Eliom_service.non_ext,
      Eliom_service.reg,
      [ `WithoutSuffix ],
      [ `One of Web_serializable_t.uuid ] Eliom_parameter.param_name,
      [ `One of string ] Eliom_parameter.param_name,
      Eliom_service.non_ocaml )
    Eliom_service.t

  val election_home_dir :
    ( Web_serializable_t.uuid,
      unit,
      Eliom_service.get,
      Eliom_service.att,
      Eliom_service.non_co,
      Eliom_service.non_ext,
      Eliom_service.reg,
      [ `WithSuffix ],
      [ `One of Web_serializable_t.uuid ] Eliom_parameter.param_name,
      unit,
      Eliom_service.non_ocaml )
    Eliom_service.t

  val election_home :
    ( Web_serializable_t.uuid * unit,
      unit,
      Eliom_service.get,
      Eliom_service.att,
      Eliom_service.non_co,
      Eliom_service.non_ext,
      Eliom_service.reg,
      [ `WithSuffix ],
      [ `One of Web_serializable_t.uuid ] Eliom_parameter.param_name
      * [ `One of unit ] Eliom_parameter.param_name,
      unit,
      Eliom_service.non_ocaml )
    Eliom_service.t

  val set_cookie_disclaimer :
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
    ( Web_serializable_t.uuid,
      unit,
      Eliom_service.get,
      Eliom_service.att,
      Eliom_service.non_co,
      Eliom_service.non_ext,
      Eliom_service.reg,
      [ `WithoutSuffix ],
      [ `One of Web_serializable_t.uuid ] Eliom_parameter.param_name,
      unit,
      Eliom_service.non_ocaml )
    Eliom_service.t

  val election_regenpwd :
    ( Web_serializable_t.uuid,
      unit,
      Eliom_service.get,
      Eliom_service.att,
      Eliom_service.non_co,
      Eliom_service.non_ext,
      Eliom_service.reg,
      [ `WithoutSuffix ],
      [ `One of Web_serializable_t.uuid ] Eliom_parameter.param_name,
      unit,
      Eliom_service.non_ocaml )
    Eliom_service.t

  val election_regenpwd_post :
    ( Web_serializable_t.uuid,
      string,
      Eliom_service.post,
      Eliom_service.att,
      Eliom_service.co,
      Eliom_service.non_ext,
      Eliom_service.reg,
      [ `WithoutSuffix ],
      [ `One of Web_serializable_t.uuid ] Eliom_parameter.param_name,
      [ `One of string ] Eliom_parameter.param_name,
      Eliom_service.non_ocaml )
    Eliom_service.t

  val election_login :
    ( (Web_serializable_t.uuid * unit) * string option,
      unit,
      Eliom_service.get,
      Eliom_service.att,
      Eliom_service.non_co,
      Eliom_service.non_ext,
      Eliom_service.reg,
      [ `WithSuffix ],
      ([ `One of Web_serializable_t.uuid ] Eliom_parameter.param_name
      * [ `One of unit ] Eliom_parameter.param_name)
      * [ `One of string ] Eliom_parameter.param_name,
      unit,
      Eliom_service.non_ocaml )
    Eliom_service.t

  val election_open :
    ( Web_serializable_t.uuid,
      unit,
      Eliom_service.post,
      Eliom_service.att,
      Eliom_service.co,
      Eliom_service.non_ext,
      Eliom_service.reg,
      [ `WithoutSuffix ],
      [ `One of Web_serializable_t.uuid ] Eliom_parameter.param_name,
      unit,
      Eliom_service.non_ocaml )
    Eliom_service.t

  val election_close :
    ( Web_serializable_t.uuid,
      unit,
      Eliom_service.post,
      Eliom_service.att,
      Eliom_service.co,
      Eliom_service.non_ext,
      Eliom_service.reg,
      [ `WithoutSuffix ],
      [ `One of Web_serializable_t.uuid ] Eliom_parameter.param_name,
      unit,
      Eliom_service.non_ocaml )
    Eliom_service.t

  val election_hide_result :
    ( Web_serializable_t.uuid,
      string,
      Eliom_service.post,
      Eliom_service.att,
      Eliom_service.co,
      Eliom_service.non_ext,
      Eliom_service.reg,
      [ `WithoutSuffix ],
      [ `One of Web_serializable_t.uuid ] Eliom_parameter.param_name,
      [ `One of string ] Eliom_parameter.param_name,
      Eliom_service.non_ocaml )
    Eliom_service.t

  val election_show_result :
    ( Web_serializable_t.uuid,
      unit,
      Eliom_service.post,
      Eliom_service.att,
      Eliom_service.co,
      Eliom_service.non_ext,
      Eliom_service.reg,
      [ `WithoutSuffix ],
      [ `One of Web_serializable_t.uuid ] Eliom_parameter.param_name,
      unit,
      Eliom_service.non_ocaml )
    Eliom_service.t

  val election_auto_post :
    ( Web_serializable_t.uuid,
      string * string,
      Eliom_service.post,
      Eliom_service.att,
      Eliom_service.co,
      Eliom_service.non_ext,
      Eliom_service.reg,
      [ `WithoutSuffix ],
      [ `One of Web_serializable_t.uuid ] Eliom_parameter.param_name,
      [ `One of string ] Eliom_parameter.param_name
      * [ `One of string ] Eliom_parameter.param_name,
      Eliom_service.non_ocaml )
    Eliom_service.t

  val election_delete :
    ( Web_serializable_t.uuid,
      unit,
      Eliom_service.post,
      Eliom_service.att,
      Eliom_service.co,
      Eliom_service.non_ext,
      Eliom_service.reg,
      [ `WithoutSuffix ],
      [ `One of Web_serializable_t.uuid ] Eliom_parameter.param_name,
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
    ( Web_serializable_t.uuid,
      unit,
      Eliom_service.get,
      Eliom_service.att,
      Eliom_service.non_co,
      Eliom_service.non_ext,
      Eliom_service.reg,
      [ `WithoutSuffix ],
      [ `One of Web_serializable_t.uuid ] Eliom_parameter.param_name,
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
    ( Web_serializable_t.uuid,
      unit,
      Eliom_service.get,
      Eliom_service.att,
      Eliom_service.non_co,
      Eliom_service.non_ext,
      Eliom_service.reg,
      [ `WithoutSuffix ],
      [ `One of Web_serializable_t.uuid ] Eliom_parameter.param_name,
      unit,
      Eliom_service.non_ocaml )
    Eliom_service.t

  val election_pretty_ballots :
    ( Web_serializable_t.uuid * unit,
      unit,
      Eliom_service.get,
      Eliom_service.att,
      Eliom_service.non_co,
      Eliom_service.non_ext,
      Eliom_service.reg,
      [ `WithSuffix ],
      [ `One of Web_serializable_t.uuid ] Eliom_parameter.param_name
      * [ `One of unit ] Eliom_parameter.param_name,
      unit,
      Eliom_service.non_ocaml )
    Eliom_service.t

  val election_pretty_ballot :
    ( (Web_serializable_t.uuid * unit) * string,
      unit,
      Eliom_service.get,
      Eliom_service.att,
      Eliom_service.non_co,
      Eliom_service.non_ext,
      Eliom_service.reg,
      [ `WithSuffix ],
      ([ `One of Web_serializable_t.uuid ] Eliom_parameter.param_name
      * [ `One of unit ] Eliom_parameter.param_name)
      * [ `One of string ] Eliom_parameter.param_name,
      unit,
      Eliom_service.non_ocaml )
    Eliom_service.t

  val election_pretty_records :
    ( Web_serializable_t.uuid * unit,
      unit,
      Eliom_service.get,
      Eliom_service.att,
      Eliom_service.non_co,
      Eliom_service.non_ext,
      Eliom_service.reg,
      [ `WithSuffix ],
      [ `One of Web_serializable_t.uuid ] Eliom_parameter.param_name
      * [ `One of unit ] Eliom_parameter.param_name,
      unit,
      Eliom_service.non_ocaml )
    Eliom_service.t

  val election_project_result :
    ( (Web_serializable_t.uuid * unit) * int,
      unit,
      Eliom_service.get,
      Eliom_service.att,
      Eliom_service.non_co,
      Eliom_service.non_ext,
      Eliom_service.reg,
      [ `WithSuffix ],
      ([ `One of Web_serializable_t.uuid ] Eliom_parameter.param_name
      * [ `One of unit ] Eliom_parameter.param_name)
      * [ `One of int ] Eliom_parameter.param_name,
      unit,
      Eliom_service.non_ocaml )
    Eliom_service.t

  val election_missing_voters :
    ( Web_serializable_t.uuid * unit,
      unit,
      Eliom_service.get,
      Eliom_service.att,
      Eliom_service.non_co,
      Eliom_service.non_ext,
      Eliom_service.reg,
      [ `WithSuffix ],
      [ `One of Web_serializable_t.uuid ] Eliom_parameter.param_name
      * [ `One of unit ] Eliom_parameter.param_name,
      unit,
      Eliom_service.non_ocaml )
    Eliom_service.t

  val election_download_archive :
    ( Web_serializable_t.uuid * unit,
      unit,
      Eliom_service.get,
      Eliom_service.att,
      Eliom_service.non_co,
      Eliom_service.non_ext,
      Eliom_service.reg,
      [ `WithSuffix ],
      [ `One of Web_serializable_t.uuid ] Eliom_parameter.param_name
      * [ `One of unit ] Eliom_parameter.param_name,
      unit,
      Eliom_service.non_ocaml )
    Eliom_service.t

  val election_compute_encrypted_tally :
    ( Web_serializable_t.uuid,
      unit,
      Eliom_service.post,
      Eliom_service.att,
      Eliom_service.co,
      Eliom_service.non_ext,
      Eliom_service.reg,
      [ `WithoutSuffix ],
      [ `One of Web_serializable_t.uuid ] Eliom_parameter.param_name,
      unit,
      Eliom_service.non_ocaml )
    Eliom_service.t

  val election_nh_ciphertexts :
    ( Web_serializable_t.uuid,
      unit,
      Eliom_service.get,
      Eliom_service.att,
      Eliom_service.non_co,
      Eliom_service.non_ext,
      Eliom_service.reg,
      [ `WithoutSuffix ],
      [ `One of Web_serializable_t.uuid ] Eliom_parameter.param_name,
      unit,
      Eliom_service.non_ocaml )
    Eliom_service.t

  val election_shuffle_link :
    ( Web_serializable_t.uuid * string,
      unit,
      Eliom_service.get,
      Eliom_service.att,
      Eliom_service.non_co,
      Eliom_service.non_ext,
      Eliom_service.reg,
      [ `WithSuffix ],
      [ `One of Web_serializable_t.uuid ] Eliom_parameter.param_name
      * [ `One of string ] Eliom_parameter.param_name,
      unit,
      Eliom_service.non_ocaml )
    Eliom_service.t

  val election_shuffle_link_static :
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

  val election_shuffle_post :
    ( Web_serializable_t.uuid * string,
      string,
      Eliom_service.post,
      Eliom_service.att,
      Eliom_service.non_co,
      Eliom_service.non_ext,
      Eliom_service.reg,
      [ `WithoutSuffix ],
      [ `One of Web_serializable_t.uuid ] Eliom_parameter.param_name
      * [ `One of string ] Eliom_parameter.param_name,
      [ `One of string ] Eliom_parameter.param_name,
      Eliom_service.non_ocaml )
    Eliom_service.t

  val election_shuffler_select :
    ( unit,
      Web_serializable_t.uuid * string,
      Eliom_service.post,
      Eliom_service.non_att,
      Eliom_service.co,
      Eliom_service.non_ext,
      Eliom_service.reg,
      [ `WithoutSuffix ],
      unit,
      [ `One of Web_serializable_t.uuid ] Eliom_parameter.param_name
      * [ `One of string ] Eliom_parameter.param_name,
      Eliom_service.non_ocaml )
    Eliom_service.t

  val election_shuffler_skip_confirm :
    ( unit,
      Web_serializable_t.uuid * string,
      Eliom_service.post,
      Eliom_service.non_att,
      Eliom_service.co,
      Eliom_service.non_ext,
      Eliom_service.reg,
      [ `WithoutSuffix ],
      unit,
      [ `One of Web_serializable_t.uuid ] Eliom_parameter.param_name
      * [ `One of string ] Eliom_parameter.param_name,
      Eliom_service.non_ocaml )
    Eliom_service.t

  val election_shuffler_skip :
    ( unit,
      Web_serializable_t.uuid * string,
      Eliom_service.post,
      Eliom_service.non_att,
      Eliom_service.co,
      Eliom_service.non_ext,
      Eliom_service.reg,
      [ `WithoutSuffix ],
      unit,
      [ `One of Web_serializable_t.uuid ] Eliom_parameter.param_name
      * [ `One of string ] Eliom_parameter.param_name,
      Eliom_service.non_ocaml )
    Eliom_service.t

  val election_decrypt :
    ( Web_serializable_t.uuid,
      unit,
      Eliom_service.post,
      Eliom_service.att,
      Eliom_service.co,
      Eliom_service.non_ext,
      Eliom_service.reg,
      [ `WithoutSuffix ],
      [ `One of Web_serializable_t.uuid ] Eliom_parameter.param_name,
      unit,
      Eliom_service.non_ocaml )
    Eliom_service.t

  val election_tally_trustees :
    ( Web_serializable_t.uuid * string,
      unit,
      Eliom_service.get,
      Eliom_service.att,
      Eliom_service.non_co,
      Eliom_service.non_ext,
      Eliom_service.reg,
      [ `WithSuffix ],
      [ `One of Web_serializable_t.uuid ] Eliom_parameter.param_name
      * [ `One of string ] Eliom_parameter.param_name,
      unit,
      Eliom_service.non_ocaml )
    Eliom_service.t

  val election_tally_trustees_static :
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

  val election_tally_trustees_post :
    ( Web_serializable_t.uuid * string,
      string,
      Eliom_service.post,
      Eliom_service.att,
      Eliom_service.non_co,
      Eliom_service.non_ext,
      Eliom_service.reg,
      [ `WithoutSuffix ],
      [ `One of Web_serializable_t.uuid ] Eliom_parameter.param_name
      * [ `One of string ] Eliom_parameter.param_name,
      [ `One of string ] Eliom_parameter.param_name,
      Eliom_service.non_ocaml )
    Eliom_service.t

  val election_tally_release :
    ( Web_serializable_t.uuid,
      unit,
      Eliom_service.post,
      Eliom_service.att,
      Eliom_service.co,
      Eliom_service.non_ext,
      Eliom_service.reg,
      [ `WithoutSuffix ],
      [ `One of Web_serializable_t.uuid ] Eliom_parameter.param_name,
      unit,
      Eliom_service.non_ocaml )
    Eliom_service.t

  val election_dir :
    ( Web_serializable_t.uuid * Web_common.election_file,
      unit,
      Eliom_service.get,
      Eliom_service.att,
      Eliom_service.non_co,
      Eliom_service.non_ext,
      Eliom_service.reg,
      [ `WithSuffix ],
      [ `One of Web_serializable_t.uuid ] Eliom_parameter.param_name
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
    ( Web_serializable_t.uuid * int,
      unit,
      Eliom_service.get,
      Eliom_service.att,
      Eliom_service.non_co,
      Eliom_service.non_ext,
      Eliom_service.reg,
      [ `WithoutSuffix ],
      [ `One of Web_serializable_t.uuid ] Eliom_parameter.param_name
      * [ `One of int ] Eliom_parameter.param_name,
      unit,
      Eliom_service.non_ocaml )
    Eliom_service.t

  val method_mj :
    ( Web_serializable_t.uuid * (int * int option),
      unit,
      Eliom_service.get,
      Eliom_service.att,
      Eliom_service.non_co,
      Eliom_service.non_ext,
      Eliom_service.reg,
      [ `WithoutSuffix ],
      [ `One of Web_serializable_t.uuid ] Eliom_parameter.param_name
      * ([ `One of int ] Eliom_parameter.param_name
        * [ `One of int ] Eliom_parameter.param_name),
      unit,
      Eliom_service.non_ocaml )
    Eliom_service.t

  val method_stv :
    ( Web_serializable_t.uuid * (int * int option),
      unit,
      Eliom_service.get,
      Eliom_service.att,
      Eliom_service.non_co,
      Eliom_service.non_ext,
      Eliom_service.reg,
      [ `WithoutSuffix ],
      [ `One of Web_serializable_t.uuid ] Eliom_parameter.param_name
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
