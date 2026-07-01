(**************************************************************************)
(*                                BELENIOS                                *)
(*                                                                        *)
(*  Copyright © 2026 VCAST                                                *)
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

open Ppxlib
open Ast_builder.Default

(* Extract the type constructor lid from a core_type *)
let extract_type_constructor ~loc ty =
  match ty.ptyp_desc with
  | Ptyp_constr ({ txt = lid; loc = lid_loc }, _args) -> Ok (lid, lid_loc)
  | _ ->
      Error
        (Location.error_extensionf ~loc
           "expected a type constructor, e.g. (w : _ Foo.Bar.my_type)")

(* Foo.Bar.my_type -> Foo.Bar.my_type_of_yojson *)
let derive_of_yojson_lid lid =
  match lid with
  | Lident name -> Lident (name ^ "_of_yojson")
  | Ldot (path, name) -> Ldot (path, name ^ "_of_yojson")
  | Lapply _ ->
      failwith
        "[%group_of_yojson] functor application in type path not supported"

(* Foo.Bar.my_type -> Foo.Bar.yojson_of_my_type *)
let derive_yojson_of_lid lid =
  match lid with
  | Lident name -> Lident ("yojson_of_" ^ name)
  | Ldot (path, name) -> Ldot (path, "yojson_of_" ^ name)
  | Lapply _ ->
      failwith
        "[%yojson_of_group] functor application in type path not supported"

(* Shared: parse (w : _ t) and call [derive_lid] and [make_args] to build the expansion *)
let expand_witness ~derive_lid ~make_args ~ctxt ty =
  let loc = Expansion_context.Extension.extension_point_loc ctxt in
  match extract_type_constructor ~loc ty with
  | Error ext -> pexp_extension ~loc ext
  | Ok (lid, lid_loc) ->
      let fn_lid = { txt = derive_lid lid; loc = lid_loc } in
      pexp_apply ~loc (pexp_ident ~loc fn_lid) (make_args ~loc)

let witness_of_yojson_extension =
  Extension.V3.declare "group_of_yojson" Extension.Context.expression
    Ast_pattern.(ptyp __)
    (expand_witness ~derive_lid:derive_of_yojson_lid ~make_args:(fun ~loc ->
         [
           (Nolabel, [%expr !$G.of_string]); (Nolabel, [%expr !$G.Zq.of_string]);
         ]))

let yojson_of_witness_extension =
  Extension.V3.declare "yojson_of_group" Extension.Context.expression
    Ast_pattern.(ptyp __)
    (expand_witness ~derive_lid:derive_yojson_of_lid ~make_args:(fun ~loc ->
         [
           (Nolabel, [%expr !&G.to_string]); (Nolabel, [%expr !&G.Zq.to_string]);
         ]))

let () =
  Driver.register_transformation
    ~extensions:[ witness_of_yojson_extension; yojson_of_witness_extension ]
    "ppx_group_witness"
