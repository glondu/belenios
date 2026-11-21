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

(* Foo.Bar.my_type -> Foo.Bar.my_type_d *)
let derive_desc_lid lid =
  match lid with
  | Lident name -> Lident (name ^ "_d")
  | Ldot (path, name) -> Ldot (path, name ^ "_d")
  | Lapply _ ->
      failwith "[%desc_of_group] functor application in type path not supported"

(* Shared: parse (w : _ t) and call [derive_lid] and [make_args] to build the expansion *)
let expand_witness ~derive_lid ~make_args ~ctxt ty =
  let loc = Expansion_context.Extension.extension_point_loc ctxt in
  match extract_type_constructor ~loc ty with
  | Error ext -> pexp_extension ~loc ext
  | Ok (lid, lid_loc) ->
      let fn_lid = { txt = derive_lid lid; loc = lid_loc } in
      pexp_apply ~loc (pexp_ident ~loc fn_lid) (make_args ~loc)

let desc_of_witness_extension =
  Extension.V3.declare "desc_of_group" Extension.Context.expression
    Ast_pattern.(ptyp __)
    (expand_witness ~derive_lid:derive_desc_lid ~make_args:(fun ~loc ->
         [ (Nolabel, [%expr G.t_d]); (Nolabel, [%expr G.Zq.t_d]) ]))

let () =
  Driver.register_transformation
    ~extensions:[ desc_of_witness_extension ]
    "ppx_group_witness"
