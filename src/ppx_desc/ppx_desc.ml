(**************************************************************************)
(*                                BELENIOS                                *)
(*                                                                        *)
(*  Copyright © 2026-2026 Inria                                           *)
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

(* [[@@deriving desc]]: for a type [('a, 'b) foo], generates

   - [foo_d : 'a Desc.Core.t -> 'b Desc.Core.t -> ('a, 'b) foo Desc.Core.t];
   - during the transition from ppx_yojson_conv, [yojson_of_foo] and
     [foo_of_yojson], with the same types as those of ppx_yojson_conv.

   A type [M.bar] is described by [M.bar_d]; [int], [int64], [string],
   [bool], [unit], [list], [array] and [option] by the corresponding
   functions of [Desc.Core]. Record fields of type [_ option] are
   optional (left out when [None]). Attributes on record fields: [[@key
   "name"]] (JSON key), [[@default expr]] (left out when equal to [expr]). *)

open Ppxlib
open Ast_builder.Default

let desc_name name = name ^ "_d"
let param_var v = "_desc_" ^ v
let self_var = "_desc_self"

let key_attr =
  Attribute.declare "desc.key" Attribute.Context.label_declaration
    Ast_pattern.(single_expr_payload (estring __))
    (fun x -> x)

let default_attr =
  Attribute.declare "desc.default" Attribute.Context.label_declaration
    Ast_pattern.(single_expr_payload __)
    (fun x -> x)

let map_lid ~loc f = function
  | Lident n -> Lident (f n)
  | Ldot (p, n) -> Ldot (p, f n)
  | Lapply _ -> Location.raise_errorf ~loc "ppx_desc: functor application"

(* Type parameters, named; anonymous ones get a name from their position. *)
let param_names td =
  List.mapi
    (fun i (ty, _) ->
      match ty.ptyp_desc with
      | Ptyp_var v -> v
      | Ptyp_any -> Printf.sprintf "_p%d" i
      | _ ->
          Location.raise_errorf ~loc:ty.ptyp_loc
            "ppx_desc: unexpected type parameter")
    td.ptype_params

(* The declared type, with its parameters as variables or wildcards. *)
let self_type ~loc ?(vars = true) td =
  ptyp_constr ~loc
    { txt = Lident td.ptype_name.txt; loc }
    (List.map
       (fun v -> if vars then ptyp_var ~loc v else ptyp_any ~loc)
       (param_names td))

type ctx = {
  self : (string * string list) option;
      (** Name and parameters of the type being defined, if recursive. *)
  others : string list;  (** Other types of the same recursive group. *)
}

let fresh prefix n = List.init n (fun i -> Printf.sprintf "%s%d" prefix i)

let rec list_expr ~loc = function
  | [] -> [%expr []]
  | x :: xs -> [%expr [%e x] :: [%e list_expr ~loc xs]]

let rec expr_of_type ctx ty =
  let loc = ty.ptyp_loc in
  match ty.ptyp_desc with
  | Ptyp_var v -> evar ~loc (param_var v)
  | Ptyp_constr
      ( { txt = Lident (("int" | "int64" | "string" | "bool" | "unit") as n); _ },
        [] ) ->
      evar ~loc ("Desc.Core." ^ n)
  | Ptyp_constr ({ txt = Lident (("list" | "array" | "option") as n); _ }, [ a ])
    ->
      [%expr [%e evar ~loc ("Desc.Core." ^ n)] [%e expr_of_type ctx a]]
  | Ptyp_constr ({ txt = Lident n; _ }, args)
    when match ctx.self with Some (s, _) -> s = n | None -> false ->
      let params = match ctx.self with Some (_, p) -> p | None -> [] in
      let same =
        List.length args = List.length params
        && List.for_all2
             (fun a p ->
               match a.ptyp_desc with Ptyp_var v -> v = p | _ -> false)
             args params
      in
      if not same then
        Location.raise_errorf ~loc "ppx_desc: polymorphic recursion";
      evar ~loc self_var
  | Ptyp_constr ({ txt = Lident n; _ }, _) when List.mem n ctx.others ->
      Location.raise_errorf ~loc "ppx_desc: mutually recursive types"
  | Ptyp_constr ({ txt = lid; loc = lid_loc }, args) -> (
      let f =
        pexp_ident ~loc { txt = map_lid ~loc desc_name lid; loc = lid_loc }
      in
      match args with
      | [] -> f
      | _ -> eapply ~loc f (List.map (expr_of_type ctx) args))
  | Ptyp_tuple ts -> tuple_expr ctx ~loc ts
  | Ptyp_variant (rows, Closed, None) -> poly_variant_expr ctx ~loc rows
  | _ -> Location.raise_errorf ~loc "ppx_desc: unsupported type"

and tuple_expr ctx ~loc ts =
  let xs = fresh "x" (List.length ts) in
  let elts =
    List.fold_right2
      (fun ty x acc ->
        let get =
          pexp_fun ~loc Nolabel None
            (ppat_tuple ~loc
               (List.map
                  (fun y -> if y = x then pvar ~loc y else ppat_any ~loc)
                  xs))
            (evar ~loc x)
        in
        [%expr
          Desc.Core.Cons
            ( { Desc.Core.ty = [%e expr_of_type ctx ty]; get = [%e get] },
              [%e acc] )])
      ts xs [%expr Desc.Core.Nil]
  in
  let make =
    List.fold_right
      (fun x acc -> pexp_fun ~loc Nolabel None (pvar ~loc x) acc)
      xs
      (pexp_tuple ~loc (List.map (evar ~loc) xs))
  in
  [%expr Desc.Core.tuple [%e elts] [%e make]]

(* Cases of a variant: [(tag, argument type option, inj, pattern, value)].
   [inj] builds the value from the case argument, [pattern] matches it and
   binds [x], or [x0]…[xn] for several arguments, [value] is the case
   argument rebuilt from those variables. *)
and variant_expr ~loc ~annot cases =
  let names = fresh "_case" (List.length cases) in
  let bindings =
    List.map2
      (fun name (tag, arg, inj, _, _) ->
        let e =
          match arg with
          | None -> [%expr Desc.Core.const [%e estring ~loc tag] [%e inj]]
          | Some d ->
              [%expr Desc.Core.case [%e estring ~loc tag] [%e d] [%e inj]]
        in
        value_binding ~loc ~pat:(pvar ~loc name) ~expr:e)
      names cases
  in
  let anys =
    list_expr ~loc
      (List.map (fun n -> [%expr Desc.Core.Any [%e evar ~loc n]]) names)
  in
  let classify =
    pexp_function_cases ~loc
      (List.map2
         (fun name (_, _, _, pat, value) ->
           case
             ~lhs:(ppat_constraint ~loc pat annot)
             ~guard:None
             ~rhs:[%expr Desc.Core.Value ([%e evar ~loc name], [%e value])])
         names cases)
  in
  pexp_let ~loc Nonrecursive bindings
    [%expr Desc.Core.variant [%e anys] [%e classify]]

and poly_variant_expr ctx ~loc rows =
  let cases =
    List.map
      (fun row ->
        let loc = row.prf_loc in
        match row.prf_desc with
        | Rtag ({ txt = tag; _ }, true, []) ->
            let v = pexp_variant ~loc tag None in
            (tag, None, v, ppat_variant ~loc tag None, [%expr ()])
        | Rtag ({ txt = tag; _ }, false, [ ty ]) ->
            ( tag,
              Some (expr_of_type ctx ty),
              [%expr fun x -> [%e pexp_variant ~loc tag (Some [%expr x])]],
              ppat_variant ~loc tag (Some [%pat? x]),
              [%expr x] )
        | _ -> Location.raise_errorf ~loc "ppx_desc: unsupported variant row")
      rows
  in
  variant_expr ~loc ~annot:(ptyp_any ~loc) cases

(* The fields of a record. [get ld] reads field [ld] from the value
   described, [make vars] builds it from variables holding the fields. *)
and fields_expr ctx ~loc ~get ~make labels =
  let fields =
    List.fold_right
      (fun ld acc ->
        let loc = ld.pld_loc in
        let key =
          match Attribute.get key_attr ld with
          | Some k -> k
          | None -> ld.pld_name.txt
        in
        let get = get ld in
        let key = estring ~loc key in
        let field =
          match (Attribute.get default_attr ld, ld.pld_type.ptyp_desc) with
          | Some default, _ ->
              [%expr
                Desc.Core.default [%e key]
                  [%e expr_of_type ctx ld.pld_type]
                  [%e default] [%e get]]
          | None, Ptyp_constr ({ txt = Lident "option"; _ }, [ ty ]) ->
              [%expr
                Desc.Core.optional [%e key] [%e expr_of_type ctx ty] [%e get]]
          | None, _ ->
              [%expr
                Desc.Core.field [%e key]
                  [%e expr_of_type ctx ld.pld_type]
                  [%e get]]
        in
        [%expr Desc.Core.Fcons ([%e field], [%e acc])])
      labels [%expr Desc.Core.Fnil]
  in
  let xs = List.map (fun ld -> "f_" ^ ld.pld_name.txt) labels in
  let make =
    List.fold_right
      (fun x acc -> pexp_fun ~loc Nolabel None (pvar ~loc x) acc)
      xs (make xs)
  in
  [%expr Desc.Core.record [%e fields] [%e make]]

(* A record built from labels and variables. *)
and record_of_vars ~loc labels xs =
  pexp_record ~loc
    (List.map2
       (fun ld x -> ({ txt = Lident ld.pld_name.txt; loc }, evar ~loc x))
       labels xs)
    None

let record_expr ctx ~loc td labels =
  let annot = self_type ~loc ~vars:false td in
  let get ld =
    [%expr
      fun (r : [%t annot]) ->
        [%e pexp_field ~loc [%expr r] { txt = Lident ld.pld_name.txt; loc }]]
  in
  let make xs = pexp_constraint ~loc (record_of_vars ~loc labels xs) annot in
  fields_expr ctx ~loc ~get ~make labels

let regular_variant_expr ctx ~loc td constrs =
  let annot = self_type ~loc ~vars:false td in
  let cases =
    List.map
      (fun cd ->
        let loc = cd.pcd_loc in
        if cd.pcd_res <> None then
          Location.raise_errorf ~loc "ppx_desc: GADT constructors";
        let tag = cd.pcd_name.txt in
        let lid = { txt = Lident tag; loc } in
        let constr e = pexp_constraint ~loc (pexp_construct ~loc lid e) annot in
        match cd.pcd_args with
        | Pcstr_record labels ->
            (* Described as a record over the tuple of its fields. *)
            let xs = List.map (fun ld -> "f_" ^ ld.pld_name.txt) labels in
            let tuple_of es =
              match es with [ e ] -> e | _ -> pexp_tuple ~loc es
            in
            let ptuple_of ps =
              match ps with [ p ] -> p | _ -> ppat_tuple ~loc ps
            in
            let get ld =
              let pats =
                List.map
                  (fun l -> if l == ld then pvar ~loc "x" else ppat_any ~loc)
                  labels
              in
              pexp_fun ~loc Nolabel None (ptuple_of pats) [%expr x]
            in
            let make vars = tuple_of (List.map (evar ~loc) vars) in
            let p = ptuple_of (List.map (pvar ~loc) xs) in
            ( tag,
              Some (fields_expr ctx ~loc ~get ~make labels),
              pexp_fun ~loc Nolabel None p
                (constr (Some (record_of_vars ~loc labels xs))),
              ppat_construct ~loc lid
                (Some
                   (ppat_record ~loc
                      (List.map2
                         (fun ld x ->
                           ({ txt = Lident ld.pld_name.txt; loc }, pvar ~loc x))
                         labels xs)
                      Closed)),
              make xs )
        | Pcstr_tuple [] ->
            (tag, None, constr None, ppat_construct ~loc lid None, [%expr ()])
        | Pcstr_tuple [ ty ] ->
            ( tag,
              Some (expr_of_type ctx ty),
              [%expr fun x -> [%e constr (Some [%expr x])]],
              ppat_construct ~loc lid (Some [%pat? x]),
              [%expr x] )
        | Pcstr_tuple tys ->
            let xs = fresh "x" (List.length tys) in
            let e = pexp_tuple ~loc (List.map (evar ~loc) xs) in
            let p = ppat_tuple ~loc (List.map (pvar ~loc) xs) in
            ( tag,
              Some (tuple_expr ctx ~loc tys),
              pexp_fun ~loc Nolabel None p (constr (Some e)),
              ppat_construct ~loc lid (Some p),
              e ))
      constrs
  in
  variant_expr ~loc ~annot cases

let mentions name td =
  let found = ref false in
  let it =
    object
      inherit Ast_traverse.iter as super

      method! core_type ty =
        (match ty.ptyp_desc with
        | Ptyp_constr ({ txt = Lident n; _ }, _) when n = name -> found := true
        | _ -> ());
        super#core_type ty
    end
  in
  it#type_declaration td;
  !found

let str_of_td ~rec_flag ~group td =
  let loc = td.ptype_loc in
  let name = td.ptype_name.txt in
  let params = param_names td in
  let recursive = rec_flag = Recursive && mentions name td in
  let others =
    if rec_flag = Recursive then
      List.filter
        (fun n -> n <> name)
        (List.map (fun td -> td.ptype_name.txt) group)
    else []
  in
  let ctx =
    { self = (if recursive then Some (name, params) else None); others }
  in
  let body =
    match (td.ptype_kind, td.ptype_manifest) with
    | Ptype_record labels, _ -> record_expr ctx ~loc td labels
    | Ptype_variant constrs, _ -> regular_variant_expr ctx ~loc td constrs
    | Ptype_abstract, Some ty -> expr_of_type ctx ty
    | Ptype_abstract, None ->
        Location.raise_errorf ~loc "ppx_desc: abstract type"
    | Ptype_open, _ -> Location.raise_errorf ~loc "ppx_desc: open type"
  in
  let body = [%expr ([%e body] : [%t self_type ~loc td] Desc.Core.t)] in
  let body =
    if recursive then
      [%expr
        Desc.Core.fix [%e estring ~loc name] (fun [%p pvar ~loc self_var] ->
            [%e body])]
    else body
  in
  let fun_of prefix body =
    List.fold_right
      (fun p acc -> pexp_fun ~loc Nolabel None (pvar ~loc (prefix ^ p)) acc)
      params body
  in
  let desc = fun_of "_desc_" body in
  let applied conv =
    eapply ~loc
      (evar ~loc (desc_name name))
      (List.map
         (fun p -> [%expr [%e evar ~loc conv] [%e evar ~loc ("_f_" ^ p)]])
         params)
  in
  let applied conv =
    if params = [] then evar ~loc (desc_name name) else applied conv
  in
  let to_json =
    fun_of "_f_"
      [%expr fun x -> Desc.Json.to_json [%e applied "Desc.Json.of_encoder"] x]
  in
  let of_json =
    fun_of "_f_"
      [%expr fun x -> Desc.Json.of_json [%e applied "Desc.Json.of_decoder"] x]
  in
  let bind n e =
    pstr_value ~loc Nonrecursive
      [ value_binding ~loc ~pat:(pvar ~loc n) ~expr:e ]
  in
  [
    bind (desc_name name) desc;
    bind ("yojson_of_" ^ name) to_json;
    bind (name ^ "_of_yojson") of_json;
  ]

let str_type_decl ~ctxt:_ (rec_flag, tds) =
  let rec_flag = really_recursive rec_flag tds in
  List.concat_map (str_of_td ~rec_flag ~group:tds) tds

let sig_of_td td =
  let loc = td.ptype_loc in
  let name = td.ptype_name.txt in
  let params = param_names td in
  let t = self_type ~loc td in
  let arrows args res =
    List.fold_right (fun a acc -> ptyp_arrow ~loc Nolabel a acc) args res
  in
  let var p = ptyp_var ~loc p in
  let json = [%type: Desc.Core.json] in
  let v n ty =
    psig_value ~loc
      (value_description ~loc ~name:{ txt = n; loc } ~type_:ty ~prim:[])
  in
  [
    v (desc_name name)
      (arrows
         (List.map (fun p -> [%type: [%t var p] Desc.Core.t]) params)
         [%type: [%t t] Desc.Core.t]);
    v ("yojson_of_" ^ name)
      (arrows
         (List.map (fun p -> [%type: [%t var p] -> [%t json]]) params)
         [%type: [%t t] -> [%t json]]);
    v (name ^ "_of_yojson")
      (arrows
         (List.map (fun p -> [%type: [%t json] -> [%t var p]]) params)
         [%type: [%t json] -> [%t t]]);
  ]

let sig_type_decl ~ctxt:_ (_rec_flag, tds) = List.concat_map sig_of_td tds

let () =
  Deriving.add "desc"
    ~str_type_decl:(Deriving.Generator.V2.make_noarg str_type_decl)
    ~sig_type_decl:(Deriving.Generator.V2.make_noarg sig_type_decl)
  |> Deriving.ignore
