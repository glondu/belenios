(**************************************************************************)
(*                                BELENIOS                                *)
(*                                                                        *)
(*  Copyright © 2012-2016 Inria                                           *)
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

let document = Dom_html.window##document

let alert s : unit =
  let open Js.Unsafe in
  fun_call (variable "alert") [| s |> Js.string |> inject |]

let get_textarea id =
  let res = ref None in
  Js.Opt.iter
    (document##getElementById (Js.string id))
    (fun e ->
      Js.Opt.iter
        (Dom_html.CoerceTo.textarea e)
        (fun x -> res := Some (Js.to_string (x##value)))
    );
  match !res with
  | None -> raise Not_found
  | Some x -> x

let set_textarea id z =
  Js.Opt.iter
    (document##getElementById (Js.string id))
    (fun e ->
      Js.Opt.iter
        (Dom_html.CoerceTo.textarea e)
        (fun x -> x##value <- Js.string z)
    )

let get_input id =
  let res = ref None in
  Js.Opt.iter
    (document##getElementById (Js.string id))
    (fun e ->
      Js.Opt.iter
        (Dom_html.CoerceTo.input e)
        (fun x -> res := Some (Js.to_string (x##value)))
    );
  match !res with
  | None -> raise Not_found
  | Some x -> x
