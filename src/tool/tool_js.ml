(**************************************************************************)
(*                                BELENIOS                                *)
(*                                                                        *)
(*  Copyright © 2012-2014 Inria                                           *)
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

open Platform

let document = Dom_html.window##document

let alert (s : Js.js_string Js.t) : unit =
  Js.Unsafe.fun_call (Js.Unsafe.variable "alert") [| Js.Unsafe.inject s |]

let install_handler (id, handler) =
  let f _ =
    begin try handler () with e ->
      let msg = "Unexpected error: " ^ Printexc.to_string e in
      alert (Js.string msg)
    end;
    Js._false
  in
  Js.Opt.iter
    (document##getElementById (Js.string id))
    (fun e -> e##onclick <- Dom_html.handler f)

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

module Calc = struct

  let add () =
    let a = Z.of_string (get_textarea "calc_a") in
    let b = Z.of_string (get_textarea "calc_b") in
    set_textarea "calc_r" Z.(to_string (a + b))

  let mul () =
    let a = Z.of_string (get_textarea "calc_a") in
    let b = Z.of_string (get_textarea "calc_b") in
    set_textarea "calc_r" Z.(to_string (a * b))

  let sub () =
    let a = Z.of_string (get_textarea "calc_a") in
    let b = Z.of_string (get_textarea "calc_b") in
    set_textarea "calc_r" Z.(to_string (a - b))

  let cmds = [
    "do_add", add;
    "do_mul", mul;
    "do_sub", sub;
  ]
end

module Tkeygen = struct
  open Tool_tkeygen

  let tkeygen () =
    let module P : PARAMS = struct
      let group = get_textarea "tkeygen_group"
    end in
    let module X = (val make (module P : PARAMS) : S) in
    ignore (X.trustee_keygen ())

  let cmds = ["do_tkeygen", tkeygen]
end

let cmds = Calc.cmds @ Tkeygen.cmds

let install_handlers () =
  List.iter install_handler cmds

let () =
  Dom_html.window##onload <- Dom_html.handler (fun _ ->
    install_handlers ();
    Js._false
  )
