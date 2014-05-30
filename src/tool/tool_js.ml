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

let alert s =
  let open Js.Unsafe in
  fun_call (variable "alert") [| s |> Js.string |> inject |]

let install_handler (id, handler) =
  let f _ =
    begin try handler () with e ->
      let msg = "Unexpected error: " ^ Printexc.to_string e in
      alert msg
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

module Tests = struct

  let unit_tests () =
    let a = "13133254971699857128" and b = "31748915560162976106" in
    let c = Z.of_string a and d = Z.of_string b in
    let ntests = ref 0 in
    let check name f =
      if not (f ()) then Printf.ksprintf failwith "test %s failed" name;
      incr ntests
    in
    check "ZERO" (fun () -> Z.to_string Z.zero = "0");
    check "ONE" (fun () -> Z.to_string Z.one = "1");
    let string_roundtrip a c () = a = Z.to_string c in
    check "string_roundtrip_a" (string_roundtrip a c);
    check "string_roundtrip_b" (string_roundtrip b d);
    let operator op expected () = expected = Z.to_string (op c d) in
    check "add" (operator Z.( + ) "44882170531862833234");
    check "mul" (operator Z.( * ) "416966603126589360375328894595477783568");
    check "sub" (operator Z.( - ) "-18615660588463118978");
    let a = 132180439 and b = 41907500 in
    let c = Z.of_int a and d = Z.of_int b in
    let int_roundtrip a c () = a = Z.to_int c in
    check "int_roundtrip_a" (int_roundtrip a c);
    check "int_roundtrip_b" (int_roundtrip b d);
    let m = Z.of_int 181944121 in
    check "mod" (fun () -> Z.to_int Z.((c * d) mod m) = 30881634);
    check "erem" (fun () -> Z.to_int Z.((zero - c * d) mod m) = 151062487);
    check "powm" (fun () -> Z.to_int (Z.powm c d m) = 81171525);
    check "invert" (fun () -> Z.to_int (Z.invert c m) = 54455411);
    check "prime" (fun () -> Z.probab_prime m 5 > 0);
    check "eq" (fun () -> Z.(c =% c));
    check "neq" (fun () -> Z.(not (c =% d)));
    check "geq" (fun () -> Z.geq c d);
    check "lt" (fun () -> Z.lt d c);
    Printf.ksprintf alert "%d tests were successful!" !ntests

  let cmds = ["do_unit_tests", unit_tests]
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

let cmds = Tests.cmds @ Tkeygen.cmds

let install_handlers () =
  List.iter install_handler cmds

let () =
  Dom_html.window##onload <- Dom_html.handler (fun _ ->
    install_handlers ();
    Js._false
  )
