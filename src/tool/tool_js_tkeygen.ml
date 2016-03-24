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

open Tool_js_common
open Tool_tkeygen

let tkeygen _ =
  let module P : PARAMS = struct
    let group = get_textarea "group"
  end in
  let module X = (val make (module P : PARAMS) : S) in
  let open X in
  let {id; priv; pub} = trustee_keygen () in
  let data_uri = (Js.string "data:application/json,")##concat (Js.encodeURI (Js.string priv)) in
  ignore (Dom_html.window##open_ (data_uri, Js.string id, Js.null));
  set_textarea "pk" pub;
  alert "The private key has been open in a new window (or tab). Please save it before submitting the public key!";
  Js._false

let fill_interactivity _ =
  Js.Opt.iter
    (document##getElementById (Js.string "interactivity"))
    (fun e ->
     let b = document##createElement (Js.string "button") in
     let t = document##createTextNode (Js.string "Generate a new keypair") in
     b##onclick <- Dom_html.handler tkeygen;
     Dom.appendChild b t;
     Dom.appendChild e b;
    );
  Js._false

let () =
  Dom_html.window##onload <- Dom_html.handler fill_interactivity;
