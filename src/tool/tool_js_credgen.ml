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
open Tool_credgen

let generate _ =
  let ids =
    let raw = get_textarea "voters" in
    let rec loop i accu =
      if i >= 0 then
        let j = try String.rindex_from raw i '\n' with Not_found -> -1 in
        loop (j-1) (String.sub raw (j+1) (i-j) :: accu)
      else
        accu
    in loop (String.length raw - 1) []
  in
  let module P : PARAMS = struct
    let uuid = get_textarea "uuid"
    let group = get_textarea "group"
  end in
  let module X = (val make (module P : PARAMS) : S) in
  let privs, pubs, hashs =
    List.fold_left
      (fun (privs, pubs, hashs) id ->
       let priv, pub, hash = X.generate () in
       let priv = id ^ " " ^ priv and hash = id ^ " " ^ hash in
       priv::privs, pub::pubs, hash::hashs
      ) ([], [], []) ids
  in
  let text_pks = pubs |> List.sort compare |> String.concat "\n" in
  set_textarea "pks" text_pks;
  let text_creds = (privs |> List.rev |> String.concat "\n") ^ "\n" in
  let data_creds = (Js.string "data:text/plain,")##concat (Js.encodeURI (Js.string text_creds)) in
  ignore (Dom_html.window##open_ (data_creds, Js.string "creds", Js.null));
  let text_hashed = (hashs |> List.rev |> String.concat "\n") ^ "\n" in
  let data_hashed = (Js.string "data:text/plain,")##concat (Js.encodeURI (Js.string text_hashed)) in
  ignore (Dom_html.window##open_ (data_hashed, Js.string "hashed", Js.null));
  alert "New windows (or tabs) were open with private credentials and credential hashes. Please save them before submitting public credentials!";
  Js._false

let fill_interactivity _ =
  Js.Opt.iter
    (document##getElementById (Js.string "interactivity"))
    (fun e ->
     let x = document##createElement (Js.string "div") in
     Dom.appendChild e x;
     let b = document##createElement (Js.string "button") in
     let t = document##createTextNode (Js.string "Generate") in
     b##onclick <- Dom_html.handler generate;
     Dom.appendChild b t;
     Dom.appendChild x b;
    );
  Js._false

let () =
  Dom_html.window##onload <- Dom_html.handler fill_interactivity;
