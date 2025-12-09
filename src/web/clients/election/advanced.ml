(**************************************************************************)
(*                                BELENIOS                                *)
(*                                                                        *)
(*  Copyright © 2024-2024 Inria                                           *)
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

open Lwt.Syntax
open Js_of_ocaml
open Js_of_ocaml_lwt
open Js_of_ocaml_tyxml
open Tyxml_js.Html
open Belenios
open Belenios_js.Common
open Common

let booths = [ ("Version 2", "vote") ]
let global_ballot = ref None
let global_result = ref None

let post_ballot uuid ~get_ballot _ =
  global_ballot := Some (get_ballot ());
  Dom_html.window##.location##.href
  := Js.string @@ Printf.sprintf "#%s/advanced/submit" (Uuid.unwrap uuid);
  false

let advanced uuid =
  global_ballot := None;
  global_result := None;
  let open (val !Belenios_js.I18n.gettext) in
  let@ election cont =
    let* x = get_election uuid in
    match x with
    | None -> Lwt.return @@ error "Could not get election parameters!"
    | Some x -> cont x
  in
  let module W = (val election) in
  let title = W.template.t_name ^^^ s_ "Advanced mode" in
  let footer = [ make_audit_footer election ] in
  let form_rawballot =
    let t, get_ballot =
      textarea ~rows:10 ~cols:40
        ~placeholder:(s_ "Encrypted ballot in JSON format")
        ""
    in
    let b =
      let handler = post_ballot uuid ~get_ballot in
      Tyxml_js.Html.button ~a:[ a_onclick handler ] [ txt @@ s_ "Submit" ]
    in
    div
      [
        div
          [
            txt
            @@ s_
                 "Please paste your encrypted ballot in JSON format in the \
                  following box:";
          ];
        div [ t ];
        div [ b ];
      ]
  in
  let form_upload =
    let t = Tyxml_js.Html.input ~a:[ a_input_type `File ] () in
    let ballot = ref "" in
    let () =
      let dom = Tyxml_js.To_dom.of_input t in
      let onchange _ =
        let ( let& ) x f = Js.Opt.case x (fun () -> Js._false) f in
        let ( let$ ) x f = match x with None -> Js._false | Some x -> f x in
        let$ file = Belenios_js.Compat.get_file dom in
        let reader = new%js File.fileReader in
        let () =
          reader##.onload :=
            let@ _ = Dom.handler in
            let& content = File.CoerceTo.string reader##.result in
            ballot := Js.to_string content;
            Js._false
        in
        reader##readAsText file;
        Js._false
      in
      dom##.onchange := Dom_html.handler onchange
    in
    let b =
      let handler = post_ballot uuid ~get_ballot:(fun () -> !ballot) in
      Tyxml_js.Html.button ~a:[ a_onclick handler ] [ txt @@ s_ "Submit" ]
    in
    div
      [
        div
          [
            txt
            @@ s_
                 "Alternatively, you can also upload a file containing your \
                  ballot:";
          ];
        div [ txt @@ s_ "File:"; txt " "; t ];
        div [ b ];
      ]
  in
  let booths =
    let fragment =
      Url.encode_arguments [ ("uuid", Uuid.unwrap uuid); ("lang", lang) ]
    in
    let make uri =
      let href = Printf.sprintf "%s#%s" uri fragment in
      a ~href (s_ "direct link")
    in
    booths
    |> List.map (fun (name, base) ->
           let href = !!base in
           li [ a ~href name; txt " ("; make href; txt ")" ])
  in
  let intro =
    div
      [
        div
          [
            txt
            @@ s_
                 "You can create an encrypted ballot by using the command-line \
                  tool ";
            txt @@ s_ "(available in the ";
            a ~href:!!"belenios.tar.gz" (s_ "sources");
            txt @@ s_ "), or any compatible booth.";
            txt " ";
            txt
            @@ s_
                 "A specification of encrypted ballots is also available in \
                  the sources.";
          ];
        div [ txt @@ s_ "Booths available on this server:"; ul booths ];
        div
          [
            a
              ~href:(Printf.sprintf "#%s" (Uuid.unwrap uuid))
              (s_ "Back to election home");
          ];
      ]
  in
  let contents =
    [
      intro;
      h3 [ txt @@ s_ "Submit by copy/paste" ];
      form_rawballot;
      h3 [ txt @@ s_ "Submit by file" ];
      form_upload;
    ]
  in
  Lwt.return { title; contents; footer }

let process_result configuration election container result =
  global_result := Some result;
  container##.innerHTML := Js.string "";
  List.iter
    (fun x -> Dom.appendChild container (Tyxml_js.To_dom.of_node x))
    (Belenios_js.Cast.confirmation configuration election result);
  Lwt.return_unit

let submit configuration uuid =
  let open (val !Belenios_js.I18n.gettext) in
  let@ election cont =
    let* x = get_election uuid in
    match x with
    | None -> Lwt.return @@ error "Could not get election parameters!"
    | Some x -> cont x
  in
  let module W = (val election) in
  let title = W.template.t_name ^^^ s_ "Processing ballot" in
  let footer = [ make_audit_footer election ] in
  let container = div [] in
  let container_dom = Tyxml_js.To_dom.of_div container in
  let container_contents =
    match !global_result with
    | None -> (
        match !global_ballot with
        | None ->
            Dom_html.window##.location##replace
              (Js.string @@ Printf.sprintf "#%s/advanced" (Uuid.unwrap uuid));
            []
        | Some ballot ->
            let handler _ =
              let@ () = finally false in
              let@ () = Lwt.async in
              let* x = Belenios_js.Cast.post_ballot uuid ~ballot in
              match x with
              | Ok state ->
                  let target = make_login_target ~state in
                  Dom_html.window##.location##.href := Js.string target;
                  Lwt.return_unit
              | Error e ->
                  process_result configuration election container_dom (`Error e)
            in
            [
              div
                [
                  txt
                  @@ Printf.sprintf
                       (f_
                          "You are about to submit a ballot with smart ballot \
                           tracker %s.")
                       (sha256_b64 ballot);
                ];
              div
                [
                  Tyxml_js.Html.button
                    ~a:[ a_onclick handler ]
                    [ txt @@ s_ "Proceed" ];
                ];
            ])
    | Some result -> Belenios_js.Cast.confirmation configuration election result
  in
  List.iter
    (fun x -> Dom.appendChild container_dom (Tyxml_js.To_dom.of_node x))
    container_contents;
  let contents = [ container ] in
  Lwt.return { title; contents; footer }
