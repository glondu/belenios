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
open Belenios_js.Session
open Common

let booths = [ ("Version 2", "static/frontend/booth/vote.html") ]

let check_origin =
  let open Regexp in
  let rex = regexp "^(https?://[^/]+)(/.*)?$" in
  let prefix =
    match string_match rex (Js.to_string window##.location##.href) 0 with
    | None -> ""
    | Some m -> ( match matched_group m 1 with None -> "" | Some x -> x)
  in
  fun x -> String.starts_with ~prefix (Js.to_string x##.origin)

let targetOrigin =
  match
    String.split_on_char '#' (Js.to_string Dom_html.window##.location##.href)
  with
  | x :: _ -> Js.string x
  | [] -> Js.string "*"

module Message = struct
  class type message = object
    method ballot : Js.js_string Js.t Js.optdef Js.readonly_prop
    method ready : bool Js.t Js.optdef Js.readonly_prop
  end

  class type wrapped_message = object
    method belenios : message Js.t Js.optdef Js.readonly_prop
  end

  let postBallot (window : window Js.t) ~ballot =
    let message : message Js.t =
      object%js
        val ballot = Js.Optdef.return (Js.string ballot)
        val ready = Js.undefined
      end
    in
    let wrapped_message : wrapped_message Js.t =
      object%js
        val belenios = Js.Optdef.return message
      end
    in
    window##postMessage wrapped_message targetOrigin

  let postReady (window : window Js.t) () =
    let message : message Js.t =
      object%js
        val ballot = Js.undefined
        val ready = Js.Optdef.return Js._true
      end
    in
    let wrapped_message : wrapped_message Js.t =
      object%js
        val belenios = Js.Optdef.return message
      end
    in
    window##postMessage wrapped_message targetOrigin

  let getMessage x =
    let x : wrapped_message Js.t = Js.Unsafe.coerce x##.data in
    x##.belenios

  let getBallot e =
    if check_origin e then
      Js.Optdef.case (getMessage e)
        (fun () -> None)
        (fun x ->
          Js.Optdef.case x##.ballot
            (fun () -> None)
            (fun x -> Some (Js.to_string x)))
    else None

  let getReady e =
    if check_origin e then
      Js.Optdef.case (getMessage e)
        (fun () -> false)
        (fun x -> Js.Optdef.case x##.ready (fun () -> false) Js.to_bool)
    else false
end

let make_login_target ~state =
  let params = Url.encode_arguments [ ("state", state) ] in
  !!(Printf.sprintf "actions/voter-login?%s" params)

let submit_ballot uuid ~ballot =
  let* x = Api.(post (election_ballots uuid) `Nobody (String.trim ballot)) in
  match x.code with
  | 401 -> (
      match Yojson.Safe.from_string x.content with
      | `Assoc o -> (
          match List.assoc_opt "state" o with
          | Some (`String state) -> Lwt.return_some state
          | _ -> Lwt.return_none)
      | _ -> Lwt.return_none)
  | _ ->
      Firebug.console##log_2 (Js.string "Submitting ballot returned") x;
      Lwt.return_none

let finally x cont =
  cont ();
  x

let post_ballot uuid ~get_ballot _ =
  let open (val !Belenios_js.I18n.gettext) in
  let x =
    let submit = Printf.sprintf "#%s/advanced/submit" (Uuid.unwrap uuid) in
    Dom_html.window##open_ (Js.string submit) (Js.string "_blank") Js.null
  in
  let@ () = finally false in
  let@ window = Js.Opt.iter x in
  let window = coerce_window window in
  let id = ref None in
  let handler =
    let@ event = Dom_html.handler in
    let ready = Message.getReady event in
    let@ () = finally Js._false in
    if ready then (
      Option.iter Dom_html.removeEventListener !id;
      Message.postBallot window ~ballot:(get_ballot ()))
  in
  id :=
    Some
      (Dom_html.addEventListener Dom_html.window Event.message handler Js._false)

let advanced uuid =
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
        let ( let$ ) x f = Js.Optdef.case x (fun () -> Js._false) f in
        let$ files = dom##.files in
        let& file = files##item 0 in
        let reader = new%js File.fileReader in
        reader##.onload :=
          Dom.handler (fun _ ->
              let& content = File.CoerceTo.string reader##.result in
              ballot := Js.to_string content;
              Js._false);
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

let message_listener = ref None
let status = ref `Waiting

let explain () =
  let open (val !Belenios_js.I18n.gettext) in
  match !status with
  | `Waiting -> s_ "Please wait while your ballot is being processed..."
  | `Unexpected -> s_ "Unexpected response from server!"

let submit uuid =
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
  let container = div [ txt @@ explain () ] in
  let contents = [ container ] in
  let () =
    let dom = Tyxml_js.To_dom.of_div container in
    match !message_listener with
    | Some _ -> ()
    | None ->
        let handler =
          let@ event = Dom_html.handler in
          let ballot = Message.getBallot event in
          let@ () = finally Js._false in
          match ballot with
          | None -> ()
          | Some ballot -> (
              let@ () = Lwt.async in
              let* x = submit_ballot uuid ~ballot in
              match x with
              | None ->
                  status := `Unexpected;
                  dom##.textContent := Js.some @@ Js.string @@ explain ();
                  Lwt.return_unit
              | Some state ->
                  let target = make_login_target ~state in
                  window##.location##.href := Js.string target;
                  Lwt.return_unit)
        in
        message_listener :=
          Some
            (Dom_html.addEventListener window Event.message handler Js._false);
        let@ window = Js.Opt.iter window##.opener in
        Message.postReady window ()
  in
  Lwt.return { title; contents; footer }
