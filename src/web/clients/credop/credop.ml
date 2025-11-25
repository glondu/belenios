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
open Js_of_ocaml_tyxml
open Tyxml_js.Html5
open Belenios
open Belenios_js.Common
open Belenios_js.Session
open Belenios_js.Secondary_ui

let seed = ref None
let valid = ref false

module App (U : UI) = struct
  let component = "admin"

  let get_credit_history uuid seed =
    let open (val !Belenios_js.I18n.gettext) in
    let fail () = Lwt.return [ txt @@ s_ "Could not get credit history!" ] in
    let@ credits cont =
      let* x = Api.(get (credentials_credits uuid) (`Credauth seed)) in
      match x with Ok (x, _) -> cont x | Error _ -> fail ()
    in
    let format_credit (x : Belenios_web_api.credentials_credit) =
      let t = new%js Js.date_fromTimeValue (Js.float (x.timestamp *. 1000.)) in
      let t = Js.to_string t##toLocaleString in
      tr
        [
          td [ txt t ];
          td [ txt @@ string_of_int x.credits ];
          td
            [ txt @@ Belenios_web_api.string_of_credentials_credit_kind x.kind ];
          td [ txt @@ string_of_bool x.success ];
        ]
    in
    let head =
      tr
        [
          td [ txt @@ s_ "Date and time" ];
          td [ txt @@ s_ "Credit variation" ];
          td [ txt @@ s_ "Variation kind" ];
          td [ txt @@ s_ "Success?" ];
        ]
    in
    let body = List.map format_credit credits in
    [
      tablex ~thead:(thead [ head ]) [ tbody body ];
      div
        [
          txt @@ s_ "Remaining resend credits:";
          txt " ";
          txt @@ string_of_int @@ Belenios_web_api.remaining_credits credits;
        ];
    ]
    |> Lwt.return

  let router _configuration path =
    let open (val !Belenios_js.I18n.gettext) in
    U.set_title @@ s_ "Credential operator interface";
    let@ uuid_s cont =
      match path with
      | [ uuid; seed' ] ->
          seed := Some seed';
          Dom_html.window##.location##replace (Js.string ("#" ^ uuid));
          cont uuid
      | [ uuid ] -> cont uuid
      | _ -> Lwt.return [ div [ txt @@ s_ "Error" ] ]
    in
    let uuid = Uuid.wrap uuid_s in
    let ask_seed, get_seed =
      match !seed with
      | Some x -> ([], fun () -> x)
      | None ->
          let inp = Tyxml_js.Html.input ~a:[ a_id "inp_seed" ] () in
          let inp_dom = Tyxml_js.To_dom.of_input inp in
          let lab =
            label
              ~a:[ a_label_for "inp_seed" ]
              [ txt @@ s_ "Please enter the secret key:"; txt " " ]
          in
          ([ div [ lab; inp ] ], fun () -> Js.to_string inp_dom##.value)
    in
    let credit_history = div [] in
    let credit_history_dom = Tyxml_js.To_dom.of_div credit_history in
    let check_certificate =
      let@ () = button @@ s_ "Check certificate" in
      let success () =
        alert @@ Printf.sprintf (f_ "The certificate for %s is valid!") uuid_s;
        Lwt.return_unit
      in
      let@ () = fun cont -> if !valid then success () else cont () in
      let fail () =
        alert @@ s_ "Error while retrieving election!";
        Lwt.return_unit
      in
      let@ roots cont =
        let* x = Api.(get (election_roots uuid) `Nobody) in
        match x with Ok (x, _) -> cont x | Error _ -> fail ()
      in
      let@ setup cont =
        match roots.roots_setup_data with
        | None -> fail ()
        | Some h -> (
            let* x = Api.(get (election_object uuid h) `Nobody) in
            match x with
            | Ok (x, _) -> cont @@ setup_data_of_string x
            | Error _ -> fail ())
      in
      let@ election cont =
        let h = setup.setup_election in
        let* x = Api.(get (election_object uuid h) `Nobody) in
        match x with
        | Ok (x, _) -> cont @@ Election.of_string (module Random) x
        | Error _ -> fail ()
      in
      let@ credentials cont =
        let h = setup.setup_credentials in
        let* x = Api.(get (election_object uuid h) `Nobody) in
        match x with Ok (x, _) -> cont x | Error _ -> fail ()
      in
      let fail () =
        alert @@ s_ "No certificate for this election!";
        Lwt.return_unit
      in
      let module E = (val election) in
      let module C = Credentials_certificate (E.G) in
      let@ certificate cont =
        match setup.setup_credentials_certificate with
        | None -> fail ()
        | Some h -> (
            let* x = Api.(get (election_object uuid h) `Nobody) in
            match x with
            | Ok (x, _) ->
                cont
                @@ credentials_certificate_of_string (sread E.G.of_string)
                     (sread E.G.Zq.of_string) x
            | Error _ -> fail ())
      in
      let@ () =
       fun cont ->
        if
          certificate.uuid = uuid
          && certificate.voter_list_length
             = List.length (public_credentials_of_string credentials)
          && certificate.public_creds_hash = Hash.hash_string credentials
          && C.check certificate
        then cont ()
        else (
          alert @@ s_ "The certificate is not valid!";
          Lwt.return_unit)
      in
      let seed_ = get_seed () in
      let module P = Pki.Make (E.G) (Dummy_random) in
      let decryption_key = P.derive_dk seed_ in
      let signature_key = P.derive_sk seed_ in
      let@ () =
       fun cont ->
        if
          (certificate.verification_key = E.G.(g **~ signature_key))
          && certificate.encryption_key = E.G.(g **~ decryption_key)
        then cont ()
        else (
          alert @@ s_ "The secret key is not valid!";
          Lwt.return_unit)
      in
      seed := Some seed_;
      valid := true;
      let* history = get_credit_history uuid seed_ in
      credit_history_dom##.innerHTML := Js.string "";
      List.iter
        (fun x ->
          Dom.appendChild credit_history_dom (Tyxml_js.To_dom.of_node x))
        history;
      success ()
    in
    let resend =
      let open Tyxml_js.Html in
      let make value l =
        let id = Printf.sprintf "inp_%s" value in
        div
          [
            input
              ~a:[ a_id id; a_input_type `Radio; a_name "spec"; a_value value ]
              ();
            label ~a:[ a_label_for id ] [ txt l ];
          ]
      in
      let some_voters_textarea =
        textarea
          ~a:[ a_rows 5; a_cols 40; a_style "vertical-align: text-top;" ]
          (txt "")
      in
      let some_voters =
        let id = "inp_some_voters" in
        div
          [
            input
              ~a:
                [
                  a_id id;
                  a_input_type `Radio;
                  a_name "spec";
                  a_value "some_voters";
                ]
              ();
            label ~a:[ a_label_for id ] [ txt @@ s_ "Some voters:" ];
            txt " ";
            some_voters_textarea;
          ]
      in
      let onsubmit e =
        let@ () = finally false in
        let@ seed cont =
          match (!seed, !valid) with
          | Some s, true -> cont s
          | _ -> alert @@ s_ "Please check certificate first!"
        in
        let@ e = Js.Opt.iter e##.target in
        let@ e = Js.Opt.iter (Dom_html.CoerceTo.form e) in
        let@ spec = Js.Opt.iter (e##.elements##namedItem (Js.string "spec")) in
        let@ spec cont =
          match Js.to_string (Js.Unsafe.coerce spec)##.value with
          | "all_voters" -> cont `All_voters
          | "missing_voters" -> cont `Missing_voters
          | "some_voters" ->
              let t = Tyxml_js.To_dom.of_textarea some_voters_textarea in
              let vs = t##.value |> Js.to_string |> split_lines in
              cont (`Some_voters vs)
          | _ -> ()
        in
        let r : Belenios_web_api.credentials_resend = { uuid; seed; spec } in
        let@ () = Lwt.async in
        let* x = Api.(post credentials_server `Nobody (`Resend r)) in
        let msg = match x.code with 200 -> s_ "Success" | _ -> s_ "Failure" in
        alert msg;
        Lwt.return_unit
      in
      form
        ~a:[ a_onsubmit onsubmit ]
        [
          make "all_voters" (s_ "All voters");
          make "missing_voters" (s_ "Missing voters");
          some_voters;
          input ~a:[ a_input_type `Submit; a_value @@ s_ "Resend" ] ();
        ]
    in
    let body =
      ask_seed
      @ [
          h1 [ txt @@ s_ "Check certificate" ];
          div [ check_certificate ];
          h1 [ txt @@ s_ "Resend private credentials" ];
          credit_history;
          div [ resend ];
        ]
    in
    Lwt.return body
end

module _ = Make (App) ()
