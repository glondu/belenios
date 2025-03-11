(**************************************************************************)
(*                                BELENIOS                                *)
(*                                                                        *)
(*  Copyright © 2012-2022 Inria                                           *)
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
open Belenios
open Belenios_js.Common
open Belenios_js.Session
module Messages = Belenios_js.Window_messages

class type renderingFunctions = object
  method text : int -> Js.js_string Js.t -> Js.Unsafe.any Js.meth
  method bold : int -> Js.Unsafe.any Js.js_array Js.t -> Js.Unsafe.any Js.meth
  method italic : int -> Js.Unsafe.any Js.js_array Js.t -> Js.Unsafe.any Js.meth

  method link :
    int -> Js.js_string Js.t -> Js.js_string Js.t -> Js.Unsafe.any Js.meth

  method result : Js.Unsafe.any Js.js_array Js.t -> Js.Unsafe.any Js.meth
  method error : Js.js_string Js.t -> Js.Unsafe.any Js.meth
end

module type ELECTION_WITH_SK = sig
  include Election.ELECTION

  val sk : G.Zq.t
end

class type initCallbacks = object
  method onsuccess : unit Js.meth
  method onfailure : Js.js_string Js.t -> unit Js.meth
end

class type initParams = object
  method root : Js.js_string Js.t Js.readonly_prop
  method stateful : bool Js.t Js.optdef Js.readonly_prop
  method lang : Js.js_string Js.t Js.optdef Js.readonly_prop
  method callbacks : initCallbacks Js.t Js.optdef Js.readonly_prop
end

class type checkCredentialCallbacks = object
  method success : (module ELECTION_WITH_SK) -> unit Js.meth
  method failure : Js.js_string Js.t -> Js.js_string Js.t Js.opt -> unit Js.meth
end

class type encryptBallotCallbacks = object
  method success : Js.js_string Js.t -> Js.js_string Js.t -> unit Js.meth
  method failure : Js.js_string Js.t -> unit Js.meth
end

class type submitBallotCallbacks = object
  method finished : unit Js.meth
end

class type tracker = object
  method filename : Js.js_string Js.t Js.readonly_prop
  method contents : Js.js_string Js.t Js.readonly_prop
end

class type belenios = object
  method init : initParams Js.t -> unit Js.meth
  method computeFingerprint : Js.Unsafe.any Js.t -> Js.js_string Js.t Js.meth

  method checkCredential :
    Js.Unsafe.any ->
    Js.js_string Js.t ->
    checkCredentialCallbacks Js.t ->
    unit Js.meth

  method encryptBallot :
    (module ELECTION_WITH_SK) ->
    Js.js_string Js.t ->
    encryptBallotCallbacks Js.t ->
    unit Js.meth

  method markup :
    renderingFunctions Js.t -> Js.js_string Js.t -> Js.Unsafe.any Js.meth

  method formatTracker :
    (module ELECTION_WITH_SK) -> Js.js_string Js.t -> tracker Js.t Js.meth

  method submitBallot : submitBallotCallbacks Js.t -> unit Js.meth
  method renderConfirmation : Dom_html.divElement Js.t -> unit Js.meth
  method initiateLogin : unit Js.meth
  method finalizeLogin : Js.Unsafe.any -> unit Js.meth
end

let stateful = ref false
let global_configuration = ref None
let election_and_ballot = ref None
let global_result = ref None

let belenios : belenios Js.t =
  object%js
    method init (p : initParams Js.t) =
      let root = Js.to_string p##.root in
      relative_root := root;
      let dir = root ^ "static/" in
      let () =
        Js.Optdef.iter p##.stateful (fun x -> stateful := Js.to_bool x)
      in
      let@ () = Lwt.async in
      let* () =
        Js.Optdef.case p##.lang
          (fun () -> Lwt.return_unit)
          (fun lang ->
            let lang = Js.to_string lang in
            Belenios_js.I18n.init ~dir ~component:"voter" ~lang)
      in
      let* () =
        if !stateful then (
          let* x = Api.(get configuration `Nobody) in
          match x with
          | Ok (x, _) ->
              global_configuration := Some x;
              Lwt.return_unit
          | Error _ ->
              let open (val !Belenios_js.I18n.gettext) in
              Js.Optdef.iter p##.callbacks (fun cb ->
                  cb##onfailure
                    (Js.string @@ s_ "Could not get server configuration!"));
              Lwt.return_unit)
        else Lwt.return_unit
      in
      Js.Optdef.iter p##.callbacks (fun cb -> cb##onsuccess);
      Lwt.return_unit

    method computeFingerprint x =
      Js._JSON##stringify x |> Js.to_string |> sha256_b64 |> Js.string

    method checkCredential params cred
        (callbacks : checkCredentialCallbacks Js.t) =
      Lwt.async (fun () ->
          Lwt.catch
            (fun () ->
              let* () = Lwt_js.yield () in
              let raw = Js._JSON##stringify params |> Js.to_string in
              let module W = (val Election.of_string (module Random) raw) in
              let* () = Lwt_js.yield () in
              let module Cred =
                Credential.Make
                  (W.G)
                  (struct
                    type 'a t = 'a Lwt.t

                    let return = Lwt.return
                    let bind = Lwt.bind
                    let pause = Lwt.pause
                    let uuid = W.uuid
                  end)
              in
              let* x = Cred.derive (Js.to_string cred) in
              let () =
                match x with
                | Ok sk ->
                    let module X : ELECTION_WITH_SK = struct
                      include W

                      let sk = sk
                    end in
                    callbacks##success (module X : ELECTION_WITH_SK)
                | Error `Invalid ->
                    callbacks##failure (Js.string "INVALID_CREDENTIAL") Js.null
                | Error `MaybePassword ->
                    callbacks##failure (Js.string "MAYBE_PASSWORD") Js.null
                | Error `Wrong ->
                    callbacks##failure (Js.string "WRONG_CREDENTIAL") Js.null
              in
              Lwt.return_unit)
            (fun e ->
              callbacks##failure
                (Js.string "GENERIC_FAILURE")
                (Js.some (Printexc.to_string e |> Js.string));
              Lwt.return_unit))

    method encryptBallot election plaintext
        (callbacks : encryptBallotCallbacks Js.t) =
      Lwt.async (fun () ->
          Lwt.catch
            (fun () ->
              let* () = Lwt_js.yield () in
              let open (val election : ELECTION_WITH_SK) in
              let plaintext =
                Js._JSON##stringify plaintext
                |> Js.to_string |> plaintext_of_string
              in
              let* () = Lwt_js.yield () in
              let b = E.create_ballot ~sk plaintext in
              let ballot = write_ballot -- b in
              if !stateful then election_and_ballot := Some (election, ballot);
              let tracker = sha256_b64 ballot in
              callbacks##success (Js.string ballot) (Js.string tracker);
              Lwt.return_unit)
            (fun e ->
              callbacks##failure (Printexc.to_string e |> Js.string);
              Lwt.return_unit))

    method markup (p : renderingFunctions Js.t) x =
      let open Belenios_ui in
      let pp : _ Markup_light.rendering_functions =
        {
          text = (fun key x -> p##text key (Js.string x));
          italic = (fun key xs -> p##italic key (Js.array @@ Array.of_list xs));
          bold = (fun key xs -> p##bold key (Js.array @@ Array.of_list xs));
          link =
            (fun key ~target ~label ->
              p##link key (Js.string target) (Js.string label));
        }
      in
      try
        let xs = Markup_light.parse_html (Js.to_string x) in
        let xs = Markup_light.render pp xs in
        p##result (Js.array @@ Array.of_list xs)
      with _ -> p##error x

    method formatTracker election tracker =
      let open (val !Belenios_js.I18n.gettext) in
      let module W = (val election : ELECTION_WITH_SK) in
      let uuid_s = Uuid.unwrap W.uuid in
      let url = Printf.sprintf "%selection#%s\n" (compute_prefix ()) uuid_s in
      let page =
        let open Tyxml.Html in
        html
          (head
             (title (txt @@ W.template.t_name ^^^ s_ "Smart ballot tracker"))
             [])
          (body
             [
               h1 [ txt W.template.t_name ];
               table
                 [
                   tr
                     [
                       th [ txt @@ s_ "Election public URL" ];
                       td [ a ~a:[ a_href url ] [ txt url ] ];
                     ];
                   tr
                     [
                       th [ txt @@ s_ "Smart ballot tracker" ];
                       td [ code [ txt @@ Js.to_string tracker ] ];
                     ];
                 ];
             ])
      in
      let contents =
        let b = Buffer.create 1024 in
        Tyxml.Html.pp () (Format.formatter_of_buffer b) page;
        Buffer.contents b
        |> encode_data_uri ~charset:"UTF-8" ~mime_type:"text/html"
        |> Js.string
      in
      let filename =
        Printf.sprintf "%s_%s_tracker.html"
          (remove_special_characters W.template.t_name)
          uuid_s
        |> Js.string
      in
      object%js
        val filename = filename
        val contents = contents
      end

    method submitBallot (callbacks : submitBallotCallbacks Js.t) =
      match !election_and_ballot with
      | None -> failwith "submitBallot not available in non-stateful mode"
      | Some (election, ballot) -> (
          let window =
            Dom_html.window##open_
              (Js.string !!"actions/voter-login")
              (Js.string "_blank") Js.null
          in
          let@ window = Js.Opt.iter window in
          let window = coerce_window window in
          let open (val election) in
          let@ () = Lwt.async in
          let@ () =
           fun cont ->
            let* result = cont () in
            global_result := Some result;
            callbacks##finished;
            Lwt.return_unit
          in
          let* x = Belenios_js.Cast.post_ballot uuid ~ballot in
          match x with
          | Ok state -> (
              let* _ = Messages.(wait ready) () in
              Messages.(post state) window state;
              let* confirmation = Messages.(wait confirmation) () in
              Messages.(post ready) window true;
              match
                Belenios_web_api.cast_result_of_string
                  (Js.to_string confirmation)
              with
              | exception _ -> Lwt.return @@ `Error `UnexpectedResponse
              | result -> Lwt.return result)
          | Error e ->
              window##close;
              Lwt.return @@ `Error e)

    method renderConfirmation container =
      match (!global_configuration, !election_and_ballot, !global_result) with
      | Some configuration, Some (election, _), Some result ->
          let module W = (val election) in
          container##.innerHTML := Js.string "";
          List.iter
            (fun x -> Dom.appendChild container (Tyxml_js.To_dom.of_node x))
            (Belenios_js.Cast.confirmation configuration (module W) result)
      | _ -> ()

    method initiateLogin =
      let () =
        let@ () = Lwt.async in
        let* state = Messages.(wait state) () in
        let target = make_login_target ~state in
        Dom_html.window##.location##.href := Js.string target;
        Lwt.return_unit
      in
      let@ window = Js.Opt.iter (coerce_window Dom_html.window)##.opener in
      Messages.(post ready) window true

    method finalizeLogin confirmation =
      let () =
        let@ () = Lwt.async in
        let* _ = Messages.(wait ready) () in
        Dom_html.window##close;
        Lwt.return_unit
      in
      let@ window = Js.Opt.iter (coerce_window Dom_html.window)##.opener in
      Messages.(post confirmation) window (Js._JSON##stringify confirmation)
  end

let () = Js.export "belenios" belenios
