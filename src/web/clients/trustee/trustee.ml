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
open Tyxml_js.Html
open Belenios
open Belenios_js.Common
open Belenios_js.Secondary_ui
open Belenios_js.Session
open Common

type wrapped_election_status =
  | WES : {
      election : ('a, 'b) Election.u;
      status : ('a, 'b) Belenios_web_api.election_trustee_status option;
    }
      -> wrapped_election_status

module App (U : UI) = struct
  let component = "admin"

  let router _configuration path =
    let open (val !Belenios_js.I18n.gettext) in
    U.set_title @@ s_ "Trustee management";
    match path with
    | [ uuid; token ] ->
        let@ uuid cont =
          match Uuid.of_string uuid with
          | exception _ -> Lwt.return [ txt @@ s_ "Invalid board identifier!" ]
          | x -> cont x
        in
        let@ group cont =
          let* x = Api.(get (trustees_group uuid) `Nobody) in
          match x with
          | Error _ -> Lwt.return [ txt @@ s_ "Could not get parameters!" ]
          | Ok (x, _) -> cont x
        in
        let module G = (val Group.make group) in
        let status_div = div [] in
        let update_status =
          let status_div = Tyxml_js.To_dom.of_div status_div in
          fun () ->
            let* status =
              let* x =
                Api.(get (trustees_trustee uuid (module G)) (`Trustee token))
              in
              match x with
              | Error _ -> Lwt.return_none
              | Ok (x, _) -> Lwt.return_some x
            in
            status_div##.innerHTML := Js.string "";
            match status with
            | None ->
                appendElements status_div [ txt @@ s_ "État inconnu!" ];
                Lwt.return_unit
            | Some { index; status = `Draft x } ->
                let* contents =
                  Generate.generate uuid ~token ~index (module G) x
                in
                appendElements status_div contents;
                Lwt.return_unit
            | Some
                { status = `Ready { cert_verification_key = vk; elections }; _ }
              ->
                let@ () = finally Lwt.return_unit in
                let@ () = load_and_check_private_key (module G) vk status_div in
                let@ () = Lwt.async in
                let@ () =
                 fun cont ->
                  let* contents = cont () in
                  appendElements status_div contents;
                  Lwt.return_unit
                in
                let* elections =
                  Lwt_list.filter_map_p
                    (fun uuid ->
                      let@ election cont =
                        let* x = Api.(get (election uuid) `Nobody) in
                        match x with
                        | Error _ -> Lwt.return_none
                        | Ok (x, _) -> cont x
                      in
                      let module W = (val election) in
                      let* status =
                        let* x =
                          Api.(
                            get
                              (election_trustee uuid (module W.G))
                              (`Trustee token))
                        in
                        match x with
                        | Error _ -> Lwt.return_none
                        | Ok (x, _) -> Lwt.return_some x
                      in
                      Lwt.return_some @@ WES { election = (module W); status })
                    elections
                in
                let action_div = div [] in
                let elections =
                  let action_div = Tyxml_js.To_dom.of_div action_div in
                  elections
                  |> List.map (fun (WES { election; status }) ->
                      let module W = (val election) in
                      let uuid_s = Uuid.to_string W.uuid in
                      let btn =
                        match status with
                        | None -> []
                        | Some status -> (
                            let@ () = fun cont -> [ txt " "; cont () ] in
                            match status with
                            | `Shuffle ->
                                let@ () =
                                  button
                                    ~a:
                                      [
                                        a_id
                                        @@ Printf.sprintf "shuffle_%s" uuid_s;
                                      ]
                                  @@ s_ "Shuffle"
                                in
                                action_div##.innerHTML := Js.string "";
                                let* xs = Shuffle.shuffle uuid ~token in
                                List.iter
                                  (fun x ->
                                    Dom.appendChild action_div
                                      (Tyxml_js.To_dom.of_node x))
                                  xs;
                                Lwt.return_unit
                            | `Tally x ->
                                let@ () =
                                  button
                                    ~a:
                                      [
                                        a_id
                                        @@ Printf.sprintf "decrypt_%s" uuid_s;
                                      ]
                                  @@ s_ "Decrypt"
                                in
                                action_div##.innerHTML := Js.string "";
                                let* xs = Decrypt.decrypt ~token election x in
                                List.iter
                                  (fun x ->
                                    Dom.appendChild action_div
                                      (Tyxml_js.To_dom.of_node x))
                                  xs;
                                Lwt.return_unit)
                      in
                      let link =
                        Tyxml_js.Html.a
                          ~a:
                            [
                              a_href
                              @@ Printf.sprintf "election#%s"
                                   (Uuid.to_string W.uuid);
                              a_target "_blank";
                            ]
                          [ txt W.template.name ]
                      in
                      [ [ link ]; btn ] |> List.flatten |> li)
                in
                [
                  h3
                    [ txt @@ s_ "Elections associated to this electoral board" ];
                  ul
                    (if elections = [] then [ li [ em [ txt @@ s_ "(none)" ] ] ]
                     else elections);
                  hr ();
                  action_div;
                ]
                |> Lwt.return
        in
        (Common.refresh := fun () -> Lwt.async update_status);
        let* () = update_status () in
        let btn_update =
          let@ () =
            button ~a:[ a_id "refresh_status" ] @@ s_ "Refresh status"
          in
          update_status ()
        in
        [ div [ btn_update ]; hr (); status_div ] |> Lwt.return
    | _ -> Lwt.return [ txt @@ s_ "Error in URL!" ]
end

module _ = Make (App) ()
