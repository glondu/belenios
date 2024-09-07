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
open Common

module type CONFIG = sig
  val headless : bool
  val webdriver : string
  val belenios : string
  val config : Admin.config
  val election_id : string
  val emails : in_channel
end

type authenticator = Webdriver.helpers -> unit Lwt.t

module Make (Config : CONFIG) = struct
  open Config

  let is_booth =
    let rex = Re.Pcre.regexp "^.*/vote(#.*)?$" in
    fun x -> Re.execp rex x

  let with_auth_window session f =
    let* windows = session#get_windows in
    let booth = ref None and auth = ref None in
    let* () =
      Lwt_list.iter_s
        (fun w ->
          let* () = session#switch_to_window w in
          let* url = session#get_url in
          if is_booth url then booth := Some w else auth := Some w;
          Lwt.return_unit)
        windows
    in
    match (!booth, !auth) with
    | Some booth, Some auth ->
        let* () = session#switch_to_window auth in
        let* () = f () in
        session#switch_to_window booth
    | _ -> failwith "identify_windows"

  let auth_password ~username ~password session =
    let@ () = with_auth_window session in
    let* () = session#fill_with ~selector:"#username" username in
    let* () = session#fill_with ~selector:"#password" password in
    session#click_on ~selector:"input[type=submit]"

  let auth_email ~username session =
    let* () = Lwt_unix.sleep 1. in
    let@ () = with_auth_window session in
    let x = Emails.parse emails in
    match Emails.extract_code x username with
    | Some code ->
        let* () = session#fill_with ~selector:"input[type=text]" code in
        session#click_on ~selector:"input[type=submit]"
    | None ->
        Lwt.fail
        @@ Failure (Printf.sprintf "could not find code for %s" username)

  let answer_question session i = function
    | Admin.Select { min; blank; answers; _ } ->
        let* () =
          if min = 0 then Lwt.return_unit
          else if blank then
            session#click_on
              ~selector:
                (Printf.sprintf "label[for=question_%d__choice_%d]" i
                   (List.length answers))
          else Lwt.fail @@ Failure "do not know how to answer question"
        in
        session#click_on ~selector:".vote-navigation__next-button"

  let vote ~voter ~credential ~auth =
    Printf.printf "  Voter %s votes...\n%!" voter;
    let@ session = Webdriver.with_session ~headless ~url:webdriver () in
    let session = new Webdriver.helpers session in
    let url = Printf.sprintf "%s/election#%s" belenios election_id in
    let* () = session#navigate_to url in
    let* () = session#click_on ~selector:"#start" in
    let* () = session#fill_with ~selector:"#credential" credential in
    let* () = session#click_on ~selector:".input-credential-section__button" in
    let* () =
      Lwt_list.iteri_s (fun i q -> answer_question session i q) config.questions
    in
    let* () = session#click_on_last_button in
    let* () = auth session in
    Lwt.return_unit
end
