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

type admin =
  | Local of { username : string; password : string }
  | Demo of { username : string }

type question =
  | Select of {
      question : string;
      min : int;
      max : int;
      blank : bool;
      answers : string list;
    }

type auth = Password | Email
type config = { questions : question list; voters : string list; auth : auth }

module type CONFIG = sig
  val webdriver : string
  val belenios : string
  val headless : bool
  val admin : admin
  val config : config
  val emails : in_channel
end

module Make (Config : CONFIG) = struct
  open Config

  let accept_tos_if_needed session =
    let* elements = session#get_elements ~selector:"#accept_tos" in
    match elements with
    | [ e ] ->
        Printf.printf "    Accepting terms of service...\n%!";
        let* () = session#click e in
        session#implicit_wait
    | _ -> Lwt.return_unit

  let local_login session ~username ~password =
    Printf.printf "  Logging in %s as local:%s...\n%!" belenios username;
    let* () = session#click_on ~selector:"#login_local" in
    let* () = session#fill_with ~selector:"#username" username in
    let* () = session#fill_with ~selector:"#password" password in
    let* () = session#click_on ~selector:"input[type=submit]" in
    let* () = accept_tos_if_needed session in
    session#click_on ~selector:"#experimental"

  let demo_login session ~username =
    Printf.printf "  Logging in %s as demo:%s...\n%!" belenios username;
    let do_login () =
      let* () = session#click_on ~selector:"#login_demo" in
      let* () = session#fill_with ~selector:"#username" username in
      let* () = session#click_on ~selector:"input[type=submit]" in
      accept_tos_if_needed session
    in
    let* () = do_login () in
    let* () =
      let* elements = session#get_elements ~selector:"input[type=text]" in
      match elements with
      | [ e ] ->
          Printf.printf "    Setting email address after first login...\n%!";
          let address = "admin@example.org" in
          let* () = session#clear e in
          let* () = session#send_keys e address in
          let* () = session#click_on ~selector:"input[type=submit]" in
          let* () = Lwt_unix.sleep 1. in
          let emails = Emails.parse emails in
          let code =
            match Emails.extract_code emails address with
            | None ->
                failwith @@ Printf.sprintf "could not find code for %s" address
            | Some x -> x
          in
          let* () = session#fill_with ~selector:"input[type=text]" code in
          let* () = session#click_on ~selector:"input[type=submit]" in
          do_login ()
      | _ -> Lwt.return_unit
    in
    session#click_on ~selector:"#experimental"

  let login session =
    match admin with
    | Local { username; password } -> local_login session ~username ~password
    | Demo { username } -> demo_login session ~username

  let create_new_election session =
    Printf.printf "  Creating new election...\n%!";
    let* () = session#click_on ~selector:"#create_new_election" in
    let* x = session#get_url in
    let n = String.length x in
    let i = String.index x '#' + 1 in
    Lwt.return @@ String.sub x i (n - i)

  let set_name_and_description session ~name ~description =
    Printf.printf "    Setting election name and description...\n%!";
    let* () = session#click_on ~selector:"#tab_title" in
    let* () = session#fill_with ~selector:"#election_name_textarea" name in
    session#fill_with ~selector:"#election_description_textarea" description

  let force_fill_with (session : Webdriver.helpers) ~selector text =
    let* () =
      let@ e = session#with_single_element ~selector () in
      let* () = session#clear e in
      session#implicit_wait
    in
    let* () =
      let@ e = session#with_single_element ~selector () in
      let* () = session#send_keys e text in
      session#implicit_wait
    in
    Lwt.return_unit

  let fill_with_num (session : Webdriver.helpers) ~selector x =
    let@ e = session#with_single_element ~selector () in
    let* () = session#send_keys e (Printf.sprintf "\xee\x80\x83%d" x) in
    session#implicit_wait

  let set_question session q i =
    let* () = session#click_on ~selector:(Printf.sprintf "#qq%d" i) in
    match q with
    | Select { question; min; max; blank; answers } ->
        let* () = session#click_on ~selector:(Printf.sprintf "#type%d_1" i) in
        let* () =
          force_fill_with session ~selector:(Printf.sprintf "#q%d" i) question
        in
        let* () =
          Lwt_list.iter_s
            (fun _ ->
              let@ e =
                session#with_single_element ~selector:(Printf.sprintf "#qq%d" i)
                  ()
              in
              let* () = session#click e in
              let* elements =
                session#get_sub_elements e ~selector:".add_answer"
              in
              match elements with
              | [] -> Lwt.fail @@ Failure "could not find .add_answer"
              | e :: _ ->
                  let* () = session#click e in
                  session#implicit_wait)
            answers
        in
        let* () =
          Lwt_list.iteri_s
            (fun j a ->
              force_fill_with session
                ~selector:(Printf.sprintf "#ans%d_%d" i j)
                a)
            answers
        in
        let* () = session#click_on ~selector:(Printf.sprintf "#qq%d" i) in
        let* () =
          fill_with_num session ~selector:(Printf.sprintf "#selm%d" i) min
        in
        let* () =
          fill_with_num session ~selector:(Printf.sprintf "#selM%d" i) max
        in
        let* () =
          if blank then Lwt.return_unit
          else session#click_on ~selector:(Printf.sprintf "#blank%d" i)
        in
        Lwt.return_unit

  let rec repeat session ~selector ~accept =
    let* elements = session#get_elements ~selector in
    match elements with
    | [] -> Lwt.return_unit
    | e :: _ ->
        let* () = session#click e in
        let* () = accept () in
        repeat session ~selector ~accept

  let set_questions session =
    Printf.printf "    Setting questions...\n%!";
    let* () = session#click_on ~selector:"#tab_questions" in
    let* () =
      (* remove all questions *)
      repeat session ~selector:".remove_question" ~accept:(fun () ->
          session#accept)
    in
    let* () =
      Lwt_list.iter_s
        (fun _ ->
          let* elements = session#get_elements ~selector:".add_question" in
          match elements with
          | [] -> Lwt.fail @@ Failure "could not find .add_question"
          | e :: _ ->
              let* () = session#click e in
              (* remove all answers *)
              repeat session ~selector:".remove_answer" ~accept:(fun () ->
                  Lwt.return_unit))
        config.questions
    in
    let* () =
      Lwt_list.iteri_s
        (fun i q -> set_question session q (i + 1))
        config.questions
    in
    Lwt.return_unit

  let set_voters session voters =
    Printf.printf "    Setting voters...\n%!";
    let* () = session#click_on ~selector:"#tab_voters" in
    let* () =
      session#fill_with ~selector:"textarea" (String.concat "\n" voters)
    in
    session#click_on ~selector:"#add_voters"

  let set_trustees session =
    Printf.printf "    Setting trustees...\n%!";
    let* () = session#click_on ~selector:"#tab_trustees" in
    let* () = session#click_on ~selector:"button" in
    let* () = session#accept in
    session#accept

  let set_credentials session nvoters =
    Printf.printf "    Setting credentials...\n%!";
    let* () = session#click_on ~selector:"#tab_credentials" in
    let* () = session#click_on ~selector:"#rad_serv" in
    let* () = session#click_on ~selector:"button" in
    let* () = Lwt_unix.sleep (float_of_int nvoters *. 0.01) in
    let* () = session#click_on ~selector:"#tab_credentials" in
    session#click_on ~selector:"a[download]"

  let set_authentication session =
    Printf.printf "    Setting authentication...\n%!";
    let* () = session#click_on ~selector:"#tab_authentication" in
    match config.auth with
    | Password ->
        let* () = session#click_on ~selector:"#auth0" in
        let* () = session#click_on ~selector:"button" in
        session#accept
    | Email -> session#click_on ~selector:"#auth3"

  let open_election session =
    Printf.printf "  Opening election...\n%!";
    let* () = session#click_on ~selector:"#tab_openclose" in
    session#click_on ~selector:"button"

  let logout session =
    Printf.printf "  Logging out...\n%!";
    let* () = session#click_on ~selector:"#nav_username" in
    session#click_on ~selector:"#logout"

  let setup_election () =
    let nvoters = List.length config.voters in
    let@ session = Webdriver.with_session ~headless ~url:webdriver () in
    let session = new Webdriver.helpers session in
    let url = Printf.sprintf "%s/admin" belenios in
    let* () = session#navigate_to url in
    let* () = session#set_window_rect ~width:1000 ~height:1000 () in
    let* () = login session in
    let* election_id = create_new_election session in
    Printf.printf "    Election ID: %s\n%!" election_id;
    let* () =
      set_name_and_description session ~name:"Test election"
        ~description:"Automatic test election"
    in
    let* () = set_questions session in
    let* () = set_voters session config.voters in
    let* () = set_trustees session in
    let* () = set_credentials session nvoters in
    let* () = set_authentication session in
    let* () = open_election session in
    let* () = logout session in
    Lwt.return election_id

  let regen_password ~election_id ~username =
    Printf.eprintf "  Regenerating password of %s...\n%!" username;
    let@ session = Webdriver.with_session ~headless ~url:webdriver () in
    let session = new Webdriver.helpers session in
    let url = Printf.sprintf "%s/elections/%s/" belenios election_id in
    let* () = session#navigate_to url in
    let* () = session#set_window_rect ~width:1000 ~height:1000 () in
    let* () =
      session#click_on
        ~selector:(Printf.sprintf "#election_admin_%s" election_id)
    in
    let* () = login session in
    let* () = session#click_on ~selector:"#tab_authentication" in
    let* () = session#fill_with ~selector:"input" username in
    let* () = session#click_on ~selector:"button" in
    let* () = session#accept in
    let* () = logout session in
    let* () = Lwt_unix.sleep 1. in
    let x = Emails.parse Config.emails in
    match Emails.extract_password x username with
    | Some x -> Lwt.return x
    | None -> Lwt.fail @@ Failure "failed to retrieve new password"

  let tally_election ~election_id =
    Printf.printf "  Tallying election %s...\n%!" election_id;
    let@ session = Webdriver.with_session ~headless ~url:webdriver () in
    let session = new Webdriver.helpers session in
    let url = Printf.sprintf "%s/elections/%s/" belenios election_id in
    let* () = session#navigate_to url in
    let* () = session#set_window_rect ~width:1000 ~height:1000 () in
    let* () =
      session#click_on
        ~selector:(Printf.sprintf "#election_admin_%s" election_id)
    in
    let* () = login session in
    let* () = session#click_on ~selector:"#tab_openclose" in
    let* () = session#click_on ~selector:"button" in
    let* () = session#click_on ~selector:"#tab_tally" in
    let* () = session#accept in
    let* () = session#click_on ~selector:"button" in
    let* () = session#click_on ~selector:"a[target]" in
    let* () =
      let* windows = session#get_windows in
      let rec loop = function
        | [] -> Lwt.fail @@ Failure "did not find election window"
        | w :: ws ->
            let* () = session#switch_to_window w in
            let* url = session#get_url in
            if
              String.starts_with url
                ~prefix:(Printf.sprintf "%s/elections/" belenios)
            then Lwt.return_unit
            else loop ws
      in
      loop windows
    in
    let* () = session#close_window in
    let* () =
      let* windows = session#get_windows in
      match windows with
      | [ w ] -> session#switch_to_window w
      | _ -> Lwt.fail @@ Failure "not exactly 1 window"
    in
    Printf.printf "  Deleting election %s...\n%!" election_id;
    let* () = session#click_on ~selector:"#tab_delete" in
    session#accept
end
