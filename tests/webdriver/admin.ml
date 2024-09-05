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

type trustee = { name : string; email : string }
type trustee_mode = Basic | Threshold of int
type trustees = { mode : trustee_mode; trustees : trustee list }
type auth = Password | Email

type config = {
  questions : question list;
  voters : string list;
  trustees : trustees;
  registrar : string option;
  auth : auth;
}

module type CONFIG = sig
  val webdriver : string
  val belenios : string
  val headless : bool
  val admin : admin
  val config : config
  val emails : in_channel
end

type election_params = {
  id : string;
  private_keys : string list;
  private_creds : Yojson.Safe.t option;
}

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

  let add_trustee session { name; email } =
    let* () = session#click_on ~selector:".new_trustee .ins_sym" in
    let* () = session#fill_with ~selector:"#add_trustee_popup #inp1" email in
    let* () = session#fill_with ~selector:"#add_trustee_popup #inp2" name in
    session#click_on ~selector:"#add_trustee_popup button:nth-child(2)"

  let collect_trustee_links nth session =
    let* elements =
      let selector = Printf.sprintf "#main_zone td:nth-child(%d) > a" nth in
      session#get_elements ~selector
    in
    let* x =
      session#execute ~script:"return Array.from(arguments).map((x) => x.href)"
        ~args:(List.map Webdriver.json_of_element elements)
    in
    match x with
    | Some (`List xs) ->
        Lwt.return
        @@ List.map (function `String x -> x | _ -> assert false) xs
    | _ -> assert false

  let set_trustees session { mode; trustees } =
    Printf.printf "    Setting trustees...\n%!";
    let* () = session#click_on ~selector:"#tab_trustees" in
    let* () =
      match mode with
      | Basic -> Lwt.return_unit
      | Threshold _ -> session#click_on ~selector:"#thresh"
    in
    let* () =
      match trustees with
      | [] -> Lwt.return_unit
      | _ -> Lwt_list.iter_s (add_trustee session) trustees
    in
    let* () =
      match mode with
      | Basic -> Lwt.return_unit
      | Threshold t ->
          let* () =
            session#fill_with ~selector:"#thresh_val" (string_of_int t)
          in
          session#click_on ~selector:"#main_zone"
    in
    let* () = session#click_on ~selector:"#trustee_proc_but button" in
    let* () = session#accept in
    match trustees with
    | [] ->
        let* () = session#accept in
        Lwt.return_nil
    | _ -> collect_trustee_links 3 session

  let set_credentials session nvoters =
    Printf.printf "    Setting credentials...\n%!";
    let* () = session#click_on ~selector:"#tab_credentials" in
    match config.registrar with
    | None ->
        let* () = session#click_on ~selector:"#rad_serv" in
        let* () = session#click_on ~selector:"button" in
        let* () = Lwt_unix.sleep (float_of_int nvoters *. 0.01) in
        let* () = session#click_on ~selector:"#tab_credentials" in
        let* () = session#click_on ~selector:"a[download]" in
        Lwt.return_none
    | Some registrar -> (
        let* () = session#click_on ~selector:"#rad_ext" in
        let* () =
          session#fill_with ~selector:"#cred_auth_name input" registrar
        in
        let* () = session#click_on ~selector:"#main_zone" in
        let* x = session#get_elements ~selector:"#cred_link_target" in
        match x with
        | [ x ] -> (
            let* x =
              session#execute ~script:"return arguments[0].textContent"
                ~args:[ Webdriver.json_of_element x ]
            in
            match x with
            | Some (`String x) -> Lwt.return_some x
            | _ -> assert false)
        | _ -> assert false)

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
    session#click_on ~selector:"#logout_direct"

  let setup_trustee link =
    Printf.printf "    Setting up trustee...\n%!";
    let@ session = Webdriver.with_session ~headless ~url:webdriver () in
    let session = new Webdriver.helpers session in
    let* () = session#navigate_to link in
    let* () = session#set_window_rect ~width:1000 ~height:1000 () in
    let* () = session#click_on ~selector:"#generate_key" in
    let* x = session#get_elements ~selector:"#private_key" in
    match x with
    | [ x ] -> (
        let* () = session#click x in
        let* x =
          session#execute ~script:"return arguments[0].href"
            ~args:[ Webdriver.json_of_element x ]
        in
        match x with
        | Some (`String x) ->
            let private_key = decode_data_uri x in
            (* TODO: check fingerprint *)
            let* () = session#click_on ~selector:"#submit_public_key" in
            Lwt.return private_key
        | _ -> assert false)
    | _ -> assert false

  let set_private_key session private_key =
    let script =
      {|
        const x = document.getElementById("private_key");
        x.value = arguments[0];
        x.dispatchEvent(new Event("change"));
        return true;
      |}
    in
    let* x = session#execute ~script ~args:[ `String private_key ] in
    match x with
    | Some (`Bool true) -> Lwt.return_unit
    | _ -> failwith "setting #private_key failed"

  let setup_threshold_trustee step (private_key, link) =
    Printf.printf "    Setting up trustee (step %d)...\n%!" step;
    let@ session = Webdriver.with_session ~headless ~url:webdriver () in
    let session = new Webdriver.helpers session in
    let* () = session#navigate_to link in
    let* () = session#set_window_rect ~width:1000 ~height:1000 () in
    let* () = set_private_key session private_key in
    let* () = session#click_on ~selector:"#submit_data" in
    Lwt.return_unit

  let setup_registrar = function
    | None -> Lwt.return_none
    | Some link ->
        Printf.printf "    Setting up registrar...\n%!";
        let@ session = Webdriver.with_session ~headless ~url:webdriver () in
        let session = new Webdriver.helpers session in
        let* () = session#navigate_to link in
        let* () = session#set_window_rect ~width:1000 ~height:1000 () in
        let* () = session#click_on ~selector:"#generate" in
        let* creds =
          let* x = session#get_elements ~selector:"#creds" in
          match x with
          | [ x ] -> (
              let* () = session#click x in
              let* x =
                session#execute ~script:"return arguments[0].href"
                  ~args:[ Webdriver.json_of_element x ]
              in
              match x with
              | Some (`String x) ->
                  decode_data_uri x |> Yojson.Safe.from_string |> Lwt.return
              | _ -> assert false)
          | _ -> assert false
        in
        let* () = session#click_on ~selector:"#submit" in
        Lwt.return_some creds

  let with_admin ?id () f =
    let@ session = Webdriver.with_session ~headless ~url:webdriver () in
    let session = new Webdriver.helpers session in
    let url = Printf.sprintf "%s" belenios in
    let* () = session#navigate_to url in
    let* () = session#set_window_rect ~width:1000 ~height:1000 () in
    let* () = login session in
    let* () =
      match id with
      | None -> Lwt.return_unit
      | Some id ->
          let url = Printf.sprintf "%s/admin#%s" belenios id in
          session#navigate_to url
    in
    f session

  let setup_election () =
    let nvoters = List.length config.voters in
    let* id, trustee_links =
      let@ session = with_admin () in
      let* election_id = create_new_election session in
      Printf.printf "    Election ID: %s\n%!" election_id;
      let* () =
        set_name_and_description session ~name:"Test election"
          ~description:"Automatic test election"
      in
      let* () = set_questions session in
      let* () = set_voters session config.voters in
      let* links = set_trustees session config.trustees in
      let* () = logout session in
      Lwt.return (election_id, links)
    in
    let* private_keys =
      match config.trustees.mode with
      | Basic -> Lwt_list.map_s setup_trustee trustee_links
      | Threshold _ ->
          let* private_keys = Lwt_list.map_s setup_trustee trustee_links in
          let ts = List.combine private_keys trustee_links in
          let* () = Lwt_list.iter_s (setup_threshold_trustee 2) ts in
          let* () = Lwt_list.iter_s (setup_threshold_trustee 3) ts in
          Lwt.return private_keys
    in
    let* registrar =
      let@ session = with_admin ~id () in
      set_credentials session nvoters
    in
    let* private_creds = setup_registrar registrar in
    let* () =
      let@ session = with_admin ~id () in
      let* () = set_authentication session in
      let* () = open_election session in
      let* () = logout session in
      Lwt.return_unit
    in
    Lwt.return { id; private_keys; private_creds }

  let regen_password ~election_id ~username =
    Printf.eprintf "  Regenerating password of %s...\n%!" username;
    let@ session = Webdriver.with_session ~headless ~url:webdriver () in
    let session = new Webdriver.helpers session in
    let url = Printf.sprintf "%s/election#%s" belenios election_id in
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

  let do_partial_decryption (private_key, link) =
    Printf.printf "  Computing partial decryption...\n%!";
    let@ session = Webdriver.with_session ~headless ~url:webdriver () in
    let session = new Webdriver.helpers session in
    let* () = session#navigate_to link in
    let* () = set_private_key session private_key in
    let* () = session#click_on ~selector:"#submit_data" in
    Lwt.return_unit

  let tally_election check { id; private_keys; _ } =
    Printf.printf "  Tallying election %s...\n%!" id;
    let* links =
      let@ session = with_admin ~id () in
      let* () = session#click_on ~selector:"#tab_openclose" in
      let* () = session#click_on ~selector:"button" in
      let* () = session#click_on ~selector:"#tab_tally" in
      let* () = session#accept in
      let* () = session#click_on ~selector:"#tab_trustees" in
      let* links =
        match private_keys with
        | [] -> Lwt.return_nil
        | _ -> collect_trustee_links 2 session
      in
      let* () = logout session in
      Lwt.return links
    in
    let* () =
      Lwt_list.iter_s do_partial_decryption (List.combine private_keys links)
    in
    let@ session = with_admin ~id () in
    let* () = session#click_on ~selector:"#tab_status" in
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
                ~prefix:(Printf.sprintf "%s/election#" belenios)
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
    let* () = check () in
    Printf.printf "  Deleting election %s...\n%!" id;
    let* () = session#click_on ~selector:"#tab_delete" in
    session#accept
end
