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

open Lwt
open Lwt.Syntax
open Web_common

module Make (X : Pages_sig.S) = struct
  open X
  open Web_services
  open Eliom_service
  open Eliom_registration

  let get_preferred_gettext () = Web_i18n.get_preferred_gettext "voter"

  let election_not_found () =
    let* l = get_preferred_gettext () in
    let open (val l) in
    Pages_common.generic_page ~title:(s_ "Not found")
      (s_
         "This election does not exist. This may happen for elections that \
          have not yet been open or have been deleted.")
      ()
    >>= Html.send ~code:404

  let with_election s uuid f =
    Public_archive.with_election s uuid ~fallback:election_not_found f

  let () =
    File.register ~service:source_code ~content_type:"application/x-gzip"
      (fun () () -> return !Web_config.source_file)

  let () =
    Any.register ~service:logo (fun () () ->
        match !Web_config.logo with
        | None -> fail_http `Not_found
        | Some (file, content_type) -> File.send ~content_type file)

  let () =
    Any.register ~service:favicon (fun () () ->
        match !Web_config.favicon with
        | None -> fail_http `Not_found
        | Some (file, content_type) -> File.send ~content_type file)

  let () =
    File.register ~content_type:"text/html" ~service:banner (fun lang () ->
        match !Web_config.warning_file with
        | None -> fail_http `Not_found
        | Some file ->
            if Web_i18n.is_valid_language lang then
              let f = Printf.sprintf "%s.%s" file lang in
              let* b = Lwt_unix.file_exists f in
              Lwt.return (if b then f else file)
            else Lwt.return file)

  let () =
    File.register ~content_type:"text/html" ~service:apps (fun page () ->
        match page with
        | "admin" | "trustee" | "credauth" | "election" | "vote" ->
            Lwt.return
            @@ Printf.sprintf "%s/apps/%s.html" !Web_config.share_dir page
        | _ -> fail_http `Not_found)

  let redir_preapply s u () =
    Redirection.send (Redirection (preapply ~service:s u))

  let wrap_handler f =
    Lwt.catch f (fun e ->
        Pages_common.generic_page ~title:"Error" (Printexc.to_string e) ()
        >>= Html.send)

  let get_cont_state cont =
    let redir =
      match cont.path with
      | ContSiteHome -> Redirection home
      | ContSiteElection uuid ->
          Redirection (preapply ~service:election_home_redirect (uuid, ()))
    in
    fun () -> Redirection.send redir

  let () =
    Any.register ~service:set_consent (fun cont () ->
        let () = Web_state.set_consent_cookie () in
        get_cont_state cont ())

  let () =
    Any.register ~service:set_language (fun (lang, cont) () ->
        let exp = Unix.gettimeofday () +. (10. *. 365. *. 86400.) in
        let () =
          Eliom_state.set_cookie ~exp ~name:"belenios-lang" ~value:lang ()
        in
        get_cont_state cont ())

  let forbidden () =
    let* l = get_preferred_gettext () in
    let open (val l) in
    let msg = s_ "You are not allowed to access this page!" in
    Pages_common.generic_page ~title:(s_ "Forbidden") msg ()
    >>= Html.send ~code:403
end
