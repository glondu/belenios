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
open Belenios
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
      | ContSiteAdmin -> Redirection admin
      | ContSiteElection uuid ->
          Redirection (preapply ~service:election_home (uuid, ()))
    in
    fun () -> Redirection.send redir

  let () =
    Any.register ~service:set_cookie_disclaimer (fun cont () ->
        let* () = Eliom_reference.set Web_state.show_cookie_disclaimer false in
        get_cont_state cont ())

  let () =
    Any.register ~service:election_nh_ciphertexts (fun uuid () ->
        Lwt.try_bind
          (fun () ->
            let@ s = Storage.with_transaction in
            Public_archive.get_nh_ciphertexts s uuid)
          (fun x -> String.send (x, "application/json"))
          (function
            | Election_not_found _ -> fail_http `Not_found | e -> Lwt.reraise e))

  let () =
    Any.register ~service:set_language (fun (lang, cont) () ->
        let* () = Eliom_reference.set Web_state.language (Some lang) in
        get_cont_state cont ())

  let forbidden () =
    let* l = get_preferred_gettext () in
    let open (val l) in
    let msg = s_ "You are not allowed to access this page!" in
    Pages_common.generic_page ~title:(s_ "Forbidden") msg ()
    >>= Html.send ~code:403
end
