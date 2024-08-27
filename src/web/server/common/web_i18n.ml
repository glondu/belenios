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
open Belenios
open Belenios_server_core

let default_lang = "en"
let devel_lang = "en_devel"
let components = Hashtbl.create 2

module type LANG = sig
  val lang : string
  val mo_file : string
end

module Belenios_Gettext (L : LANG) (T : GettextTranslate.TRANSLATE_TYPE) :
  Belenios_ui.I18n.GETTEXT = struct
  let lang = L.lang

  open GettextCategory
  open GettextTypes

  let t =
    {
      failsafe = Ignore;
      textdomains = MapTextdomain.empty;
      categories = MapCategory.empty;
      language = Some L.lang;
      codeset = "UTF-8";
      path = [];
      default = "belenios";
    }

  let u = T.create t L.mo_file (fun x -> x)
  let s_ str = T.translate u false str None

  let f_ str =
    Scanf.format_from_string
      (T.translate u true (string_of_format str) None)
      str
end

let build_gettext_input component lang =
  (module struct
    let lang = lang

    let mo_file =
      !Web_config.share_dir // "static" // "locales" // component
      // (lang ^ ".mo")
  end : LANG)

let default_gettext component =
  let module L = (val build_gettext_input component devel_lang) in
  let module G = Belenios_Gettext (L) (GettextTranslate.Dummy) in
  (module G : Belenios_ui.I18n.GETTEXT)

let () =
  List.iter
    (fun component ->
      let h = Hashtbl.create 10 in
      Hashtbl.add h devel_lang (default_gettext component);
      Hashtbl.add components component h)
    [ "voter"; "admin" ]

let lang_mutex = Lwt_mutex.create ()

let get ~component ~lang =
  let langs = Hashtbl.find components component in
  match Hashtbl.find_opt langs lang with
  | Some l -> Lwt.return l
  | None ->
      Lwt_mutex.with_lock lang_mutex (fun () ->
          match Hashtbl.find_opt langs lang with
          | Some l -> Lwt.return l
          | None ->
              let module L = (val build_gettext_input component lang) in
              let* b = Filesystem.file_exists L.mo_file in
              if b then (
                let get () =
                  let module L = Belenios_Gettext (L) (GettextTranslate.Map) in
                  (module L : Belenios_ui.I18n.GETTEXT)
                in
                let* l = Lwt_preemptive.detach get () in
                Hashtbl.add langs lang l;
                Lwt.return l)
              else Lwt.return (Hashtbl.find langs devel_lang))

let parse_lang =
  let rex = Re.Pcre.regexp "^([a-z]{2})(?:-.*)?$" in
  fun s ->
    match Re.Pcre.exec ~rex s with
    | groups -> Some (Re.Pcre.get_substring groups 1)
    | exception Not_found -> None

let get_preferred_language () =
  let langs = Eliom_request_info.get_accept_language () in
  match langs with
  | [] -> default_lang
  | (lang, _) :: _ -> (
      match parse_lang lang with None -> default_lang | Some lang -> lang)

let valid_languages =
  List.fold_left
    (fun accu (lang, _) -> SSet.add lang accu)
    SSet.empty Belenios_ui.Languages.available

let is_valid_language lang = SSet.mem lang valid_languages

module Make () = struct
  let is_valid_language = is_valid_language

  let get_preferred_gettext component =
    let* lang =
      let cookies = Eliom_request_info.get_cookies () in
      match Ocsigen_cookie_map.Map_inner.find_opt "belenios-lang" cookies with
      | Some lang when is_valid_language lang -> Lwt.return lang
      | _ -> Lwt.return @@ get_preferred_language ()
    in
    get ~component ~lang
end
