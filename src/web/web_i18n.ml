(**************************************************************************)
(*                                BELENIOS                                *)
(*                                                                        *)
(*  Copyright © 2012-2020 Inria                                           *)
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

let langs = Hashtbl.create 10

module type LANG = sig
  val lang : string
end

module Belenios_Gettext (L : LANG) (T : GettextTranslate.TRANSLATE_TYPE) : Web_i18n_sig.GETTEXT = struct
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
  let u = T.create t ("po/" ^ L.lang ^ ".mo") (fun x -> x)
  let s_ str = T.translate u false str None
  let f_ str = Scanf.format_from_string (T.translate u true (string_of_format str) None) str
  let sn_ str str_plural n = T.translate u false str (Some (str_plural, n))
  let fn_ str str_plural n = Scanf.format_from_string (T.translate u true (string_of_format str) (Some (string_of_format str_plural, n))) str
end

let default_gettext =
  let module L = struct let lang = "en" end in
  let module G = Belenios_Gettext (L) (GettextTranslate.Dummy) in
  (module G : Web_i18n_sig.GETTEXT)

let register lang =
  let module L = struct let lang = lang end in
  let module G = Belenios_Gettext (L) (GettextTranslate.Map) in
  Hashtbl.add langs lang (module G : Web_i18n_sig.GETTEXT)

let () =
  Lwt_main.run (Lwt_io.lines_of_file "po/LINGUAS" |> Lwt_stream.to_list)
  |> List.iter register

let get_lang_gettext lang =
  try Hashtbl.find langs lang
  with Not_found -> default_gettext
