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

type t = string

let available =
  [
    ("ar", "العربية");
    ("cs", "Čeština");
    ("de", "Deutsch");
    ("el", "Ελληνικά");
    ("en", "English");
    ("es", "Español");
    ("es_419", "Español (Latinoamérica)");
    ("fi", "Suomi");
    ("fr", "Français");
    ("it", "Italiano");
    ("jpn_JP", "日本語");
    ("lt", "Lietuvių");
    ("ms", "Bahasa Melayu");
    ("nb", "Norsk (Bokmål)");
    ("nl", "Nederlands");
    ("oc", "Occitan");
    ("pl", "Polski");
    ("pt", "Português");
    ("pt_BR", "Português (Brasil)");
    ("ro", "Română");
    ("ru", "Русский");
    ("sk", "Slovenčina");
    ("sl", "Slovenščina");
    ("ta", "தமிழ்");
    ("uk", "Українська");
  ]

let devel = "en_devel"
let default = "en"
let of_string_opt x = if List.mem_assoc x available then Some x else None
let get x = match of_string_opt x with None -> default | Some x -> x
let unwrap = Fun.id

let wrap x =
  if List.mem_assoc x available then x else invalid_arg "Language.wrap"
