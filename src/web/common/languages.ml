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

let available l =
  let open (val l : I18n.GETTEXT) in
  [
    "ar", s_ "Arabic";
    "cs", s_ "Czech";
    "de", s_ "German";
    "el", s_ "Greek";
    "en", s_ "English";
    "es", s_ "Spanish";
    "es_419", s_ "Spanish (Latin America)";
    "fi", s_ "Finnish";
    "fr", s_ "French";
    "it", s_ "Italian";
    "lt", s_ "Lithuanian";
    "ms", s_ "Malay";
    "nb", s_ "Norwegian Bokmål";
    "nl", s_ "Dutch";
    "oc", s_ "Occitan";
    "pl", s_ "Polish";
    "pt_BR", s_ "Portuguese (Brazil)";
    "ro", s_ "Romanian";
    "sk", s_ "Slovak";
    "uk", s_ "Ukrainian";
  ]
