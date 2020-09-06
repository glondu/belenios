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

let lang = "de"
let not_found = "Not found"
let election_does_not_exist = "This election does not exist. This may happen for elections that have not yet been open or have been deleted."
let cookies_are_blocked = "Cookies sind deaktiviert"
let please_enable_them = "Ihr Browser nimmt keine Cookies an, bitte aktivieren Sie diese."


let mail_password_subject : ('a, 'b, 'c, 'd, 'e, 'f) format6 =
  "Ihr Passwort für die Abstimmung %s"

let mail_password : ('a, 'b, 'c, 'd, 'e, 'f) format6 =
  "Sie sind für die folgende Abstimmung als Wähler eingetragen:

  %s

Am Ende der Mail finden Sie Ihren Benutzername und Ihr Passwort. Um
abzustimmen benötigen sie außerdem noch Ihre Wählernummer, die Ihnen
in einer separaten Mail zugestellt wird. Obwohl Passwort und
Wählernummer ähnlich aussehen, erfüllen sie zwei verschiedene Zwecke:
die Wählernummer wird für die Verschlüsselung Ihrer Stimme in der
virtuellen Wahlkabine benötigt, mit dem Passwort können Sie
anschließend Ihre verschlüsselte Stimme auf den Wahlserver übertragen.

Benutzername: %s
Passwort: %s
Website der Abstimmung: %s

Sie können so oft abstimmen wie Sie wollen, nur die letzte Stimme zählt.%s"


let mail_credential_subject : ('a, 'b, 'c, 'd, 'e, 'f) format6 =
  "Ihre Wählernummer für die Abstimmung %s"

let mail_credential : ('a, 'b, 'c, 'd, 'e, 'f) format6 =
  "Sie sind für die folgende Abstimmung als Wähler eingetragen:

  %s

%s

Wählernummer: %s
Website der Abstimmung: %s

Sie können so oft abstimmen wie Sie wollen, nur die letzte Stimme zählt.%s"

let mail_credential_password =
"Am Ende der Mail finden Sie Ihre Wählernummer. Um abzustimmen
benötigen sie außerdem noch Ihr Passwort, die Ihnen in einer separaten
Mail zugestellt wird. Obwohl Passwort und Wählernummer ähnlich
aussehen, erfüllen sie zwei verschiedene Zwecke: die Wählernummer wird
für die Verschlüsselung Ihrer Stimme in der virtuellen Wahlkabine
benötigt, mit dem Passwort können Sie anschließend Ihre verschlüsselte
Stimme auf den Wahlserver übertragen."

let mail_credential_cas =
"Am Ende der Mail finden Sie Ihre Wählernummer."

let mail_confirmation_subject : ('a, 'b, 'c, 'd, 'e, 'f) format6 =
  "Ihre Stimme zur Abstimmung %s"

let mail_confirmation : ('a, 'b, 'c, 'd, 'e, 'f) format6 =
  "%s,

Ihre Stimme zur Abstimmung

  %s

wurde angenommen. Ihre Stimmennummer ist:

  %s%s


Mit dieser Nummer können Sie überprüfen, ob sich Ihre Stimme in der
Wahlurne befindet:
  %s

Das Ergebnis wird auf der Website der Abstimmung veröffentlicht:
  %s%s

-- \nBelenios"

let this_vote_replaces = "\n\nDiese Stimme ersetzt eine eventuelle vorherige Stimme."
let please_contact = "Für mehr Informationen wenden Sie sich bitte an:"
