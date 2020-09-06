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
