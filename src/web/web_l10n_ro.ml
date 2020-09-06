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

let lang = "ro"


let mail_credential_subject : ('a, 'b, 'c, 'd, 'e, 'f) format6 =
  "Codul vostru de votare pentru alegere %s"

let mail_credential : ('a, 'b, 'c, 'd, 'e, 'f) format6 =
  "Sunteți înregistrat(ă) ca votant(ă) pentru alegerea

  %s

%s

Cod de votare: %s
Pagina alegerii: %s

Rețineți că este posibil să votați de mai multe ori.
Numai ultimul vot va fi luat în considerare.%s"

let mail_credential_password =
"Veți găsi mai jos codul de vot.  Pentru a depune votul vostru, vă
trebuie o parolă, ce va fi trimisă într-un e-mail separat.  Aveți
grijă, parola și codul de votare arată similare, dar joacă roluri
diferite. Sistemul va solicita codul de votare la intrarea în cabina
de vot.  Numele de utilizator și parola sunt necesare atunci când
buletinul de vot este gata pentru depunere."

let mail_credential_cas =
"Veți găsi mai jos codul de vot."

let mail_confirmation_subject : ('a, 'b, 'c, 'd, 'e, 'f) format6 =
  "Votul vostru pentru alegerea %s"

let mail_confirmation : ('a, 'b, 'c, 'd, 'e, 'f) format6 =
  "%s,

Votul vostru pentru alegerea

  %s

a fost înregistrat. Numărul vostru de identificare este

  %s%s


Puteți verifica prezența acestuia în urna de vot, accesibilă la
  %s

Rezultatele vor fi publicate pe pagina alegerii
  %s%s

-- \nBelenios"

let this_vote_replaces = "\n\nAcest vot înlocuiește orice vot precedent."
let please_contact = "Pentru a obține mai multe informații, va rugăm contactați:"
