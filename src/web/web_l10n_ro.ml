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
let booth_step5 = "Pas 5/6: Confirmare"
let booth_step6 = "Pas 6/6: "
let i_am = "Eu sunt "
let and_ = " și "
let i_cast_my_vote = "Am depus votul meu"
let please_login_to_confirm = "Vă rugăm să vă logați pentru a confirma votul vostru."
let your_ballot_for = "Buletinul de vot pentru "
let has_been_received = " a fost primit, dar nu a fost încă înregistrat. "
let nobody_can_see = "Notă: buletinul de vot este criptat și nimeni nu-i poate vedea conținutul."
let you_have_already_voted = "Notă: ați votat deja. Votul vostru va fi înlocuit."
let go_back_to_election = "Întoarcere la pagina de start a alegerii"
let has_been_accepted = " a fost acceptat."
let you_can_check_its_presence = "Puteți verifica prezența sa în "
let ballot_box = "urna de vot"
let anytime_during_the_election = " în orice moment din perioada alegerii."
let confirmation_email = " Un e-mail de confirmare v-a fost trimis."
let thank_you_for_voting = "Vă mulțumim pentru participare!"
let is_rejected_because = " este resprins, deoarece "
let fail = "EȘEC!"
let accepted_ballots = "Buletinele de vot acceptate"
let ballots_have_been_accepted_so_far = " buletin(e) de vot au fost acceptat(e) până în prezent."
let ballots_have_been_accepted = " buletin(e) de vot au fost acceptat(e)."
let ballots_have_been_accepted_and = " buletin(e) de vot au fost acceptat(e), și "
let have_been_tallied = " au fost contorizate."
let username = "Nume utilizator:"
let password = "Parola:"
let login = "Conectare"
let password_login = "Conectare folosind parola"
let not_found = "Not found"
let election_does_not_exist = "This election does not exist. This may happen for elections that have not yet been open or have been deleted."
let cookies_are_blocked = "Cookie-urile sunt blocate"
let please_enable_them = "Browser-ul dumneavoastră pare să blocheze cookie-urile. Vă rugăm să le activați."


let mail_password_subject : ('a, 'b, 'c, 'd, 'e, 'f) format6 =
  "Parola voastră pentru alegere %s"

let mail_password : ('a, 'b, 'c, 'd, 'e, 'f) format6 =
  "Sunteți înregistrat(ă) ca votant(ă) pentru alegerea

  %s

Mai jos veți găsi numele de utilizator și parola. Pentru a
depune votul vostru, vă trebuie un cod de votare, ce va fi
trimis într-un e-mail separat.  Aveți grijă, parola și codul
de votare arată similare, dar joacă roluri diferite. Sistemul
va solicita codul de votare la intrarea în cabina de vot.
Numele de utilizator și parola sunt necesare atunci când
buletinul de vot este gata pentru depunere.

Nume utilizator: %s
Parola: %s
Pagina alegerii: %s

Rețineți că este posibil să votați de mai multe ori.
Numai ultimul vot va fi luat în considerare.%s"


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

let error_Serialization : ('a, 'b, 'c, 'd, 'e, 'f) format6 = "buletinul vostru de vot are o eroare sintactică (%s)"
let error_ProofCheck = "unele dovezi sunt invalide"
let error_ElectionClosed = "alegerea este inchisă"
let error_MissingCredential = "un cod de votare lipsește"
let error_InvalidCredential = "codul vostru de votare este invalid"
let error_RevoteNotAllowed = "nu sunteți autorizați să revotați"
let error_ReusedCredential = "codul vostru de votare a vost deja utilizat"
let error_WrongCredential = "nu sunteți autorizați să votați cu acest cod de vot"
let error_UnauthorizedVoter = "nu sunteți autorizați să votați"
