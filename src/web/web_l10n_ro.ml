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
let start = "Start"
let advanced_mode = "Mod avansat"
let see_accepted_ballots = "Consultă buletinele de vot acceptate"
let belenios_booth = "Cabina de vot Belenios"
let here = "aici"
let question_header = "Selectați între %d și %d rspuns(uri)"
let at_least = "Trebuie să selectați cel puțin %d răspuns(uri)"
let at_most = "Trebuie să selectați cel mult %d răspuns(uri)"
let previous = "Precedent"
let next = "Următor"
let nothing = "(nimic)"
let enter_cred = "Vă rugăm să introduceți codul de votare:"
let invalid_cred = "Cod de votare invalid !"
let input_credential = "Introducere cod de votare"
let answer_to_questions = "Răspuns la întrebări"
let warning_0_255 = "Warning: the system will accept any integer between 0 and 255 but, according to the election rules, invalid ballots (score too high or candidates not properly ranked) will be rejected at the end of the election."
let alert_0_255 = "Value must be an integer between 0 and 255."
let at_least_one_invalid = "At least one of the answers is invalid!"
let review_and_encrypt = "Rezumat și criptare"
let authenticate = "Autentificare"
let confirm = "Confirmare"
let done_ = "Terminat"
let booth_step1 = "Pas 1/6: Introducere cod de votare"
let booth_step2 = "Pas 2/6: Răspuns la întrebări"
let booth_step3 = "Pas 3/6: Rezumat și criptare"
let booth_step5 = "Pas 5/6: Confirmare"
let booth_step6 = "Pas 6/6: "
let input_your_credential = "Introduceți codul vostru de votare "
let wait_while_encrypted = "Vă rugăm să așteptați criptarea buletinului vostru de vot..."
let encrypting = "Criptare în curs..."
let restart = "Reîncepe"
let successfully_encrypted = "Buletinul vostru de vot a fost criptat cu succes, "
let not_cast_yet = "dar încă nu a fost depus în urna de vot"
let qmark = "!"
let your_tracker_is = "Numărul vostru de identificare este "
let we_invite_you_to_save_it = "Vă invităm să-l salvați pentru a verifica ulterior că votul vostru este luat în considerare."
let continue = "Continuă"
let election_uuid = "Codul UUID al alegerii: "
let election_fingerprint = "Amprenta digitală a alegerii: "
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
let audit_data = "Date de audit: "
let parameters = "parametrii"
let public_credentials = "chei de verificare"
let ballots = "buletine de vot"
let accepted_ballots = "Buletinele de vot acceptate"
let ballots_have_been_accepted_so_far = " buletin(e) de vot au fost acceptat(e) până în prezent."
let ballots_have_been_accepted = " buletin(e) de vot au fost acceptat(e)."
let ballots_have_been_accepted_and = " buletin(e) de vot au fost acceptat(e), și "
let have_been_tallied = " au fost contorizate."
let username = "Nume utilizator:"
let password = "Parola:"
let login = "Conectare"
let password_login = "Conectare folosind parola"
let by_using_you_accept = "Folosind acest sit, acceptați "
let privacy_policy = "politica noastră privind datele personale"
let accept = "Accept"
let not_found = "Not found"
let election_does_not_exist = "This election does not exist. This may happen for elections that have not yet been open or have been deleted."
let cookies_are_blocked = "Cookie-urile sunt blocate"
let please_enable_them = "Browser-ul dumneavoastră pare să blocheze cookie-urile. Vă rugăm să le activați."
let election_currently_closed = "Această alegere este în prezent închisă."
let election_closed_being_tallied = "Alegerea este închisă și în curs de contorizare."
let election_has_been_tallied = "Această alegere a fost contorizata."
let election_archived = "Această alegere este arhivată."
let number_accepted_ballots = "Numărul buletinelor de vot acceptate: "
let you_can_also_download = "De asemenea, puteți descărca "
let result_with_crypto_proofs = "rezultatul împreună cu dovada criptografică"
let the_raw_results = "The raw results can be viewed in the "
let json_result = "JSON result"
let it_contains_all_clear = ". It contains all submitted ballots in clear, in random order. It is up to you to apply your favorite counting method (e.g. Condorcet, STV, majority judgement)."
let blank_vote = "Vot alb"
let no_other_blank = "Nu puteți selecta o altă opțiune la un vot alb"
let it_will_open_in = "Se va deschide în "
let the_election_will_close_in = "Alegerea se va închide în "
let years = " an(i)"
let months = " luna(i)"
let days = " zi(le) "
let hours = " ora(e)"
let minutes = " minut(e)"
let seconds = " secundă(e)"
let result_currently_not_public : ('a, 'b, 'c, 'd, 'e, 'f) format6 = "Rezultatul acestei alegeri nu este disponibil deocamdată. Va fi disponibil în %s."


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
