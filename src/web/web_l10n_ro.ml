(**************************************************************************)
(*                                BELENIOS                                *)
(*                                                                        *)
(*  Copyright © 2012-2018 Inria                                           *)
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
let question_header = "Întrebarea #%d din %d — selectați între %d și %d rspuns(uri)"
let at_least = "Trebuie să selectați cel puțin %d răspuns(uri)"
let at_most = "Trebuie să selectați cel mult %d răspuns(uri)"
let previous = "Precedent"
let next = "Următor"
let nothing = "(nimic)"
let enter_cred = "Vă rugăm să introduceți codul de votare:"
let invalid_cred = "Cod de votare invalid !"
let input_credential = "Introdu codul de votare"
let answer_to_questions = "Răspunde la întrebări"
let review_and_encrypt = "Rezumat și criptare"
let authenticate = "Autentificare"
let confirm = "Confirmare"
let done_ = "Terminare"
let booth_step1 = "Pas 1/6: Introduceți codul de votare"
let booth_step2 = "Pas 2/6: Răspunde la întrebări"
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
let election_fingerprint = "Amprenta digitală al alegerii: "
let i_am = "Eu sunt "
let and_ = " și "
let i_cast_my_vote = "Am depus votul meu"
let please_login_to_confirm = "Vă rugăm să vă logați pentru a confirma votul vostru."
let your_ballot_for = "Buletinul de vot pentru "
let has_been_received = " a fost primit, dar nu a fost încă înregistrat. "
let nobody_can_see = "Notă: buletinul de vot este criptat și nimeni nu-i poate vedea conținutul."
let you_have_already_voted = "Note: you have already voted. Your vote will be replaced."
let go_back_to_election = "Întoarcete la pagina de start a alegerii"
let has_been_accepted = " a fost acceptat."
let you_can_check_its_presence = "Puteți verifica prezența în "
let ballot_box = "urna de vot"
let anytime_during_the_election = " în orice moment al alegerii."
let confirmation_email = " Un e-mail de confirmare v-a fost trimis."
let thank_you_for_voting = "Vă mulțumim pentru participare!"
let is_rejected_because = " este resprins, deoarece "
let fail = "EȘEC!"
let administer_elections = "Administrează alegerile"
let administer_this_election = "Administrează această alegere"
let powered_by = "Realizat de "
let get_the_source_code = "Obține codul sursă"
let audit_data = "Date de audit: "
let parameters = "parametrii"
let public_credentials = "chei de verificare"
let trustee_public_keys = "cheia publică"
let ballots = "buletine de vot"
let election_server = "Server al alegerii"
let accepted_ballots = "Buletinele de vot acceptate"
let ballots_have_been_accepted_so_far = " buletin(e) de vot au fost acceptat(e) până în prezent."
let ballots_have_been_accepted = " buletin(e) de vot au fost acceptat(e)."
let ballots_have_been_accepted_and = " buletin(e) de vot au fost acceptat(e), și "
let have_been_tallied = " au fost contorizate."
let username = "Nume utilizator:"
let password = "Parola:"
let login = "Conectare"
let password_login = "Conectare folosind parola"
let by_using_you_accept = "By using this site, you accept our "
let privacy_policy = "personal data policy"
let accept = "Accept"
let not_yet_open = "Din păcate, această alegere nu este încă deschisă"
let come_back_later = "Acesta alegere nu există încă. Vă rugăm să reveniți mai târziu."
let cookies_are_blocked = "Cookie-urile sunt blocate"
let please_enable_them = "Browser-ul dumneavoastră pare să blocheze cookie-urile. Vă rugăm să le activați."
let election_currently_closed = "Această alegere este în prezent închisă."
let election_closed_being_tallied = "Această alegere este închisă și în curs de contorizare."
let the = " Amprenta "
let encrypted_tally = "rezultatului criptat"
let hash_is = " este "
let election_has_been_tallied = "Această alegere a fost contorizata."
let election_archived = "Această alegere este arhivată."
let number_accepted_ballots = "Numărul buletinelor de vot acceptate: "
let you_can_also_download = "De asemenea, puteți descărca "
let result_with_crypto_proofs = "rezultat cu dovada criptografică"
let blank_vote = "Vot alb"
let no_other_blank = "Nu puteți selecta o altă opțiune la un vot alb"


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


Puteți verifica prezența acestuia în urma de vot, accesibilă la
  %s

Rezultatele vor fi publicate pe pagina de alegere
  %s%s

-- \nBelenios"

let this_vote_replaces = "\n\nThis vote replaces any previous vote."
let please_contact = "To get more information, please contact:"

let error_Serialization : ('a, 'b, 'c, 'd, 'e, 'f) format6 = "your ballot has a syntax error (%s)"
let error_ProofCheck = "some proofs failed verification"
let error_ElectionClosed = "the election is closed"
let error_MissingCredential = "a credential is missing"
let error_InvalidCredential = "your credential is invalid"
let error_RevoteNotAllowed = "you are not allowed to revote"
let error_ReusedCredential = "your credential has already been used"
let error_WrongCredential = "you are not allowed to vote with this credential"
let error_UsedCredential = "the credential has already been used"
let error_CredentialNotFound = "the credential has not been found"
let error_UnauthorizedVoter = "you are not allowed to vote"
