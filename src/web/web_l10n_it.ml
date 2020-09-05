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

let lang = "it"
let booth_step5 = "Fase 5/6 : Conferma"
let booth_step6 = "Fase 6/6 : "
let i_am = "Sono "
let and_ = " e "
let i_cast_my_vote = "depongo la mia scheda elettorale nell'urna"
let please_login_to_confirm = "La preghiamo di connettersi per confermare il suo voto"
let your_ballot_for = "La sua scheda elettorale per "
let has_been_received = " è stata ricevuta, ma non è ancora presa in considerazione. "
let nobody_can_see = "Nota: la sua scheda è cifrata e nessuno può consultarla."
let you_have_already_voted = "Note: you have already voted. Your vote will be replaced."
let go_back_to_election = "Tornare alla pagina iniziale dell'elezione"
let has_been_accepted = " è stata accettata."
let you_can_check_its_presence = "È possibile verificare la sua presenza  nell'"
let ballot_box = "urna"
let anytime_during_the_election = " ad ogni momento durante l'elezione."
let confirmation_email = " Le è stata spedita una email di conferma."
let thank_you_for_voting = "La ringraziamo per la sua partecipazione !"
let is_rejected_because = "è rifiutato, perché"
let fail = "FALLIMENTO !"
let audit_data = "Dati d'audit : "
let parameters = "parametri"
let public_credentials = "chiavi di verificazione"
let ballots = "schede elettorali"
let accepted_ballots = "Schede elettorali accettate"
let ballots_have_been_accepted_so_far = " scheda(e) accettata(e) finora."
let ballots_have_been_accepted = " scheda(e) accettata(e)."
let ballots_have_been_accepted_and = " scheda(e) accettata(e), e"
let have_been_tallied = " sono state contate"
let username = "Nome utente :"
let password = "Password :"
let login = "Connettersi"
let password_login = "Connessione tramite la password"
let not_found = "Not found"
let election_does_not_exist = "This election does not exist. This may happen for elections that have not yet been open or have been deleted."
let cookies_are_blocked = "I cookies sono bloccati"
let please_enable_them = "Il suo browser non accetta i cookies. Si prega di attivarli."
let the_raw_results = "The raw results can be viewed in the "
let json_result = "JSON result"
let it_contains_all_clear = ". It contains all submitted ballots in clear, in random order. It is up to you to apply your favorite counting method (e.g. Condorcet, STV, majority judgement)."
let years = " year(s)"
let months = " month(s)"
let days = " day(s)"
let hours = " hour(s)"
let minutes = " minute(s)"
let seconds = " second(s)"


let mail_password_subject : ('a, 'b, 'c, 'd, 'e, 'f) format6 =
  "La sua password per l'elezione %s"

let mail_password : ('a, 'b, 'c, 'd, 'e, 'f) format6 =
  "Lei è registrato(a) in quanto elettore(trice) per l'elezione

  %s


Si prega di trovare qui sotto il suo nome di utente e la sua password.
Per presentare una scheda elettorale, avrà bisogno di un codice di
voto, spedito in una email separata. Faccia attenzione, la password e
il codice di voto sono simili ma hanno un ruolo diverso. Il sistema le
domanderà il suo codice di voto non appena entrato(a) nella cabina
elettorale virtuale. Il nome di utente e la password sono necessari
quando la sua scheda è pronta per essere presentata.


Nome utente : %s
Password : %s
Pagina dell'elezione : %s

Si nota che lei può votare più volte. Ma soltanto l'ultimo voto è
preso in considerazione.%s"


let mail_credential_subject : ('a, 'b, 'c, 'd, 'e, 'f) format6 =
  "Il suo codice di voto per l'elezione %s"

let mail_credential : ('a, 'b, 'c, 'd, 'e, 'f) format6 =
  "Lei è registrato(a) in quanto elettore(trice) per l'elezione

  %s

%s

Codice di voto : %s
Pagina dell'elezione : %s

Si nota che lei può votare più volte. Ma soltanto l'ultimo voto è
preso in considerazione.%s"

let mail_credential_password =
"Si prega di trovare qui sotto il suo codice di voto.  Per presentare
una scheda elettorale, avrà bisogno di una password, spedita in una
email separata. Faccia attenzione, la password e il codice di voto
sono simili ma hanno un ruolo diverso. Il sistema le domanderà il suo
codice di voto non appena entrato(a) nella cabina elettorale
virtuale. Il nome di utente e la password sono necessari quando la sua
scheda è pronta per essere presentata."

let mail_credential_cas =
"Si prega di trovare qui sotto il suo codice di voto."

let mail_confirmation_subject : ('a, 'b, 'c, 'd, 'e, 'f) format6 =
  "La sua scheda per l'elezione %s"

let mail_confirmation : ('a, 'b, 'c, 'd, 'e, 'f) format6 =
  "%s,

La sua scheda per l'elezione

  %s

è stata registrata. Il suo codice di verifica è

  %s%s


Può verificare la sua presenza nell'urna, accessibile su
  %s

I risultati saranno pubblicati sulla pagina dell'elezione
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
let error_UnauthorizedVoter = "you are not allowed to vote"
