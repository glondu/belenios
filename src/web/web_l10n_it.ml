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

let lang = "it"
let start = "Cominciare"
let advanced_mode = "Modo avanzato"
let see_accepted_ballots = "Consultare le schede elettorali accettate"
let belenios_booth = "Cabina elettorale Belenios"
let here = "qui"
let question_header = "Domanda %d/%d — selezionare tra %d e %d risposta(e)"
let at_least = "Lei deve selezionare almeno %d risposta(e)"
let at_most = "Lei deve selezionare al massimo %d risposta(e)"
let previous = "Precedente"
let next = "Seguente"
let nothing = "(niente)"
let enter_cred = "Si prega di inserire il codice di voto :"
let invalid_cred = "Codice di voto non valido !"
let input_credential = "Inserire il codice di voto"
let answer_to_questions = "Risposta alle domande"
let review_and_encrypt = "Riepilogo e cifratura"
let authenticate = "Autenticazione"
let confirm = "Conferma"
let done_ = "Finito"
let booth_step1 = "Fase 1/6 : Inserire il codice di voto"
let booth_step2 = "Fase 2/6 : Risposta alle domande"
let booth_step3 = "Fase 3/6 : Riepilogo e cifratura"
let booth_step5 = "Fase 5/6 : Conferma"
let booth_step6 = "Fase 6/6 : "
let input_your_credential = "Inserisca il suo codice di voto "
let wait_while_encrypted = "Si prega di pazientare, la cifratura della sua scheda elettorale è in corso..."
let encrypting = "Cifratura in corso..."
let restart = "Ricominciare"
let successfully_encrypted = "La sua scheda elettorale è stata cifrata con successo, "
let not_cast_yet = "ma non è stata ancora depositata nell'urna"
let qmark = " !"
let your_tracker_is = "Il suo codice di verifica è "
let we_invite_you_to_save_it = "La preghiamo di registrarlo per potere verificare ulteriormente se il suo voto è stato preso in considerazione."
let continue = "Proseguire"
let election_uuid = "UUID dell'elezione : "
let election_fingerprint = "Impronta dell'elezione : "
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
let administer_elections = "Amministrare delle elezioni"
let administer_this_election = "Amministrare questa elezione"
let powered_by = "Utilizza "
let get_the_source_code = "Ottenere il codice sorgente"
let audit_data = "Dati d'audit : "
let parameters = "parametri"
let public_credentials = "chiavi di verificazione"
let trustee_public_keys = "chiavi pubbliche"
let ballots = "schede elettorali"
let election_server = "Server delle elezioni"
let accepted_ballots = "Schede elettorali accettate"
let ballots_have_been_accepted_so_far = " scheda(e) accettata(e) finora."
let ballots_have_been_accepted = " scheda(e) accettata(e)."
let ballots_have_been_accepted_and = " scheda(e) accettata(e), e"
let have_been_tallied = " sono state contate"
let username = "Nome utente :"
let password = "Password :"
let login = "Connettersi"
let password_login = "Connessione tramite la password"
let by_using_you_accept = "By using this site, you accept our "
let privacy_policy = "personal data policy"
let accept = "Accettare"
let not_yet_open = "Spiacente, quest'elezione non è ancora aperta"
let come_back_later = "Quest'elezione ancora non esiste. La preghiamo di consultare ulteriormente."
let cookies_are_blocked = "I cookies sono bloccati"
let please_enable_them = "Il suo browser non accetta i cookies. Si prega di attivarli."
let election_currently_closed = "Questa elezione è chiusa per ora."
let election_closed_being_tallied = "L'elezione è chiusa ed il conteggio è in corso."
let the = " L'impronta del "
let encrypted_tally = "risultato cifrato"
let hash_is = " è "
let election_has_been_tallied = "Questa elezione è stata conteggiata."
let election_archived = "Questa elezione è archiviata."
let number_accepted_ballots = "Numero di schede accettate : "
let you_can_also_download = "È possibile scaricare il "
let result_with_crypto_proofs = "risultato con le prove crittografiche"
let blank_vote = "Scheda bianca"
let no_other_blank = "Nessun'altra scelta è autorizzata quando la scheda è bianca"
let it_will_open_in = "It will close in "
let the_election_will_close_in = "The election will close in "
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
let error_UsedCredential = "the credential has already been used"
let error_CredentialNotFound = "the credential has not been found"
let error_UnauthorizedVoter = "you are not allowed to vote"
