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

let lang = "de"
let start = "Start"
let advanced_mode = "Erweiterter Modus"
let see_accepted_ballots = "angenommene Stimmen anzeigen"
let belenios_booth = "Belenios Wahlkabine"
let here = "hier"
let question_header = "Frage #%d von %d — wählen Sie zwischen %d und %d Antworten aus"
let at_least = "Sie müssen mindestens %d Antworten auswählen"
let at_most = "Sie müssen maximal %d Antworten auswählen"
let previous = "Zurück"
let next = "Weiter"
let nothing = "(nichts)"
let enter_cred = "Bitte geben Sie Ihre Wählernummer ein:"
let invalid_cred = "Falsche Wählernummer!"
let input_credential = "Wählernummer eingeben"
let answer_to_questions = "Fragen beantworten"
let review_and_encrypt = "Überprüfen und verschlüsseln"
let authenticate = "Authentifizieren"
let confirm = "Bestätigen"
let done_ = "Fertig"
let booth_step1 = "Schritt 1/6: Wählernummer eingeben"
let booth_step2 = "Schritt 2/6: Fragen beantworten"
let booth_step3 = "Schritt 3/6: Überprüfen und verschlüsseln"
let booth_step5 = "Schritt 5/6: Bestätigen"
let booth_step6 = "Schritt 6/6: "
let input_your_credential = "Bitte Wählernummer eingeben "
let wait_while_encrypted = "Bitte warten, Ihre Stimme wird verschlüsselt..."
let encrypting = "Verschlüssele..."
let restart = "Erneut beginnen"
let successfully_encrypted = "Ihre Stimme wurde erfolgreich verschlüsselt, "
let not_cast_yet = "aber noch nicht abgeschickt"
let qmark = "!"
let your_tracker_is = "Ihre Stimmennummer ist "
let we_invite_you_to_save_it = "Bitte Speichern Sie sie ab um später zu überprüfen, dass Ihre Stimme gezählt wurde."
let continue = "Weiter"
let election_uuid = "Eindeutige Nummer der Abstimmung: "
let election_fingerprint = "Fingerabdruck der Abstimmung: "
let i_am = "Ich bin "
let and_ = " und "
let i_cast_my_vote = "ich schicke meine Stimme ab"
let please_login_to_confirm = "Bitte melden Sie sich an um Ihre Stime zu bestätigen."
let your_ballot_for = "Ihre Stimme für "
let has_been_received = " wurde empfangen, aber noch nicht gespeichert. "
let nobody_can_see = "Hinweis: Ihre Stimme ist verschlüsselt und niemand kann ihren Inhalt sehen."
let you_have_already_voted = "Note: you have already voted. Your vote will be replaced."
let go_back_to_election = "Zurück zur Wahl"
let has_been_accepted = " wurde angenommen."
let you_can_check_its_presence = "Sie können jederzeit überprüfen, dass Ihre Stimme in der "
let ballot_box = "Wahlurne"
let anytime_during_the_election = " vorhanden ist."
let confirmation_email = " Sie erhalten eine Bestätigung per E-Mail."
let thank_you_for_voting = "Vielen Dank für Ihre Stimme!"
let is_rejected_because = " wurde abgelehnt, da "
let fail = "FEHLER!"
let administer_elections = "Abstimmung verwalten"
let administer_this_election = "Diese Abstimmung verwalten"
let powered_by = "Powered by "
let get_the_source_code = "Den Quellcode herunterladen"
let audit_data = "Auditdaten: "
let parameters = "Parameter"
let public_credentials = "Öffentliche Daten"
let trustee_public_keys = "Öffentliche Schlüssel der Treuhänder"
let ballots = "Stimmen"
let election_server = "Wahlserver"
let accepted_ballots = "Angenommene Stimmen"
let ballots_have_been_accepted_so_far = " Stimmen wurden bis jetzt angenommen."
let ballots_have_been_accepted = " Stimmen wurden angenommen."
let ballots_have_been_accepted_and = " Stimmen wurden angenommen, und "
let have_been_tallied = " wurden gezählt."
let username = "Benutzername:"
let password = "Passwort:"
let login = "Login"
let password_login = "Login mit Passwort"
let by_using_you_accept = "By using this site, you accept our "
let privacy_policy = "personal data policy"
let accept = "Bestätigen"
let not_yet_open = "Entschuldigung, die Abstimmung ist noch nicht geöffnet."
let come_back_later = "Diese Abstimmung gibt es noch nicht. Bitte kommen Sie später wieder."
let cookies_are_blocked = "Cookies sind deaktiviert"
let please_enable_them = "Ihr Browser nimmt keine Cookies an, bitte aktivieren Sie diese."
let election_currently_closed = "Diese Abstimmung ist beendet."
let election_closed_being_tallied = "Diese Abstimmung ist beendet und wird ausgezählt."
let the = "Der Hash des "
let encrypted_tally = "verschlüsselten Ergebnisses"
let hash_is = " ist "
let election_has_been_tallied = "Diese Abstimmung wurde ausgezählt."
let election_archived = "Diese Abstimmung wurde archiviert."
let number_accepted_ballots = "Anzahl angenommener Stimmen: "
let you_can_also_download = "Sie können außerdem das "
let result_with_crypto_proofs = "Ergbnis mit den kryptographischen Beweisen herunterladen"
let blank_vote = "ungültige Stimme"
let no_other_blank = "Bei einer ungültigen Stimme sind keine anderen Auswahlmöglichkeiten vorhanden."


let mail_password_subject : ('a, 'b, 'c, 'd, 'e, 'f) format6 =
  "Ihr Passwort für die Abstimmung %s"

let mail_password : ('a, 'b, 'c, 'd, 'e, 'f) format6 =
  "Sie sind für die folgende Abstimmung als Wähler eingetragen:

  %s

Am Ende der Mail finden Sie Ihren Benutzername und Ihr Passwort. Um
abzustimmen benötigen sie außerdem noch Ihre Wählernummer, die Ihnen
in einer seperaten Mail zugestellt wird. Obwohl Passwort und
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
benötigen sie außerdem noch Ihr Passwort, die Ihnen in einer seperaten
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
