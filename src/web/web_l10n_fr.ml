(**************************************************************************)
(*                                BELENIOS                                *)
(*                                                                        *)
(*  Copyright © 2012-2016 Inria                                           *)
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

let lang = "fr"
let start = "Commencer"
let or_ = "ou"
let submit_a_raw_ballot = "soumettre un bulletin brut"
let see_accepted_ballots = "Voir les bulletins acceptés"
let belenios_booth = "Isoloir Belenios"
let here = "ici"
let question_header = "Question %d/%d — sélectionnez entre %d et %d réponse(s)"
let at_least = "Vous devez sélectionner au moins %d réponse(s)"
let at_most = "Vous devez sélectionner au plus %d réponse(s)"
let previous = "Précédent"
let next = "Suivant"
let nothing = "(rien)"
let enter_cred = "Veuillez entrer votre code de vote :"
let invalid_cred = "Code de vote invalide !"
let input_credential = "Entrée du code de vote"
let answer_to_questions = "Réponse aux questions"
let review_and_encrypt = "Récapitulatif et chiffrement"
let authenticate = "Authentification"
let confirm = "Confirmation"
let done_ = "Terminé"
let booth_step1 = "Étape 1/6 : Entrée du code de vote"
let booth_step2 = "Étape 2/6 : Réponse aux questions"
let booth_step3 = "Étape 3/6 : Récapitulatif et chiffrement"
let booth_step5 = "Étape 5/6 : Confirmation"
let booth_step6 = "Étape 6/6 : "
let input_your_credential = "Entrez votre code de vote "
let wait_while_encrypted = "Veuillez patienter, votre bulletin est en train d'être chiffré..."
let encrypting = "Chiffrement en cours..."
let restart = "Recommencer"
let successfully_encrypted = "Votre bulletin a été chiffré avec succès, "
let not_cast_yet = "mais n'a pas encore été déposé dans l'urne"
let qmark = " !"
let your_tracker_is = "Votre numéro de suivi est "
let we_invite_you_to_save_it = "Nous vous invitons à le sauvegarder afin de vérifier ultérieurement qu'il est bien pris en compte"
let continue = "Continuer"
let election_uuid = "UUID de l'élection : "
let election_fingerprint = "Empreinte de l'élection : "
let i_am = "Je suis "
let and_ = " et "
let i_cast_my_vote = "Je dépose mon bulletin dans l'urne"
let please_login_to_confirm = "Veuillez vous connecter pour confirmer votre vote"
let your_ballot_for = "Votre bulletin pour "
let has_been_received = " a été reçu, mais pas encore pris en compte. "
let nobody_can_see = "Note: votre bulletin est chiffré et personne ne peut voir son contenu."
let go_back_to_election = "Retourner à la page d'accueil de l'élection"
let has_been_accepted = " a été accepté."
let you_can_check_its_presence = "Vous pouvez vérifier sa présence dans l'"
let ballot_box = "urne"
let anytime_during_the_election = " à tout moment pendant l'élection."
let confirmation_email = " Un e-mail de confirmation vous a été envoyé."
let thank_you_for_voting = "Merci pour votre participation !"
let is_rejected_because = " est refusé, parce que "
let fail = "ÉCHEC !"
let logout_and_come_back = "Se déconnecter et revenir à la page d'accueil de l'élection"
let administer_elections = "Administrer des élections"
let administer_this_election = "Administrer cette élection"
let powered_by = "Propulsé par "
let get_the_source_code = "Obtenir le code source"
let audit_data = "Données d'audit : "
let parameters = "paramètres"
let public_credentials = "clés de vérification"
let trustee_public_keys = "clés publiques"
let ballots = "bulletins"
let election_server = "Serveur d'élections"
let accepted_ballots = "Bulletins acceptés"
let ballots_have_been_accepted_so_far = " bulletin(s) ont été accepté(s) jusqu'à présent."
let ballots_have_been_accepted = " bulletin(s) ont été accepté(s)."
let ballots_have_been_accepted_and = " bulletin(s) ont été accepté(s), et "
let have_been_tallied = " ont été compté(s)."
let username = "Nom d'utilisateur :"
let password = "Mot de passe :"
let login = "Se connecter"
let password_login = "Connexion par mot de passe"


let mail_password : ('a, 'b, 'c, 'd, 'e, 'f) format6 =
  "Vous êtes enregistré(e) en tant qu'électeur pour l'élection

  %s

Veuillez trouver ci-dessous votre nom d'utilisateur et votre mot de
passe. Pour soumettre un bulletin, vous aurez également besoin d'un
code de vote, envoyé dans un e-mail séparé. Soyez prudent(e), le mot
de passe et le code de vote se ressemblent mais jouent des rôles
différents. Le système vous demandera votre code de vote dès l'entrée
dans l'isoloir virtuel. Le nom d'utilisateur et le mot de passe sont
nécessaires lorsque votre bulletin est prêt à être soumis.

Nom d'utilisateur : %s
Mot de passe : %s
Page de l'élection : %s

Notez que vous pouvez voter plusieurs fois. Seule la dernière est
prise en compte."


let mail_credential : ('a, 'b, 'c, 'd, 'e, 'f) format6 =
  "Vous êtes enregistré(e) en tant qu'électeur pour l'élection

  %s

Veuillez trouver ci-dessous votre nom d'utilisateur et votre code de
vote. Pour soumettre un bulletin, vous aurez également besoin d'un
mot de passe, envoyé dans un e-mail séparé. Soyez prudent(e), le mot
de passe et le code de vote se ressemblent mais jouent des rôles
différents. Le système vous demandera votre code de vote dès l'entrée
dans l'isoloir virtuel. Le nom d'utilisateur et le mot de passe sont
nécessaires lorsque votre bulletin est prêt à être soumis.

Nom d'utilisateur : %s
Code de vote : %s
Page de l'élection : %s

Notez que vous pouvez voter plusieurs fois. Seule la dernière est
prise en compte."


let mail_confirmation : ('a, 'b, 'c, 'd, 'e, 'f) format6 =
  "%s,

Votre vote pour l'élection

  %s

a été enregistré. Votre numéro de suivi est

  %s

Vous pouvez vérifier sa présence dans l'urne, accessible au

  %s

Les résultats seront publiés sur la page de l'élection

  %s

-- \nBelenios"
