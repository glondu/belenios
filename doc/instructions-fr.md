Qui fait quoi dans une élection Belenios ?
=============


Introduction
------------

Belenios propose un système de vote vérifiable. Chaque votant peut
s'assurer que son bulletin est bien dans l'urne, n'importe quel tiers peut
vérifier que le résultat proclamé correspond aux bulletins dans l'urne
et que ceux-ci proviennent d'électeurs légitimes. Le secret du vote
est assuré à travers le partage de la clé de déchiffrement entre
plusieurs autorités (par exemple des membres du comité électoral) avec
un système de seuil (par exemple 3 parmi 5 suffisent à déchiffrer).

Encore faut-il que chacun procède aux vérifications prévues par le
système de vote. Ce document détaille, pour chaque rôle d'une élection
(votant, administrateur, etc.), ce qui doit être fait à chaque étape.


Instructions pour l'électeur
-------------------------

Lorsque l'électeur vote en ligne, son ordinateur chiffre ses choix (à
l'aide d'un programme JavaScript) et affiche à l'électeur un `numéro
de suivi`, qui est une empreinte du bulletin. Ce `numéro de suivi` est
également envoyé par mail lorsque l'électeur a fini de voter.

Pour s'assurer que son bulletin de vote est bien pris en compte,
l'électeur doit s'assurer que son `numéro de suivi` apparait bien dans
l'urne en consultant la page `voir les bulletins acceptés` sur la page
d'accueil de l'élection. L'électeur doit également protester vivement
s'il reçoit un mail de confirmation sans avoir voté ou s'il reçoit un
mail de confirmation avec un `numéro de suivi` différent de celui
affiché à l'écran pendant la phase de vote. Quelqu'un a
probablement réussi à ajouter un bulletin en son nom. Cela peut être par exemple
l'indice d'une attaque par un administrateur système ayant accès au mail de
l'électeur si le login et mot de passe ainsi que le code de vote sont
envoyés sur la même adresse.

Note : un électeur peut voter à nouveau. Seul le *dernier* vote est pris
en compte. L'électeur doit s'assurer que le dernier `numéro de suivi`
reçu est bien dans l'urne. Les numéros de suivi précédents sont
supprimés.

Un électeur peut également vérifier l'intégralité du processus du
vote. C'est à dire qu'au lieu de simplement vérifier la présence de
son bulletin dans l'urne, il peut vérifier la conformité de tous les
bulletins, monitorer l'urne pour vérifier qu'aucun bulletin ne
disparait et enfin s'assurer que le résultat proclamé correspond aux
bulletins dans l'urne. Pour ce faire, il doit suivre les instructions
de l'auditeur.


Instructions pour les autorités de déchiffrement
------------------------------------------

Pendant la préparation de l'élection, il est attendu que l'autorité de
déchiffrement sauvegarde :

- sa clé de déchiffrement (ou clé privée de PKI, en mode threshold)
  (fichier `private_key.json` ou `private_key.txt`). **Cette clé doit
  être conservée dans un lieu sûr** (container chiffré, clé USB placée
  dans un endroit fermé, etc) car elle protège le secret du vote (en
  combinaison avec les autres clés de déchiffrement);
- l'`url` de l'élection;
- (en mode threshold) l'empreinte de sa clé publique de PKI `clé publique`;
- l'empreinte de sa clé vérification associée à sa clé de
  déchiffrement `clé de vérification`.

Dès que l'élection est prête,  il est attendu que l'autorité de
déchiffrement vérifie :

- que sa clé de vérification `clé de vérification`  apparait bien sur la page d'accueil de
l'élection, à côté de son nom.
- (en mode threshold) que sa clé publique de PKI `clé publique`  apparait bien sur la page d'accueil de
l'élection, à côté de son nom;


Après la fermeture de l'élection, l'autorité de déchiffrement
participe au dépouillement. Dans le cas d'une élection de
  type vote alternatif (classement des candidats, ou attribution d'une
  note), le dépouillement commence par une phase de mélange.
  Pour cette étape, il  est attendu que l'autorité de
  déchiffrement :

- sauvegarde l'empreinte de l'urne mélangée : `empreinte de votre mélange`;
- et vérifie immédiatement sa présence sur la page d'accueil de
l'élection  (pour s'assurer que son
  mélange n'a pas été ignoré).

Dans tous les cas, le dépouillement comporte ensuite une étape où
l'autorité de déchiffrement utilise sa clé privée pour procéder à
l'ouverture de l'urne. Il est attendu que l'autorité de
  déchiffrement :

- vérifie que  (seulement dans le mode vote alternatif) l'empreinte de l'urne mélangée à l'étape
  précédente  : `empreinte de votre mélange` apparait sur la page d'accueil de l'élection, à côté du nom
  de l'autorité. La clé de déchiffrement ne doit pas être entrée si ce
  n'est pas le cas.
- sauvegarde l'empreinte de l'urne à déchiffrer : `empreinte du
  résultat chiffré`.

Une fois le dépouillement terminé, les résultats sont proclamés sur la
page d'accueil de l'élection. Il est attendu que l'autorité de
déchiffrement vérifie que les données suivantes apparaissent sur la
page, à chaque fois associées à son nom :

- (en mode threshold) sa clé publique de PKI `clé publique`;
- sa clé de vérification `clé de vérification`;
- (en mode vote alternatif) l'empreinte de son mélange;
- l'empreinte de l'urne à déchiffrer : `empreinte du
  résultat chiffré`
  (pour vérifier que sa clé de déchiffrement n'a pas été utilisée pour
  déchiffrer une autre donnée).


Instructions pour l'autorité de génération de codes de vote
----------------------------------------------------

Pendant la préparation de l'élection, il est attendu que l'autorité de
de génération des codes de vote sauvegarde :

- la liste des codes de vote privés : fichier `creds.txt`. **Cette
  liste doit conservée dans un lieu sûr** (container chiffré, clé USB placée
  dans un endroit fermé, etc) car elle protège contre le bourrage
  d'urne. Elle permet également le renvoi de code de vote en cas de
  perte par l'électeur;
- l'`url` de l'élection;
- la liste électorale `voters.txt`. L'autorité de génération
  de codes de vote doit vérifier
  auprès de la commission électorale que cette liste électorale est
  correcte, ainsi que le nombre de voix attribuées à chaque électeur
  dans le cas d'un vote pondéré;
- l'empreinte de la liste électorale : `Empreinte de la liste électorale`;
- l'empreinte de la liste des codes de vote publics : `Empreinte de la partie publique des codes de vote`.

L'autorité de génération de codes de vote a en charge l'envoi des
codes de vote à chaque électeur. Elle précise alors l'`url` de
l'élection dans le courrier ou mail accompagnant cet envoi. Pour
envoyer les codes de vote, il est possible d'utiliser le programme
  `contrib/send_credentials.py ` fourni dans les sources de Belenios (voir
  section auditeur pour obtenir les sources) en l'éditant au préalable
  pour le paramétrer correctement.

	contrib/send_credentials.py 


Dès que l'élection est ouverte ainsi qu'à la fin de l'élection,  il est attendu que l'autorité de
génération de codes de vote :

- vérifie que le nombre d'électeurs correspond à la liste électorale
  reçue, ainsi que le nombre total de voix dans le cadre d'un vote
  pondéré, et que
  l'empreinte de la liste électorale correspond à l'empreinte
  enregistrée, par exemple avec l'une des commandes décrites [ici](#hash).

- vérifie que l'empreinte de la liste des codes de vote publics
enregistrée correspond à celle affichée à côté de son nom.

- pendant l'élection, l'autorité de génération de codes de vote peut, à
la demande d'un électeur, lui renvoyer son code de vote privé s'il
l'a perdu.

À la fin de l'élection et à des fins de validation,  il est attendu que l'autorité de
génération de codes de vote :

- vérifie que la liste d'émargement donnée par l'administrateur
  correspond à la liste des bulletins dans l'urne. Cette vérification
  peut être effectuée à l'aide de la commande:

      belenios-tool compute-voters --privcreds /path/to/creds.txt --url https://url/to/election

  La liste obtenue doit coïncider (à l'ordre près) avec celle fournie par
  l'administrateur.

Une fois l'élection terminée et validée, il est attendu que l'autorité de
génération de codes de vote :

- détruise le fichier `creds.txt`. En effet, ce fichier permet de faire
  le lien entre un électeur et son bulletin (chiffré). Ce lien
  pourrait compromettre l'anonymat du vote à long terme, par exemple
  si les clés de chiffrement utilisées deviennent trop faibles pour la
  puissance de calcul dans le futur (ou ordinateur quantique, ...).

Instructions pour la commission électorale
-------------------------------------

A minima, la commission électorale consulte la page d'accueil de l'élection dès
qu'elle ouverte et vérifie que :

- le nombre d'électeurs affiché correspond à la liste électorale;

- la valeur `Empreinte de la liste électorale` affichée correspond 
  à l'empreinte de la liste électorale `voters.txt` fournie
  (par le système informatique ou l'administrateur de l'élection). Le
  calcul de l'empreinte peut être fait avec l'une des commandes
  décrites [ici](#hash).

- la liste électorale `voters.txt` correspond bien aux électeurs
  légitimes, avec le nombre de voix associé dans le cadre d'un vote pondéré.

- la liste des questions et des réponses correspond bien à ce qui a
  été déterminé pour ce scrutin. Les questions et les réponses
  associées apparaissent dans le fichier `election.json`. Ce fichier peut
  être obtenu en cliquant sur
  `paramètres` dans le bandeau en bas de la page d'accueil de l'élection.

Idéalement, la commission électorale accomplit également le travail de
l'auditeur ou mandate une personne pour le faire (des services
informatiques par exemple).


Instructions pour l'auditeur
-------------------------

Tout le monde connaissant l'`url` de l'élection peut être auditeur. La sécurité de Belenios repose sur
le fait que les vérifications décrites ci-dessous sont effectuées par
au moins une personne de confiance. Pour effectuer ces tests, des
logiciels sont nécessaires. Nous décrivons ici comment exécuter les
vérifications en utilisant `belenios-tool` dont les sources sont
disponibles à partir du [Gitlab Inria](https://gitlab.inria.fr/belenios/belenios) et qui est installable sous Linux Debian/Ubuntu avec `sudo apt install belenios-tool`.

Note : ces vérifications sont effectuées de façon automatique par nos
serveurs pour les élections mises en place avec un niveau de sécurité
maximal (générateur de code tiers et au moins deux autorités de
déchiffrement extérieures).

Pendant et après l'élection, l'auditeur a accès aux fichiers suivants (depuis
la page d'accueil de l'élection) :

 * `election.json`: paramètres de l'élection;
 * `trustees.json`: clés de vérification des autorités de
   déchiffrement ainsi que leurs clés publiques en mode threshold;
 * `public_creds.txt`: parties publiques des codes de vote;
 * `ballots.jsons`: bulletins acceptés.

Pendant l'élection, il est attendu que l'auditeur :

- vérifie que le nombre d'électeurs affiché est conforme et que l'empreinte de la liste
électorale correspond à la liste électorale (s'il a
accès à ces informations). Cf instructions pour la commission
électorale.

- vérifie que le nombre d'électeurs est égal au nombre de codes de
vote publics dans `public_creds.txt`.

- vérifie que le fichier des parties publiques des codes de vote
  `public_creds.txt` correspond à l'empreinte des codes de vote
  affichée sur la page d'accueil, par exemple en utilisant l'une des commandes suggérées [ici](#hash).

- vérifie la cohérence de l'urne. En copiant les 4 fichiers listés
ci-dessus dans un répertoire `/path/to/election`, la commande suivante
exécute toutes les vérifications nécessaires :

      belenios-tool verify --dir /path/to/election

- vérifie que l'urne évolue de façon cohérente : aucun bulletin ne
  disparait à moins qu'il soit remplacé par un bulletin provenant du
  même code de vote (cas de revote d'un électeur). Pour cela, télécharger
  de nouveau les fichiers dans un autre répertoire `/path/to/election/new`
  et lancer la commande :

      belenios-tool verify-diff --dir1 /path/to/election --dir2 /path/to/election/new

- vérifie que la page web servie aux électeurs pour voter ainsi que
  ses ressources (images, style css, code JavaScript) ne change pas.
  Le code doit correspondre à celui obtenu après
  compilation des sources de Belenios. Le programme
  `contrib/check_hash.py` fourni dans les sources fait ceci
  automatiquement:
      
      contrib/check_hash.py --url https://url/to/server

  Notons que l'url est celle du serveur et non celle de l'élection ; par
  exemple `--url https://belenios.loria.fr`.

Après l'élection, l'auditeur a également accès au fichier
`result.json`. Il est attendu que  l'auditeur :

- exécute à nouveau les deux vérifications expliquées ci-dessus pour
  s'assurer de la cohérence de l'urne finale et de sa cohérence
  vis-à-vis du dernier enregistrement de l'urne par l'auditeur.
- vérifie que le résultat indiqué dans le fichier `result.json`
  correspond au résultat affiché sur la page d'accueil de l'élection.
  Actuellement, cette vérification doit être faite manuellement.
- vérifie que les données de contrôle publiées dans ces fichiers
  correspondent à ce qui est affiché sur la page d'accueil de
  l'élection et lu par les électeurs et les autres acteurs de
  l'élection.

Pour ce dernier point ainsi que toutes les autres tâches de l'auditeur
(sauf la vérification du résultat affichée),
un outil est fourni dans les sources de Belenios. Cet outil suppose que
`belenios-tool` soit compilé et installé. Ensuite, l'auditeur doit créer
un dossier `workdir` où seront stockées les informations d'audit d'une
élection au fur et à mesure des téléchargements sous forme d'un dépôt
`git`. Pendant toute la phase d'audit, il faut lancer régulièrement la
commande :

      contrib/monitor_elections.py --uuid <uuid_de_l_election> --url https://url/to/server --wdir <workdir>

ce qui télécharge les données, les vérifie, et les compare avec les
données précédemment téléchargées.

Il est possible de rediriger les messages avec l'option `--logfile`.
Alors, seules les anomalies seront rapportées sur `stdout/stderr`, ce qui
permet de lancer la commande depuis un `crontab` et d'être alerté en cas
de problème.


Note : Si l'outil en ligne de commande `belenios-tool` est utilisé, la
confiance dans les tests effectués repose en partie dans la confiance
en l'outil. Il est possible d'implémenter son propre logiciel de
vérification à partir des spécifications de Belenios, disponibles [ici](https://www.belenios.org/specification.pdf).


Instructions pour l'administrateur de l'élection
-----------------------------------------

Cela peut sembler curieux mais l'administrateur de l'élection a peu de
vérifications à effectuer. Cela s'explique par le fait que le système
de vote Belenios est conçu de manière à être sûr sans faire confiance
à l'administrateur de l'élection. La sécurité de Belenios repose à
l'inverse sur les vérifications mutuelles des différents acteurs :
autorités de déchiffrement, générateur de code de vote, commission
électorale et auditeur.

Les points importants pour l'administrateur sont les suivants :

- obtenir la liste électorale sous la forme d'une liste d'adresses
  mail valides, une adresse par électeur. Dans le cas d'un vote
  pondéré, un nombre de voix différent peut être attribué à chaque
  électeur. Cette liste doit être
  validée par la commission électorale.
- vérifier et revérifier les adresses email rentrées pour les
  électeurs avant de lancer l'élection (et même avant l'envoi des
  codes en vote en mode automatique). Il n'est pas possible de
  modifier ces adresses ensuite et il n'y a pas d'alerte en cas de
  problème d'envoi.
-  s'assurer que tous les acteurs utilisent bien la même `url` pour
l'élection.
- si l'administrateur de l'élection n'a pas chargé une
personne de générer les codes de vote, il doit penser à télécharger
la liste des codes de vote (`Télécharger les parties privées des codes de vote`) pour
pouvoir renvoyer son code de vote à un électeur qui l'aurait
perdu. D'un point de vue sécurité, il est cependant préférable de
déléguer la génération des codes de vote à une tierce personne.

Pour un niveau de sécurité maximal, l'administrateur de l'élection
doit disposer :

- d'une personne en charge de générer les codes de vote et les envoyer par mail
  aux électeurs (par défaut, c'est fait par la plateforme de vote, ce
  qui prête plus le flanc à une attaque de type bourrage d'urne).
- de plusieurs autorités de déchiffrement en charge de protéger le
  secret du vote : il faut les attaquer toutes (ou un quorum d'entre
  elles) pour être capable de déchiffrer individuellement les bulletins.

<a name="hash"></a>Comment calculer l'empreinte d'un fichier ?
---------------------------------------------------------

Pour calculer l'empreinte d'un fichier, vous devez utiliser la même
fonction de hachage que celle utilisée dans Belenios. Nous proposons
ici deux solutions pour calculer cette empreinte en ligne de
commande. Nous utilisons le fichier `voters.txt` en exemple mais vous
pouvez bien sûr le remplacer par un autre fichier.

        sha256sum voters.txt | xxd -p -r | base64 | tr -d "="

  (ou bien `shasum -a256` au lieu de `sha256sum` par exemple sur
  MacOS)

ou encore :

        cat voters.txt | python3 -c "import hashlib,base64,sys;m=hashlib.sha256();m.update(sys.stdin.read().encode());print(base64.b64encode(m.digest()).decode().strip('='))"
