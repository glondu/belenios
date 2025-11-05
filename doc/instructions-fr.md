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

Une notion-clé dans les instructions qui suivent est l'`url` à laquelle
un participant se connecte. Nous utiliserons la notion de `PREFIXE` et de
`UUID`, où `PREFIXE` donne l'`url` du serveur Belenios qui héberge
l'élection, et `UUID` identifie l'élection dont on parle, parmi les
autres élections qui sont gérées sur ce serveur. Typiquement, si la page
principale de l'élection est de la forme

    https://vote.example.org/election#8GVH85AoSyweXG

alors `PREFIXE=https://vote.example.org` et `UUID=8GVH85AoSyweXG`.

Instructions pour l'électeur
-------------------------

Avant le début de l'élection, l'électeur reçoit un mail avec son `code
de vote` et l'`url` de l'élection. La page de l'élection affiche
l'heure d'ouverture lorsqu'elle n'est pas encore ouverte.

Pendant l'élection, l'électeur peut se rendre sur la page de
l'élection et voter de la manière suivante :

- l'électeur saisit son `code de vote`. Cette étape peut être faite
  automatiquement, si l'`url` de l'élection reçue par l'électeur
  a été personnalisée avec le `code de vote` (cas d'un envoi de codes
  de vote par le serveur notamment).
- il a alors accès aux questions de l'élection et sélectionne ses
candidats
- l'ordinateur chiffre  ses choix (à
l'aide d'un programme JavaScript) et affiche à l'électeur un `numéro
de suivi`, qui est une empreinte du bulletin. Ce `numéro de suivi` est
également envoyé par mail lorsque l'électeur a fini de voter.
- une fois que l'électeur a vérifié ses choix, il est invité à
  s'authentifier. Il reçoit alors un
  mot de passe éphémère à son adresse email, qu'il saisit dans l'interface de
  vote. D'autres moyens d'authentification sont possibles (par exemple
  un envoi préalable d'un mot de passe suivant les élections).
- note : un électeur peut voter à nouveau. Seul le *dernier* vote est pris
en compte.

Un tutoriel vidéo est disponible <a
href="tutorial-voter.html">en ligne</a>.

Le système de vote Belenios est *vérifiable*.

- l'électeur peut 
s'assurer que son bulletin de vote est bien pris en compte,
en vérifiant que son `numéro de suivi` apparait bien dans
l'urne, en consultant la page `voir les bulletins acceptés` sur la page
d'accueil de l'élection. Il doit protester si ce n'est pas le cas.
Si l'électeur vote plusieurs fois, seul son dernier `numéro de suivi` apparait.

- l'électeur doit également protester vivement
s'il reçoit un mail de confirmation sans avoir voté ou s'il reçoit un
mail de confirmation avec un `numéro de suivi` différent de celui
affiché à l'écran pendant la phase de vote. Quelqu'un a
probablement réussi à ajouter un bulletin en son nom. Cela peut être par exemple
l'indice d'une attaque par un administrateur système ayant accès au mail de
l'électeur si le mot de passe et le code de vote sont
envoyés sur la même adresse.



Un électeur peut également vérifier l'intégralité du processus du
vote. C'est à dire qu'au lieu de simplement vérifier la présence de
son bulletin dans l'urne, il peut vérifier la conformité de tous les
bulletins, monitorer l'urne pour vérifier qu'aucun bulletin ne
disparait et enfin s'assurer que le résultat proclamé correspond aux
bulletins dans l'urne. Pour ce faire, il doit suivre les instructions
de l'auditeur.


Instructions pour les autorités de déchiffrement
------------------------------------------

Pendant la préparation de l'élection, chaque autorité de déchiffrement
est invitée à générer une clé de déchiffrement en suivant un lien
envoyé par l'administrateur de l'élection. L'autorité doit suivre le
lien, et s'assurer que l'URL de la page qu'elle obtient a une des
formes suivantes (où PREFIXE et UUID sont comme définis ci-dessus, et
JETON est une chaîne de caractères d'apparence aléatoire) :

- `PREFIXE/trustee#generate/UUID/JETON`

De plus, il est attendu que l'autorité de déchiffrement sauvegarde :

- sa clé de déchiffrement (ou clé privée de PKI, en mode threshold)
  (fichier `private_key.json` ou `private_key.txt`). **Cette clé doit
  être conservée dans un lieu sûr** (container chiffré, clé USB placée
  dans un endroit fermé, etc) car elle protège le secret du vote (en
  combinaison avec les autres clés de déchiffrement);
- l'`url` de l'élection;
- (en mode threshold) l'empreinte de sa clé publique de PKI `clé publique`;
- l'empreinte de sa clé vérification associée à sa clé de
  déchiffrement `clé de vérification`.

Pour s'assurer que les autorités de déchiffrement ont correctement
généré leur clé, l'administrateur de l'élection peut, avant le début
de l'élection, demander à chaque autorité de vérifier qu'elle possède
bien sa clé de déchiffrement. Dans ce cas, l'autorité de déchiffrement
reçoit une URL de la forme

    PREFIXE/trustee#check/UUID

et est invitée à entrer sa clé de déchiffrement. L'autorité doit
vérifier que l'`UUID` de l'élection affichée est correcte et que c'est
bien son nom qui apparait après avoir cliqué sur `Vérifier la clé
privée`. L'autorité de déchiffrement ne doit *jamais* entrer sa clé de
déchiffrement en dehors de cette étape de vérification (avec l'URL
indiquée ci-dessus) et de l'étape de dépouillement (voir ci-dessous).

Dès que l'élection est prête,  il est attendu que l'autorité de
déchiffrement vérifie :

- que sa clé de vérification `clé de vérification`  apparait bien sur la page d'accueil de
l'élection, à côté de son nom.
- (en mode threshold) que sa clé publique de PKI `clé publique`  apparait bien sur la page d'accueil de
l'élection, à côté de son nom;


Après la fermeture de l'élection, l'autorité de déchiffrement
participe au dépouillement. Dans le cas d'une élection de type vote
alternatif (classement des candidats, ou attribution d'une note), le
dépouillement commence par une phase de mélange.  Pour cette étape, il
est attendu que l'autorité de déchiffrement :

- vérifie que l'URL de la page a la forme suivante:
  `PREFIXE/trustee#shuffle/UUID/JETON`
- sauvegarde l'empreinte de l'urne mélangée : `empreinte de votre mélange`;
- et vérifie immédiatement sa présence sur la page d'accueil de
  l'élection  (pour s'assurer que son mélange n'a pas été ignoré).

Dans tous les cas, le dépouillement comporte ensuite une étape où
l'autorité de déchiffrement utilise sa clé privée pour procéder à
l'ouverture de l'urne. Il est attendu que l'autorité de
déchiffrement :

- vérifie que l'URL de la page a la forme suivante:
  `PREFIXE/trustee#decrypt/UUID/JETON`
- vérifie que  (seulement dans le mode vote alternatif) l'empreinte de
  l'urne mélangée à l'étape précédente : `empreinte de votre mélange`
  apparait sur la page d'accueil de l'élection, à côté du nom de
  l'autorité. La clé de déchiffrement ne doit pas être entrée si ce n'est
  pas le cas.
- sauvegarde l'empreinte de l'urne à déchiffrer : `empreinte du
  résultat chiffré`.

Une fois le dépouillement terminé, les résultats sont proclamés sur la
page d'accueil de l'élection. Il est attendu que l'autorité de
déchiffrement :

- **détruise** sa clé de déchiffrement
- vérifie que les données suivantes apparaissent sur la
page, à chaque fois associées à son nom :

	- (en mode threshold) sa clé publique de PKI `clé publique`;
	- sa clé de vérification `clé de vérification`;
	- (en mode vote alternatif) l'empreinte de son mélange;
	- l'empreinte de l'urne à déchiffrer : `empreinte du résultat chiffré`
  (pour vérifier que sa clé de déchiffrement n'a pas été utilisée pour
  déchiffrer une autre donnée).


Instructions pour l'autorité de génération de codes de vote
----------------------------------------------------

Le rôle principal de l'autorité de génération des codes de vote est de
générer et d'envoyer un code de vote à chaque électeur.

**Préparation.** Pendant la préparation de l'élection, l'autorité
reçoit un lien privé de la part de l'administrateur de
l'élection. Elle doit suivre ce lien, et s'assurer que l'URL de la
page qu'elle obtient a la forme suivante :
`PREFIXE/credauth#generate/UUID/JETON`.

Sur cette page se trouve la liste des électeurs. L'autorité doit
vérifier avec le comité en charge de l'élection que cette liste est
correcte, ainsi que les poids de chaque électeur en cas de vote à
poids ;

L'autorité a alors deux options pour générer les codes de vote:

- soit cliquer sur `Générer` dans son navigateur ;
- soit :
  - copier la liste électorale dans un fichier `voters.txt` ;
  - nommer `$UUID` l'identifiant de l'élection (le dernier composant de
    l'URL de l'élection, donnée par la page) ;
  - exécuter la commande :

        belenios-tool setup generate-credentials --file voters.txt --group Ed25519 --uuid $UUID

    Elle génère deux fichiers, `$TIMESTAMP.privcreds` et
    `$TIMESTAMP.pubcreds`, et l'`empreinte des codes de vote publics` ;
  - soumettre le fichier `.pubcreds` avec le formulaire `Soumettre via
    un fichier` ;
  - sauvegarder le fichier `.privcreds` en tant que `creds.json`.

La seconde option devrait être préférée pour plus de sécurité, en
particulier s'il n'y a pas d'auditeur en charge de surveiller le
serveur.

Pendant cette étape, il est attendu que l'autorité de de génération
des codes de vote sauvegarde :

- la liste des codes de vote privés : fichier `creds.txt`. **Cette
  liste doit conservée dans un lieu sûr** (container chiffré, clé USB
  placée dans un endroit fermé, etc) car elle protège contre le
  bourrage d'urne. Elle permet également le renvoi de code de vote en
  cas de perte par l'électeur;
- l'`url` de l'élection ;
- la liste électorale `voters.txt` ;
- l'empreinte de la liste électorale : `Empreinte de la liste
  électorale` ;
- l'empreinte de la liste des codes de vote publics : `Empreinte de la
  partie publique des codes de vote`.

L'autorité de génération de codes de vote a en charge l'envoi des
codes de vote à chaque électeur. Elle précise alors l'`url` de
l'élection dans le courrier ou mail accompagnant cet envoi. Pour
envoyer les codes de vote, il est possible d'utiliser le programme
`contrib/send_credentials.py ` fourni dans les sources de Belenios
(voir section auditeur pour obtenir les sources) en l'éditant au
préalable pour le paramétrer correctement.

    contrib/send_credentials.py

Ce programme construit en particulier un mail type, que l'autorité de
génération de codes peut personnaliser. Par contre, la présentation
des éléments techniques (`url` de l'élection et `code de vote` de
l'électeur séparés) ne doit pas être modifiée sans refaire une analyse de sécurité.

**Phase de vote.** Dès que l'élection est ouverte ainsi qu'à la fin de
l'élection, il est attendu que l'autorité de génération de codes de
vote :

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

**Après le dépouillement.** À la fin de l'élection et à des fins de
validation, il est attendu que l'autorité de génération de codes de
vote :

- vérifie que la liste d'émargement donnée par l'administrateur
  correspond à la liste des bulletins dans l'urne. Cette vérification
  peut être effectuée à l'aide de la commande:

      belenios-tool election compute-voters --privcreds /path/to/creds.json --url https://url/to/election

  La liste obtenue doit coïncider (à l'ordre près) avec celle fournie par
  l'administrateur.

Une fois l'élection terminée et validée, il est attendu que l'autorité de
génération de codes de vote :

- détruise le fichier `creds.json`. En effet, ce fichier permet de faire
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
  associées apparaissent dans le fichier `$UUID.bel`. Ce fichier peut
  être obtenu en cliquant sur `données publiques`
  dans le bandeau en bas de la page d'accueil de l'élection.

Idéalement, la commission électorale accomplit également le travail de
l'auditeur ou mandate une personne pour le faire (des services
informatiques par exemple).


Instructions pour l'auditeur
-------------------------

Tout le monde connaissant l'`url` de l'élection peut être auditeur.
L'`url` d'une élection est de la forme
`PREFIXE/election#UUID`, où, par exemple,
`PREFIXE=https://vote.example.org` et `UUID=8GVH85AoSyweXG`.

Un auditeur va, en particulier, assurer que :

- les données de l'élection (les clés publiques, la liste des codes de
  vote publics, etc) sont cohérentes et ne changent pas au cours du
  temps ;

- l'urne, qui contient les votes chiffrés, évolue de manière cohérente :
  aucun bulletin n'est retiré à moins que ça soit un bulletin avec le
  même code de vote (cela correspond à un revote) ;

- l'urne ne contient que des bulletins bien formés (avec des preuves
  zero-knowledge valides, et un code de vote valide) ;

- l'intégrité des fichiers actifs (HTML, Javascript, etc.) utilisés par
  les électeurs et les autorités est préservée ;

- le résultat de l'élection correspond aux bulletins chiffrés, grâce aux
  preuves zero-knowledge de bon déchiffrement produites par les autorités
  de déchiffrement.

La sécurité de Belenios s'appuie sur le fait que les vérifications
décrites ci-dessous sont effectuées par au moins une personne honnête.

Note: ces vérifications sont aussi effectuées automatiquement par nos
serveurs pour les élections qui sont mises en place avec un niveau de
sécurité maximal (autorité de code de vote externe et au moins deux
autorités de déchiffrement externes).

**Préparation**
Pour effectuer ces tests, des
logiciels sont nécessaires. Nous décrivons ici comment exécuter les
vérifications en utilisant `belenios-tool` dont les sources sont
disponibles à partir du [Gitlab Inria](https://gitlab.inria.fr/belenios/belenios) et qui est installable sous Linux Debian/Ubuntu avec `sudo apt install belenios-tool`.
Ensuite, l'auditeur doit se créer un dossier de travail `workdir` où les
données d'audit de l'élection seront sauvegardées au fur et à mesure des
téléchargements, sous la forme d'un dépôt `git`.

Afin de vérifier que les codes HTML/Javascript utilisés par les
électeurs, les autorités de déchiffrement et l'autorité de code de vote,
ne sont pas modifiés par un serveur corrompu, l'auditeur doit trouver le
"bon" code de tous ces programmes. Il doit ensuite s'assurer que le
serveur fournit ces fichiers de manière fidèle. Tout d'abord, un fichier
de référence doit être créé. Pour cela, on copie celui des sources de
Belenios :

    cp path/to/sources/belenios/contrib/reference_template.json workdir/hashref

Ensuite, il y a plusieurs solutions pour assurer que les fichiers servis
par le serveur sont valides, lorsque l'on audite l'élection identifiée
par UUID :

- soit l'auditeur fait simplement confiance aux fichiers téléchargés la
  première fois et vérifie qu'ils ne varient pas au cours du temps (principe
  TOFU](https://en.wikipedia.org/wiki/Trust_on_first_use)). Alors la
  commande d'audit est la suivante :

        ./monitor_elections.py --url PREFIX --wdir workdir --checkhash yes --hashref workdir/hashref --outputref workdir/hashref --uuid UUID

  Chaque fois qu'un fichier change (y compris lors de la première
  exécution), cela va afficher un message d'alerte.

- soit l'auditeur récupère les sources, recompile le code, démarre un
  serveur local, et utilise la commande précédente pour remplir le
  fichier `workdir/hashref` avec des données de confiance. Puis il peut
  copier ce fichier comme référence pour l'audit de la vraie élection qui
  est hébergée sur le serveur externe. La commande est alors la même que
  ci-dessus.

- soit l'auditeur fait confiance à une personne bien identifiée qui a
  publié une version signée avec gpg du fichier de référence. Dans ce
  cas, des arguments supplémentaires doivent être passés au programme
  d'audit : l'url de cette version signée, ainsi qu'un trousseau de clé
  gpg contenant la clé publique de la personne, en tant que clé de
  confiance. Dans le cas de notre plateforme de vote, un tel fichier est
  fourni par le développeur principal de Belenios, Stéphane Glondu. Nous
  donnons la ligne de commande correspondante, devant être adaptée pour
  un autre serveur ou une autre personne de confiance :

        ./monitor_elections.py --url https://vote.example.org/ --wdir workdir --checkhash yes --hashref workdir/hashref --outputref workdir/hashref --sighashref https://vote.example.org/monitoring-reference/reference.json.gpg --keyring workdir/trustdb.gpg --uuid UUID

Dans tous les cas, l'auditeur va régulièrement exécuter une commande
d'audit que nous appellerons `monitor_elections`.
Il est possible de rediriger les messages avec l'option `--logfile`.
Alors, seuls les comportements anormaux seront rapportés sur
`stdout/stderr`, ce qui rend possible d'exécuter la commande depuis une
`crontab` et d'être alerté en cas de problème.

**Phase de vote.**
Pendant l'élection, il est attendu de l'auditeur :

- dans le cas où l'auditeur a accès à la liste électorale `voters.txt`
  (ce qui est le cas de la commission électorale), qu'il vérifie que le
  nombre de votants affiché sur la page principale de l'élection
  correspond à la liste électorale, ainsi que le poids total de
  l'élection, s'il s'agit d'une élection à poids, et que l'empreinte de
  la liste électorale correspond à celle qui a été sauvegardée
  auparavant, par exemple en utilisant une des commandes suggérées
  [ici](#hash) ;

- si l'auditeur n'a pas accès à la liste électorale, qu'il vérifie que le
  nombre d'électeurs et le poids total de l'élection affichée sur la page
  principale de l'élection correspondent aux données officielles.

- qu'il exécute fréquemment `monitor_elections`. Idéalement, cela doit
  être effectué à des moments non-prédictible dans le temps, à partir
  d'adresses IP variées reflétant la diversité des électeurs et des
  autorités. Le but est qu'un serveur corrompu ne puisse pas deviner si
  les requêtes viennent d'un auditeur ou d'un votant ou d'une autorité.
  Voici quelques recommandations pour qu'un auditeur puisse se faire
  passer pour un utilisateur :
  * comme déjà mentionné, les requêtes au serveur doivent être
    fréquentes, mais pas à intervalle régulier ou prédictible ;
  * non seulement les adresses IP doivent varier, mais également les
    informations de configuration du navigateur (type de navigateur et
    version, système, extensions actives, fuseau horaire, langue,
    résolution de l'écran, etc), à partir d'un grand nombre de
    configurations réellement utilisées par des humains ;
  * les adresses IP doivent refléter les lieux variés et les fournisseurs
    d'accès de la population des électeurs ;
  * l'ordre dans lequel les fichiers sont demandés au serveur doit
    refléter l'ordre d'une visite typique des électeurs et des autorités,
    avec un délai plausible (mais non-prédictible) entre chaque requête.

  Notons que le script fourni par `belenios-tool` n'offre pas de support
  pour tout ceci.

**Après l'élection.**
Après l'élection, il est attendu de l'auditeur :

- qu'il exécute de nouveau `monitor_elections`. La page de l'élection
  contient désormais un fichier `result.json` et cette commande va
  vérifier les preuves cryptographiques associées au résultat de
  l'élection ;
- qu'il vérifie que le résultat mentionné dans le fichier `result.json`
  correspond au résultat publié sur la page principale de l'élection.
  Cette vérification doit être effectuée manuellement.

Note : Si l'outil en ligne de commande `belenios-tool` est utilisé, la
confiance dans les tests effectués repose en partie dans la confiance
en l'outil. Il est possible d'implémenter son propre logiciel de
vérification à partir des spécifications de Belenios, disponibles [ici](https://vote.vcast.vote/v3/static/specification.pdf).


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
- s'il y a des autorités externes de déchiffrement (fortement
  recommandé pour le secret du vote), l'administrateur de l'élection
  doit s'assurer qu'elles ont correctement enregistré leur clé de
  déchiffrement, sinon l'élection ne pourra pas être dépouillée. Pour
  cela, il est possible de cliquer sur `Vérifier la possession de la
  clé privée` dans la page récapitulative de l'élection. Il faut
  ensuite envoyer le lien obtenu à chaque autorité et s'assurer
  qu'elles confirment que le test a fonctionné correctement (avec
  affichage de leur nom).

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
ici plusieurs solutions pour calculer cette empreinte en ligne de
commande. Nous utilisons le fichier `voters.txt` en exemple mais vous
pouvez bien sûr le remplacer par un autre fichier.

    sha256sum voters.txt | xxd -p -r | base64 | tr -d "="

(ou bien `shasum -a256` au lieu de `sha256sum` par exemple sur MacOS)

ou encore :

    cat voters.txt | python3 -c "import hashlib,base64,sys;m=hashlib.sha256();m.update(sys.stdin.read().encode());print(base64.b64encode(m.digest()).decode().strip('='))"

Vous pouvez également utiliser [l'outil en ligne](https://vote.vcast.vote/v3/tools/compute-fingerprint) mis à disposition par
Belenios.
