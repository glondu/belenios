#!/usr/bin/env python3

import json
from collections import OrderedDict
import smtplib
from email.mime.text import MIMEText
from string import Template
import time
import getpass

# In DEGUB mode, emails are sent to this address instead of the true one.
# (typically the address of the credential authority)
DEBUG=False
DEBUG_MAIL='bozo.leclown@example.com'

# Edit the following according to your election:
FROM='bozo.leclown@example.com' # can be the email of the credential authority
SUBJECT='Élection du meilleur cookie: votre matériel de vote'
UUID='pmFbhhuxH3k6vc'

# Your outgoing email configuration:
SMTP='smtp.example.com'
username='bozo'
password = getpass.getpass("please type your password: ")

# name of the file where to read the voter list
VOTERS_FILE='voters.txt'

# name of the file where to read the credentials
CODE_FILE='creds.txt'

# Edit the email template:
TEMPLATE=Template("""
Bonjour,

Nous vous invitons à participer à l'élection du meilleur cookie
à l'adresse suivante:

  https://vote.belenios.org/elections/$UUID/

Vous aurez besoin de vos identifiants LDAP ou de votre login/mot de
passe, mais aussi du code de vote personnel (appelé "credential") que voici :

  $ELECTION_CODE

Le scrutin est ouvert du 1 avril à 9h au 2 avril à 18h.

Veillez bien à aller au bout des 6 étapes pour que votre vote soit pris
en compte. Un mail de confirmation vous sera envoyé.

Pour rappel, il y a deux candidats : Maïté et Amandine.

Merci de votre participation

==========================================================

Hello,

You are listed as a voter for the election of the best cookie.
Please visit the following link:

  https://vote.belenios.org/elections/$UUID/

You will need your LDAP or login / password, and also the following
credential (personal code):

  $ELECTION_CODE

The election is open from April 1st, 9am to April 2nd, 6pm.

Be sure to go through the 6 steps to ensure that your vote is taken into
account. A confirmation email will be sent.

Reminder: there are two candidates Maïté and Amandine.

Thank you for your participation.
""")

# Populate mapping from logins to addresses
voters = {}
try:
    with open(VOTERS_FILE) as vf:
        data = json.load(vf)
        for x in data:
            address = x["address"]
            login = x.get("login", address)
            voters[login] = address
except json.JSONDecodeError:
    with open(VOTERS_FILE) as vf:
        for line in vf:
            l = line.strip().split(",")
            if len(l) < 2:
                address = l[0]
                login = l[0]
            else:
                address = l[0]
                login = l[1] or address
            voters[login] = address

# Real stuff starts here. Pretty short, isn't it?
with open(CODE_FILE) as cf:
    d = dict(UUID=UUID)
    s = smtplib.SMTP(SMTP)
    s.starttls()
    s.login(username, password)
    data = json.load(cf, object_pairs_hook=OrderedDict)
    for login, credential in data.items():
        d['ELECTION_CODE'] = credential
        msg = MIMEText(TEMPLATE.substitute(d))
        email = voters[login]
        msg['Subject'] = SUBJECT
        msg['From'] = FROM
        if DEBUG:
            msg['To'] = DEBUG_MAIL
        else:
            msg['To'] = email
        s.send_message(msg)
        time.sleep(0.2) # short delay; might need more for very large election
    s.quit()
