#!/usr/bin/env python3

import argparse
import os
import sys
import hashlib
import base64
import urllib.request


static_files = {
        # javascript
        'tool_js_booth.js'      : '78d10fa9795e403fb965d089468e51c01e752d037a559c171d8b477f90ee2884',
        'tool_js_credgen.js'    : '956fbbb1d5f4fe4a59dda5ab49d56b8c3134f1ccd55de1e3d64f3108537a00a3',
        'tool_js.js'            : '67c93754feaceb6fd9f120bf71aba40f1236eb12af155ac24fa486aecf76a46d',
        'tool_js_pd.js'         : '8e525108f01ef52bc581ff796ee9e84a6203836514da5f74a0a46835ab29b6a1',
        'tool_js_questions.js'  : 'e2d9af44fb6ff1bc0e77c3d39191e872c9387f1079003281d81e8675c793521d',
        'tool_js_shuffle.js'    : '37a9ae6954f7496140c4c7b58d783e735520dca436e3fd18073d2686043e3e7a',
        'tool_js_tkeygen.js'    : 'c706a9a4d4ebb55916d9e4776069e4f8edd4e547250ced3f7ebf06fb255a4770',
        'tool_js_ttkeygen.js'   : '0534d8d7a943ca359e4505ef7aab28a6904d98a5fd6a20f3a67fba96526451be',
        # css
        'booth.css'             : 'af6478624138e1ada82715672891cb771ed79512d39c7a62456b6ab28e724450',
        'reset.css'             : '25ee6a8343352160afb705985f6a67167e448cdc91d60b2c36937a5c45861a0c',
        'site.css'              : '78404f81a38e68972c0f64a1a23a27bcae2654127851af5cca8a109e7fb9d550',
        'style.css'             : '09a196b349575aacb1e73f887b88c5f70f1f49f3ab4f55c372897a340d28eaef',
        'styled-elements.css'   : '8ff742b93fd5214aed3e3cf7fa653d9b41f0604a647b15329b66206c50a7b0ac',
        'superfish.css'         : 'd857ef3e7dacbbeafffb2cd901f1baa62dc055ea02afa59c69892baee9f958c1',
        # images
        'encrypting.gif'        : '67045b2289294c222cbab0dbfd07e0af1a40ba39c4ff6165ea9578e2345385da',
        'logo.png'              : '677fd224e8fe974a97abd5193251ceac77312f4c18fea1d643b52ccd54210a00',
        'placeholder.png'       : 'e8c07993ea46e361080c9f8c50336a2dd8a02866819ea4d442d2ae4131d89772',
}

vote_file = {
        'fr'     : 'abc5dd5519f7647b27bc71088ea958cff49239ed773680eb69fd8149148b7557',
        'en'     : '2dfdd06853988e5f8b3b93d4f6f356a2a3a147fb04ab748508cce25774667222',
        'de'     : '9847a8151c99daf7904ba6f61ae48786f1b918337ec0e542815cdbf51123607b',
        'it'     : '50f9ba063c1b8ed8c4e42a5517bcc3bd6b6163b3c96c27b15fbecb60975ee0be',
        'ro'     : 'd7fbd386cbdd6d7834b47ba57563b43974789505b09e96a0908beb32c5172119'
}

def hash_votefile(url, lang):
    link = url + "/vote.html"
    try:
        head = { 'Accept-Language' : lang }
        req = urllib.request.Request(link, headers=head)
        resp = urllib.request.urlopen(req)
        data = resp.read()
    except:
        print("Failed to download vote.html in lang {}.  Aborting".format(lang))
        sys.exit(1)
    m = hashlib.sha256()
    m.update(data)
    h = m.hexdigest()
    return h

def hash_file(url, name):
    link = url + "/static/" + name
    try:
        resp = urllib.request.urlopen(link)
        data = resp.read()
    except:
        print("Failed to download {}. Aborting".format(link))
        sys.exit(1)
    m = hashlib.sha256()
    m.update(data)
    h = m.hexdigest()
    return h


parser = argparse.ArgumentParser(description="monitor files served by a Belenios server")
parser.add_argument("--url", required=True, help="prefix url of the Belenios server")

args = parser.parse_args()

url = args.url.strip("/")

fail = False

for f in static_files.keys():
    h = hash_file(url, f)
    if h != static_files[f]:
        fail = True
        print("Wrong hash of static file {}: got {} but expect {}".format(f, h, static_files[f]))

for lang in vote_file.keys():
    h = hash_votefile(url, lang)
    if h != vote_file[lang]:
        fail = True
        print("Wrong hash of vote.hml in {}: got {} but expect {}".format(lang, h, vote_file[lang]))

if fail:
    sys.exit(1)
