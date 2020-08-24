#!/usr/bin/env python3

import argparse
import os
import sys
import hashlib
import base64
import urllib.request


static_files = {
        # javascript
        'tool_js_booth.js'      : 'bb2533e1a910d8290f91a86c82f1b0ebc30d0c6179d3c0a2b703762d3f91e28e',
        'tool_js_credgen.js'    : '956fbbb1d5f4fe4a59dda5ab49d56b8c3134f1ccd55de1e3d64f3108537a00a3',
        'tool_js.js'            : '4d537493b4b74a85e3d56963a45eebc2e23a5f6fe21e766841b1dc11c5efc323',
        'tool_js_pd.js'         : '41fb1739a252b744b990267ae03d8087726cdd210a97ca4a55526830677f3536',
        'tool_js_questions.js'  : 'e2d9af44fb6ff1bc0e77c3d39191e872c9387f1079003281d81e8675c793521d',
        'tool_js_shuffle.js'    : 'be53b1e4891d65f3022822e7e6338ebc3ade2d13562bb9aac89f12a2f20ef773',
        'tool_js_tkeygen.js'    : '9c73f0b944475bdff911b4bbd0d3ac2899dac55717408269d10d8c9e1d476004',
        'tool_js_ttkeygen.js'   : '45bda5c8a3d537ca7fd9643d72da16850bf4d4d2c4f78783894d68aa0ffb391e',
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
