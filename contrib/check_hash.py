#!/usr/bin/env python3

import argparse
import os
import sys
import hashlib
import base64
import urllib.request


static_files = {
        # javascript
        'BigIntCompat.js'       : '615e2613776c52363289c7abff0a831ca2a69d4709d5f6ee7e1455514edf3e50',
        'random.js'             : 'f641c8981f043ffb3a7b69306f2cb428ed26e99998a41e75b6ccdabd2cdb919d',
        'sjcl.js'               : '56c2f11c54347c8eea42c6534a94a638a0134307be1c7d0fb306074e4c60b150',
        'tool_js_booth.js'      : 'aff7b9d931bcb61c04563d787f83739469f42f20b5b77870ae3b811b1aeaf2d6',
        'tool_js_credgen.js'    : '11c39ade5c107d39ceca5c0a32f3e2e46bfc6846ec729f83b78680f518673762',
        'tool_js.js'            : '3118dd73e05b89a37693bbc0fab4ed74cf91c072cc741b2cc4a1f36e5f10cabc',
        'tool_js_pd.js'         : '9eb9d313033b93aa87b6a19ca29bcfa09ec70b7a442f7726430193106c3c6a98',
        'tool_js_questions.js'  : '08d3b55fab43b193c6b047f2cf9d18e47860593a385d64a9b803314afed1a68e',
        'tool_js_shuffle.js'    : 'de87b7994cb115f730e0c18dab4a6d39a4a20e047dcf057dadfdb3cf9018d03b',
        'tool_js_tkeygen.js'    : '710e34c2edb92e291c04c668ddb5b72d1a59fdcaaeabc0c9c172964b5cf8d16e',
        'tool_js_ttkeygen.js'   : '4470d7f0a3aeef7f37e11b9074d5b3e62526d1902e34b532dbf775cc904d7dc6',
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
        'fr'     : 'c14f8b6b08205b983da318a7df47e6159af39dd1e22f69e6c9a1babf6900251e',
        'en'     : 'fcf678a7e0c4a9ff3d9701d996ae98ed80839d8af8d3d3d5335e7cb4656ff86d',
        'de'     : '03448063e39cc72a6ab7a7f7c5be95b9eeb00b76c360cb1a1eb7720320b2d8eb',
        'it'     : 'd2d02ced5a14f2c804a7eb7e8773fdf9ab5f9d7771975c4a157105caea4a68ae',
        'ro'     : 'a0d852da0acc191868fc0fd3af8e1d5d4b6ad837050a73a866a904256c1efd4e'
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
