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
        'tool_js_booth.js'      : 'a8da8e83c93a15324e97ba0862c014fb48edceda16b0f28621c3523dc1878367',
        'tool_js_credgen.js'    : '880b0a9fb12f240a5aec7f04ef573809064716bd6d88501fc9ca3c4e4341a6e8',
        'tool_js.js'            : 'c06bead705ec99dc3d268e7e9294ed1e367a14cb3b3659de66a00147a62e2a53',
        'tool_js_pd.js'         : 'caa572461549b929eea837e31c9185232fd99c0003a25c45003eadafb01791a9',
        'tool_js_questions.js'  : '5f7a221ad47620b49a15bb3ce83aa6bbf9e831cd8615caa0c6d2e1bab806333e',
        'tool_js_shuffle.js'    : 'b3da510b058215ea173a9450fca6d4caa30428c0d2d83a9c89d4c3ebee03d85e',
        'tool_js_tkeygen.js'    : '758b5f3a2257a990b736130796f85104d1e1c62b378a99338baadc444725f46f',
        'tool_js_ttkeygen.js'   : '34bd92fa86a485fedd43db503e04e68a1eb24f42aef7bd51af463e8014eb762f',
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
