#!/usr/bin/env python3

import argparse
import os
import sys
import hashlib
import base64
import urllib.request
import json


def hash_votefile(link, lang):
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

def hash_file(link):
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
parser.add_argument("--reference", required=True, help="reference file")
parser.add_argument("--output", help="output new reference to this file")

args = parser.parse_args()

url = args.url.strip("/")

fail = False

with open(args.reference) as f:
    reference = json.load(f)

new_reference = {}

for f, descr in reference.items():
    if type(descr) == dict:
        new_reference[f] = {}
        for lang, descr in descr.items():
            h = hash_votefile(url + f, lang)
            new_reference[f][lang] = h
            if h != descr:
                fail = True
                print("Wrong hash of {} in {}: got {} but expected {}".format(f, lang, h, descr))
    else:
        h = hash_file(url + f)
        new_reference[f] = h
        if h != descr:
            fail = True
            print("Wrong hash of static file {}: got {} but expected {}".format(f, h, descr))

if args.output:
    with open(args.output, mode="w") as f:
        json.dump(new_reference, f)

if fail:
    sys.exit(1)
