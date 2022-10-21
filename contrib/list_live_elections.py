#!/usr/bin/env python3

import argparse
import os
import sys
import re
import json
import datetime

MIN_VOTERS=5
MAX_TALLIED_AGE=7       # expressed in days
MAX_FINALIZED_AGE=30    # expressed in days

# verb is a global variable, controlled by --verbose
def verb_print(str):
    if (verb):
        print(str, file=sys.stderr)

def all_uuid(path):
    return [ f for f in os.listdir(path) if os.path.isdir(os.path.join(path, f)) ]

def is_draft_or_deleted(elec_path):
    if os.path.exists(os.path.join(elec_path, "deleted.json")):
        return True
    if os.path.exists(os.path.join(elec_path, "draft.json")):
        return True

def is_secure(elec_path):
    meta = os.path.join(elec_path, "metadata.json")
    assert os.path.exists(meta)
    with open(meta,"r") as file:
        data = json.load(file)
    if 'cred_authority' in data and data['cred_authority'] != 'server':
        return True
    if 'trustees' in data:
        if len(data['trustees']) > 1:
            return True
    return False

def is_test(elec_path):
    elec = os.path.join(elec_path, "election.json")
    if not os.path.exists(elec):
        print("Can not read election.json in " + elec_path)
        assert False
    with open(elec,"r") as file:
        data = json.load(file)
    if re.search("test", data['name'], re.IGNORECASE) != None:
        return True
    voters = os.path.join(elec_path, "voters.txt")
    num_voters = sum(1 for line in open(voters, "r"))
    if num_voters < MIN_VOTERS:
        return True
    return False

# wrapper around strptime, because 2 formats can exist (change between
# 1.17 and 1.18)
def parse_date(s):
    try:
        d = datetime.datetime.strptime(s, "%Y-%m-%d %H:%M:%S.%f")
    except ValueError:
        d = datetime.datetime.strptime(s, "%Y-%m-%d %H:%M:%S")
    return d

def is_old(elec_path):
    dates = os.path.join(elec_path, "dates.json")
    assert os.path.exists(dates)
    with open(dates,"r") as file:
        data = json.load(file)
    if 'archive' in data:
        return True
    now = datetime.datetime.now()
    if 'tally' in data:
        tallied = data['tally']
        tt = parse_date(tallied)
        age = now-tt
        if age > datetime.timedelta(days=MAX_TALLIED_AGE):
            return True
    else: # not tallied, but finalized for a long time ?
        finalized = data['finalization']
        tt = parse_date(finalized)
        age = now-tt
        if age > datetime.timedelta(days=MAX_FINALIZED_AGE):
            return True
    return False

parser = argparse.ArgumentParser(description="list elections that are alive and deserve to be monitored")
parser.add_argument("spool_directory",
        help="Spool directory where the elections are stored")
parser.add_argument("--verbose", help="explain why elections are discarded on stderr", action="store_true")
args = parser.parse_args()
verb = args.verbose

uuids = all_uuid(args.spool_directory)
for uuid in uuids:
    elec_path = os.path.join(args.spool_directory, uuid)
    if is_draft_or_deleted(elec_path):
        verb_print("Election {} is deleted or not yet finalized".format(uuid))
        continue
    if is_test(elec_path):
        verb_print("Election {} is probably a test election".format(uuid))
        continue
    if not is_secure(elec_path):
        verb_print("Election {} is in degraded mode".format(uuid))
        continue
    if is_old(elec_path):
        verb_print("Election {} is old".format(uuid))
        continue
    print(uuid)
