#!/usr/bin/env python3

import argparse
import os
import sys
import datetime
import subprocess
import urllib.request
import urllib.error
import xml.dom.minidom
import re
import hashlib
import base64
import json

# Example :
#   ./monitor_elections.py --uuid aTGmQNj1SXA5JG --url https://belenios.loria.fr/beta/ --wdir /tmp/wdir --checkhash yes


# External dependencies:
# - belenios-tool
# - git
# - check_hash.py  (from the belenios source dist, in contrib/)


# TODO:
# - add options --belenios-tool-path and --check-hash-path
# - find a way to test that failure are detected
# - do a git gc from time to time or at the end (how?)


# The status contains:
#  - a boolean telling whether there was a problem
#  - a message to be put in the commit log
# This is updated along the way with the merge() method.
# For the moment, we have msg of type Bytes. Maybe string would be better ?
class Status:
    def __init__(self, fail, commit_msg):
        self.fail = fail
        self.msg = commit_msg
    # failure if at least one failure
    # concatenate all messages
    def merge(self, status):
        self.fail = self.fail or status.fail
        self.msg = self.msg + status.msg

# Default output for the logfile is stdout, i.e. None
log_file = None
def logme(str):
    msg = "Log: {}".format(str)
    if log_file == None:
        print(msg)
    else:
        print(msg, file=log_file)
# messages that should also go to stderr:
def Elogme(str):
    logme(str)
    print("Log: {}".format(str), file=sys.stderr)


# If it does not exist, create a fresh directory for an election
# and initialize the git.
def check_or_create_dir(wdir, uuid):
    p = os.path.join(wdir, uuid)
    if not os.path.exists(p):
        logme("creating directory for election {}".format(uuid))
        os.mkdir(p)
        os.mkdir(os.path.join(p, "new"))
    if not os.path.exists(os.path.join(p, ".git")):
        logme("init git for election {}".format(uuid))
        subprocess.run(["git", "init", p], capture_output=True)
        open(os.path.join(p, "fresh"), "w").close()

# List of audit files.
# When no ballot have been cast yet, ballots.jsons does not exist.
audit_files=['election.json', 'public_creds.txt', 'trustees.json',
        'ballots', 'index.html']
optional_audit_files=['ballots.jsons','result.json']

def download_audit_data(url, uuid):
    link = url + '/elections/' + uuid
    data = dict()
    status = Status(False, b"")
    fail = False
    msg = ""
    for f in audit_files:
        try:
            if f == 'index.html':
                l = link + '/'
            else:
                l = link + '/' + f
            resp = urllib.request.urlopen(l)
            data[f]=resp.read()
        except urllib.error.URLError as e:
            fail = True
            msg = msg + "Download {} failed with ret code \"{}\" for election {}\n".format(f, e, uuid)
    for f in optional_audit_files:
        try:
            resp = urllib.request.urlopen(link + '/' + f)
            data[f]=resp.read()
        except:
            data[f]=b''

    status = Status(fail, msg.encode())
    return status, data

# This write data to the directory in order to run verify and
# verify-diff.
# At first, this goes to a 'new' subdirectory, and once verify-diff has
# been run, this is moved to the main directory of the election.
def write_and_verify_new_data(wdir, uuid, data):
    # copy new data in the "new" subdirectory
    p = os.path.join(wdir, uuid)
    pnew = os.path.join(p, 'new')
    for f in audit_files + optional_audit_files:
        if data[f] != b'':
            with open(os.path.join(pnew, f), "wb") as newf:
                newf.write(data[f])

    # run belenios-tool verify on it
    ver = subprocess.run(["belenios-tool", "verify", "--dir={}".format(pnew)],
            stdout=subprocess.PIPE, stderr=subprocess.STDOUT)
    if ver.returncode != 0:
        msg="Error: belenios-tool verify failed on newly downloaded data from election {}, with output {}\n".format(uuid, ver.stdout).encode()
        return Status(True, msg)
    else:
        logme("Successfully verified new data of {}".format(uuid))

    # if not the first time, run belenios-tool verify-diff
    msg = b""
    if os.path.exists(os.path.join(p, "fresh")):
        os.remove(os.path.join(p, "fresh"))
    else:
        verdiff = subprocess.run(["belenios-tool", "verify-diff",
            "--dir1={}".format(p), "--dir2={}".format(pnew)],
            stdout=subprocess.PIPE, stderr=subprocess.STDOUT)
        if verdiff.returncode != 0:
            msg="Error: belenios-tool verify-diff failed on newly downloaded data from election {}, with output {}".format(uuid, verdiff.stdout).encode()
            return Status(True, msg)
        if re.search(b"W:", verdiff.stdout) != None:
            msg = verdiff.stdout
        logme("Successfully diff-verified new data of {}".format(uuid))

    # move new files to main subdirectory
    for f in audit_files + optional_audit_files:
        if data[f] != b'':
            os.rename(os.path.join(pnew, f), os.path.join(p, f))
    return Status(False, msg)

# Verify that the hash of the ballots shown on the ballot-box web page
# are consistent with the json file.
def check_hash_ballots(data):
    dom = xml.dom.minidom.parseString(data['ballots'])
    list_ballots = dom.getElementsByTagName("li")
    list_hash = [ x.firstChild.firstChild.data for x in list_ballots ]

    list_hash2 = []
    for l in data['ballots.jsons'].splitlines():
        m = hashlib.sha256()
        m.update(l)
        h = base64.b64encode(m.digest()).decode().strip('=')
        list_hash2.append(h)
    list_hash.sort()
    list_hash2.sort()
    if (not list_hash == list_hash2):
        msg = b"Error: hash of ballots do not correspond!\n"
        return Status(True, msg)
    else:
        logme("Successfully checked hash of ballots of {}".format(uuid))
        return Status(False, b"")

# Verify that the data printed on the page of the election is
# consistent with the other audit files.
def check_index_html(data):
    # when the election is closed, there is a "disabled" attributed
    # without value that the xml parser does not like. We remove it.
    st = data['index.html'].decode().replace('disabled>Start', '>Start')
    dom = xml.dom.minidom.parseString(st)
    fail = False
    msg = b""

    logme("Checking index.html of {} ...".format(uuid))

    # fingerprint of the election vs election.json
    m = hashlib.sha256()
    m.update(data['election.json'][0:-1]) # remove trailing \n
    h = base64.b64encode(m.digest()).decode().strip('=')
    h2 = dom.getElementsByTagName("code")[0].firstChild.data
    if (not h == h2):
        msg = "Error: Wrong fingerprint of election {}\n".format(uuid).encode()
        fail = True
    else:
        logme("  election fingerprint ok")

    # credential fingerprint vs public_creds.txt
    m = hashlib.sha256()
    m.update(data['public_creds.txt']) 
    h = base64.b64encode(m.digest()).decode().strip('=')
    node = [ x.firstChild for x in dom.getElementsByTagName("div")
            if x.firstChild != None and
            x.firstChild.nodeType == xml.dom.minidom.Node.TEXT_NODE and
            re.search("Credentials were generated",x.firstChild.data) != None ]
    assert len(node) == 1
    h2 = node[0].data.split(' ')[-1].strip('.')
    if (not h == h2):
        msg = msg + "Error: Wrong credential fingerprint of election {}\n".format(uuid).encode()
        fail = True
    else:
        logme("  cred fingerprint ok")

    def hash_pub_key(s):
        m = hashlib.sha256()
        m.update(b'\"')
        m.update(s.encode())
        m.update(b'\"')
        return base64.b64encode(m.digest()).decode().strip('=')

    # trustees fingerprint vs trustees.json
    jsn = json.loads(data['trustees.json'])
    names = []
    hashs = []
    for trustee in jsn:
        if trustee[0] == 'Single':
            if 'name' in trustee[1]:
                names.append(trustee[1]['name'])
            else:
                names.append("N/A")
            hashs.append(hash_pub_key(trustee[1]['public_key']))
        else:
            assert trustee[0] == 'Pedersen'
            for tru in trustee[1]['verification_keys']:
                if 'name' in tru:
                    names.append(tru['name'])
                else:
                    names.append("N/A")
                hashs.append(hash_pub_key(tru['public_key']))
    pat = re.compile('(^.*) \(([\w+/]*)\)$')
    names2 = []
    hashs2 = []
    # in index.html, the trustees are in the ul list with id "trustees"
    for trustee in [ x for x in dom.getElementsByTagName("ul") if
            x.getAttribute('id') == 'trustees'
            ][0].getElementsByTagName("li"):
        s = trustee.firstChild.data
        mat = pat.match(s)
        names2.append(mat.group(1))
        hashs2.append(mat.group(2))
    if ((not names == names2) or (not hashs == hashs2)):
        msg = msg + "Error: Wrong trustees fingerprint of election {}\n".format(uuid).encode()
        fail = True
    else:
        logme("  trustees fingerprints ok")

    # In Pedersen mode, check PKI fingerprints
    if jsn[0][0] == 'Pedersen':
        hashs = []
        for trustee in jsn[0][1]['certs']:
            m = hashlib.sha256()
            m.update(trustee['message'].encode())
            hashs.append(base64.b64encode(m.digest()).decode().strip('='))
        names2 = []
        hashs2 = []
        # in index.html, the PKI are in the ul list with id "pki"
        for trustee in [ x for x in dom.getElementsByTagName("ul") if
                x.getAttribute('id') == 'pki'
                ][0].getElementsByTagName("li"):
            s = trustee.firstChild.data
            mat = pat.match(s)
            names2.append(mat.group(1))
            hashs2.append(mat.group(2))
        if ((not names == names2) or (not hashs == hashs2)):
            msg = msg + "Error: Wrong PKI fingerprint of election {}\n".format(uuid).encode()
            fail = True
        else:
            logme("  PKI fingerprints ok")

    # shuffles fingerprint and encrypted tally vs result.json (if present)
    if data['result.json'] != b'':
        jsn = json.loads(data['result.json'])
        if 'encrypted_tally' in jsn:
            s = jsn['encrypted_tally']
            # convert to a string as Belenios use it for hashing
            ss = json.JSONEncoder().encode(s).replace(' ', '')
            m = hashlib.sha256()
            m.update(ss.encode())
            h = base64.b64encode(m.digest()).decode().strip('=')
            node = [ x.firstChild for x in dom.getElementsByTagName("div") if
                    x.firstChild != None and x.firstChild.nodeType ==
                    xml.dom.minidom.Node.TEXT_NODE and
                    re.search("The fingerprint of the encrypted tally",
                        x.firstChild.data) != None ]
            assert len(node) == 1
            h2 = node[0].data.split(' ')[-1].strip('.')
            if (not h == h2):
                msg = msg + "Error: Wrong encrypted tally fingerprint of election {}\n".format(uuid).encode()
                fail = True
            else:
                logme("  encrypted tally fingerprint ok")
            
        if 'shuffles' in jsn:
            hashs = []
            hashs2 = []
            for s in jsn['shuffles']:
                # convert to a string as Belenios use it for hashing
                ss = json.JSONEncoder().encode(s).replace(' ', '')
                m = hashlib.sha256()
                m.update(ss.encode())
                hashs.append(base64.b64encode(m.digest()).decode().strip('='))
            # in index.html, the shuffles are in the ul with id 'shuffles'
            for shuf in [ x for x in dom.getElementsByTagName("ul") if
                    x.getAttribute('id') == 'shuffles'
                    ][0].getElementsByTagName("li"):
                s = shuf.firstChild.data
                # reuse same pattern as for trustees
                mat = pat.match(s)
                hashs2.append(mat.group(2))
            if (not hashs == hashs2):
                msg = msg + "Error: Wrong shuffles fingerprint of election {}\n".format(uuid).encode()
                fail = True
            else:
                logme("  shuffles fingerprints ok")

    if not fail:
        logme("Successfully checked index.html of {}".format(uuid))
    return Status(fail, msg)


def commit(wdir, uuid, msg):
    eldir = os.path.join(wdir, uuid)
    for f in audit_files + optional_audit_files:
        if f in data.keys() and data[f] != b'':
            gitadd = subprocess.run(["git",
                "-C", eldir, "add", f])
            if gitadd.returncode != 0:
                Elogme("Failed git add {} for election {}".format(f, uuid))
                return False

    gitci = subprocess.run(["git",
        "-C", eldir,
        "commit", "-q", "--allow-empty", "--allow-empty-message",
        "-m",  msg.decode()])
    if gitci.returncode != 0:
        Elogme("Failed git commit {} for election {}".format(f, uuid))
        return False
    logme("Successfully added a commit for {}".format(uuid))
    return True

#############################################

# Parsing a bool is not a built-in of argparse :-(
# https://stackoverflow.com/questions/15008758/parsing-boolean-values-with-argparse
def str2bool(v):
    if isinstance(v, bool):
       return v
    if v.lower() in ('yes', 'true', 't', 'y', '1'):
        return True
    elif v.lower() in ('no', 'false', 'f', 'n', '0'):
        return False
    else:
        raise argparse.ArgumentTypeError('Boolean value expected.')

parser = argparse.ArgumentParser(description="monitor Belenios elections")
group = parser.add_mutually_exclusive_group(required=True)
group.add_argument("--uuidfile", help="file containing uuid's of election to monitor")
group.add_argument("--uuid", help="uuid of an election to monitor")
parser.add_argument("--url", required=True, help="prefix url (without trailing /elections )")
parser.add_argument("--wdir", required=True, help="work dir where logs are kept")
parser.add_argument("--checkhash", type=str2bool, nargs='?',
                        const=True, default=True, metavar="yes|no",
                        help="also check static files on the server")
parser.add_argument("--logfile", help="file to write the non-error logs")

args = parser.parse_args()

# Set logfile; check permissions
if args.logfile:
    log_file = open(args.logfile, "a")

# Build list of uuids
if args.uuid:
    uuids = [ args.uuid ]
else:
    assert (args.uuidfile)
    uuids = [ ]
    with open(args.uuidfile, "r") as file:
        for line in file:
            uuids.append(line.rstrip())

# check that wdir exists and is r/w
if not os.path.isdir(args.wdir) or not os.access(args.wdir, os.W_OK | os.R_OK):
    print("The wdir {} should read/write accessible".format(args.wdir))
    sys.exit(1)

logme("[{}] Starting monitoring elections.".format(datetime.datetime.now()))

for uuid in uuids:
    logme("Start monitoring election {}".format(uuid))

    check_or_create_dir(args.wdir, uuid)

    status, data = download_audit_data(args.url.strip("/"), uuid)

    # if we managed to download stuff, then check what we can
    if not status.fail:
        stat = write_and_verify_new_data(args.wdir, uuid, data)
        status.merge(stat)

        stat = check_hash_ballots(data)
        status.merge(stat)

        stat = check_index_html(data)
        status.merge(stat)

    # commit 
    if status.msg != b'':
        Elogme("Commit log for election {} is {}".format(uuid,
            status.msg.decode()))
    commit(args.wdir, uuid, status.msg)
    
# check hash of js files. They do not depend on a particular election, so
# we dot it only once

if args.checkhash == True:
    hh = subprocess.run(["check_hash.py", "--url", args.url ],
            stdout=subprocess.PIPE, stderr=subprocess.STDOUT)
    if hh.returncode != 0:
        print(hh.stdout.decode())
        sys.exit(1)
    else:
        logme("Successfully checked hash of static files")

if args.logfile:
    log_file.close()
