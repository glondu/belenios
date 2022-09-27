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
import random

# Example :
#   ./monitor_elections.py --uuid aTGmQNj1SXA5JG --url https://belenios.loria.fr/ --wdir /tmp/wdir --checkhash yes --hashref $HOME/hashref --outputref  $HOME/hashref --sighashref https://belenios.loria.fr/monitoring-reference/reference.json.gpg --keyring $HOME/.gnupg/pubring.gpg


# External dependencies (must be in path):
# - belenios-tool
# - git
# - (optional) gpg


# TODO:
# - add options --belenios-tool-path
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

def b64_of_hex(s):
    return base64.b64encode(bytes.fromhex(s)).decode().strip("=")

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
# We also put here a fake filename, for the hash of the voterlist
# and for the list of all ballot hashs.
audit_files=['election.json', 'public_creds.txt', 'trustees.json',
        'ballots', 'index.html']
optional_audit_files=['ballots.jsons','result.json','shuffles.jsons',
        'hash_voterlist','all_ballot_hashs']

def shuffle(l):
    result = [x for x in l]
    random.shuffle(result)
    return result

def download_audit_data(url, uuid):
    link = url + '/elections/' + uuid
    data = dict()
    status = Status(False, b"")
    fail = False
    msg = ""
    for f in shuffle(audit_files):
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
    for f in shuffle(optional_audit_files):
        try:
            resp = urllib.request.urlopen(link + '/' + f)
            data[f]=resp.read()
        except:
            data[f]=b''

    status = Status(fail, msg.encode())
    return status, data

def get_new_ballots(old_ballotsfile, new_ballotsfile):
    old = set([b64_of_hex(x["hash"]) for x in json.loads(old_ballotsfile)])
    result = b""
    for x in json.loads(new_ballotsfile):
        h = b64_of_hex(x["hash"])
        if not h in old:
            result = result + h.encode() + b"\n"
    return result

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
    ver = subprocess.run(["belenios-tool", "election", "verify", "--dir={}".format(pnew)],
            stdout=subprocess.PIPE, stderr=subprocess.STDOUT)
    if ver.returncode != 0:
        msg="Error: belenios-tool election verify failed on newly downloaded data from election {}, with output {}\n".format(uuid, ver.stdout).encode()
        return Status(True, msg)
    else:
        logme("Successfully verified new data of {}".format(uuid))

    # if not the first time, run belenios-tool election verify-diff
    msg = b""
    new_ballots = b""
    fresh = os.path.exists(os.path.join(p, "fresh"))
    if fresh:
        os.remove(os.path.join(p, "fresh"))
    else:
        verdiff = subprocess.run(["belenios-tool", "election", "verify-diff",
            "--dir1={}".format(p), "--dir2={}".format(pnew)],
            stdout=subprocess.PIPE, stderr=subprocess.STDOUT)
        if verdiff.returncode != 0:
            msg="Error: belenios-tool election verify-diff failed on newly downloaded data from election {}, with output {}".format(uuid, verdiff.stdout).encode()
            return Status(True, msg)
        if re.search(b"W:", verdiff.stdout) != None:
            msg = verdiff.stdout
        logme("Successfully diff-verified new data of {}".format(uuid))

    # ballots of old data
    if fresh:
        ballot_summary1 = "[]"
    else:
        ballot_summary1 = subprocess.run(["belenios-tool", "election", "compute-ballot-summary", "--dir={}".format(p)],
                                         stdout=subprocess.PIPE, stderr=subprocess.DEVNULL)
        if ballot_summary1.returncode != 0:
            msg = "Error: compute-ballot-summary on old data failed for election {}".format(uuid).encode()
            return Status(True, msg)
        ballot_summary1 = ballot_summary1.stdout

    # ballots of new data
    ballot_summary2 = subprocess.run(["belenios-tool", "election", "compute-ballot-summary", "--dir={}".format(pnew)],
                                     stdout=subprocess.PIPE, stderr=subprocess.DEVNULL)
    if ballot_summary2.returncode != 0:
        msg = "Error: compute-ballot-summary on new data failed for election {}".format(uuid).encode()
        return Status(True, msg)
    ballot_summary2 = ballot_summary2.stdout

    # compute new ballots
    new_ballots = get_new_ballots(ballot_summary1, ballot_summary2)
    data['new_ballots'] = new_ballots
    data['ballot_summary'] = ballot_summary2

    # compute checksums
    checksums = subprocess.run(["belenios-tool", "election", "compute-checksums", "--dir={}".format(pnew)],
                               stdout=subprocess.PIPE, stderr=subprocess.DEVNULL)
    if checksums.returncode != 0:
        msg = "Error: belenios-tool election compute-checksums failed on newly downloaded data form election {}, with output {}".format(uuid, checksums.stdout).encode()
        return Status(True, msg)
    data["checksums"] = checksums.stdout

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
    if len(list_ballots) == 0:
        return Status(False, b"")
    if len(list_ballots[0].childNodes) == 2:
        has_weight = True
    else:
        has_weight = False

    list_hash = [ x.firstChild.firstChild.data for x in list_ballots ]
    if has_weight:
        list_weights = [ x.childNodes[1].data for x in list_ballots ]
        list_weights = [ x[2:-1] for x in list_weights ]

    list_hash2 = []
    list_weights2 = []
    for x in json.loads(data['ballot_summary']):
        h = b64_of_hex(x["hash"])
        list_hash2.append(h)
        if has_weight:
            list_weights2.append(str(x["weight"]))
    if has_weight:
        list_hash = [ (list_hash[i],list_weights[i]) for i in range(len(list_hash)) ]
        list_hash2 = [ (list_hash2[i],list_weights2[i]) for i in range(len(list_hash2)) ]
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
    # also, the &lang in the link makes the parser crazy.
    st = st.replace('&lang', 'lang')
    dom = xml.dom.minidom.parseString(st)
    fail = False
    msg = b""

    logme("Checking index.html of {} ...".format(uuid))

    # load checksums computed by belenios-tool
    checksums = json.loads(data["checksums"])

    # fingerprint of the election vs election.json
    h = b64_of_hex(checksums["election"])
    h2 = dom.getElementsByTagName("code")[0].firstChild.data
    if (not h == h2):
        msg = "Error: Wrong fingerprint of election {}\n".format(uuid).encode()
        fail = True
    else:
        logme("  election fingerprint ok")

    # credential fingerprint
    h = b64_of_hex(checksums["public_credentials"])
    node = [ x.firstChild for x in dom.getElementsByTagName("div") if x.getAttribute("id") == "credentials" ]
    assert len(node) == 1
    h2 = node[0].data.split(' ')[-1].strip('.')
    if (not h == h2):
        msg = msg + "Error: Wrong credential fingerprint of election {}\n".format(uuid).encode()
        fail = True
    else:
        logme("  cred fingerprint ok")

    # if weights, check the total/min/max
    if "weights" in checksums:
        weights = checksums["weights"]
        node = [ x.firstChild for x in dom.getElementsByTagName("div") if x.getAttribute("id") == "weights" ]
        if len(node) == 1:
            pat = re.compile(r'The total weight is (\d+) \(min: (\d+), max: (\d+)\)')
            mat = pat.match(node[0].data)
            w_tot = int(mat.group(1))
            w_min = int(mat.group(2))
            w_max = int(mat.group(3))
            c_tot = int(weights["total"])
            c_min = int(weights["min"])
            c_max = int(weights["max"])
            if (c_tot != w_tot) or (c_min != w_min) or (c_max != w_max):
                msg = msg + "Error: Wrong stats of weights in election {}\n".format(uuid).encode()
                logme(" " + str(c_tot) + " " + str(c_min) + " " + str(c_max))
                fail = True
            else:
                logme("  stats of weights ok")
        else:
            msg += "Error: missing weights in election {}\n".format(uuid).encode()
            fail = True

    # check that the printed number of voters is consistent with number
    # of credentials
    node = [ x.firstChild for x in dom.getElementsByTagName("div") if x.getAttribute("id") == "voters" ]
    assert len(node) == 1
    pat = re.compile(r'The voter list has (\d+) voter\(s\) and fingerprint ([\w/+]+).')
    mat = pat.match(node[0].data)
    nb_voters = int(mat.group(1))
    data['hash_voterlist'] = mat.group(2).encode() # for further checks
    nb_creds = checksums["num_voters"]
    if (nb_voters != nb_creds):
        msg = msg + "Error: Number of voters different from number of credentials for election {}\n".format(uuid).encode()
        logme(" " + str(nb_voters) + " " + str(nb_creds))
        fail = True
    else:
        logme("  number of voters ok")

    # trustees fingerprint vs trustees.json
    names = []
    pks = []
    certs = []

    for trustee in checksums["trustees"]:
        if "name" in trustee:
            names.append(trustee["name"])
        else:
            names.append("N/A")
        pks.append(b64_of_hex(trustee["checksum"]))
        certs.append(None)

    for tset in checksums["trustees_threshold"]:
        for trustee in tset["trustees"]:
            if "name" in trustee:
                names.append(trustee["name"])
            else:
                names.append("N/A")
            pks.append(b64_of_hex(trustee["verification_key"]))
            certs.append(b64_of_hex(trustee["pki_key"]))

    names2 = []
    pks2 = []
    certs2 = []
    # in index.html, the non-threshold trustees are in the ul list with id "trustees"
    arr = [ x for x in dom.getElementsByTagName("ul") if x.getAttribute('id') == 'trustees' ]
    if arr != []:
        pat = re.compile(r'(^.*) \(([\w+/]*)\)$')
        for trustee in arr[0].getElementsByTagName("li"):
            s = trustee.firstChild.data
            mat = pat.match(s)
            names2.append(mat.group(1))
            pks2.append(mat.group(2))
            certs2.append(None)
    # the threshold trustees are in the ul list with class "trustees_threshold"
    arr = [ x for x in dom.getElementsByTagName("ul") if x.getAttribute('class') == 'trustees_threshold' ]
    if arr != []:
        pat = re.compile(r'(^.*) \(([\w+/]*)\) \[([\w+/]*)\]$')
        for trustee in arr[0].getElementsByTagName("li"):
            s = trustee.firstChild.data
            mat = pat.match(s)
            names2.append(mat.group(1))
            pks2.append(mat.group(2))
            certs2.append(mat.group(3))

    if ((not names == names2) or (not pks == pks2) or (not certs == certs2)):
        msg = msg + "Error: Wrong trustees fingerprint of election {}\n".format(uuid).encode()
        fail = True
    else:
        logme("  trustees fingerprints ok")

    # check consistency of encrypted tally
    if "encrypted_tally" in checksums:
        h = b64_of_hex(checksums['encrypted_tally'])
        node = [ x.firstChild for x in dom.getElementsByTagName("div") if x.getAttribute("id") == "encrypted_tally" ]
        assert len(node) <= 1
        if len(node) == 1:
            h2 = node[0].data.split(' ')[-1].strip('.')
            if (not h == h2):
                msg = msg + "Error: Wrong encrypted tally fingerprint of election {}\n".format(uuid).encode()
                fail = True
            else:
                logme("  encrypted tally fingerprint ok")

    # check consistency of shuffles
    if "shuffles" in checksums:
        hashs = [b64_of_hex(x["checksum"]) for x in checksums["shuffles"]]
        hashs2 = []
        # in index.html, the shuffles are in the ul with id 'shuffles'
        for shuf in [ x for x in dom.getElementsByTagName("ul") if
                x.getAttribute('id') == 'shuffles'
                ][0].getElementsByTagName("li"):
            s = shuf.firstChild.data
            pat = re.compile(r'(^.*) \(([\w+/]*)\)$')
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

## When a new ballot arrives, check that it was not seen earlier.
## This could be some kind of replay attack (possible only if the voter
## revotes).
def check_noreplay(uuid, path_to_all_ballot_hashs, new_hashs):
    fail = False
    msg = b""
    with open(path_to_all_ballot_hashs, "r") as file:
        ll = file.read()
        list_hashs = ll.splitlines()
    for h in new_hashs.decode().splitlines():
        if h in list_hashs:
            fail = True
            msg = msg + "Error: The new ballot {} is a replay in election {}!\n".format(h, uuid).encode()
    with open(path_to_all_ballot_hashs, "a") as file:
        for h in new_hashs.decode().splitlines():
            file.write(h + "\n")
    if not fail:
        logme("Successfully checked for a ballot replay of {}".format(uuid))
    return Status(fail, msg)


##################################
## Helper functions for monitoring static files

def hash_votefile(link, lang):
    try:
        head = { 'Accept-Language' : lang }
        req = urllib.request.Request(link, headers=head)
        resp = urllib.request.urlopen(req)
        data = resp.read()
    except:
        print("Failed to download {} in lang {}.  Aborting".format(link, lang))
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

def read_linguas(linguas):
    with open(linguas, "r") as fp:
        return set(fp.readlines())

def get_all_available_languages(belenios_srcpath):
    admin = read_linguas(belenios_srcpath + "/po/admin/LINGUAS")
    voter = read_linguas(belenios_srcpath + "/po/voter/LINGUAS")
    result = [str(x).strip() for x in admin.union(voter)]
    result.sort()
    return result

# Will be populated if needed
langs = []

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
parser.add_argument("--url", required=True, help="prefix url (without trailing /elections )")
# arguments if one wants to monitor specific elections:
group = parser.add_mutually_exclusive_group()
group.add_argument("--uuidfile", help="file containing uuid's of election to monitor")
group.add_argument("--uuid", help="uuid of an election to monitor")
parser.add_argument("--wdir", help="work dir where logs are kept")
# arguments if one wants to monitor the files served by the server:
parser.add_argument("--checkhash", type=str2bool, nargs='?',
                        const=True, default=True, metavar="yes|no",
                        help="also check static files on the server")
parser.add_argument("--hashref", help="reference file for hash of static files")
parser.add_argument("--outputref", help="new reference file in case it changed on the server")
parser.add_argument("--sighashref", help="url where to find a gpg signature for the reference file");
parser.add_argument("--keyring", help="keyring to check the signature");
parser.add_argument("--beleniospath", default=".", help="path to Belenios sources")
# other arguments
parser.add_argument("--logfile", help="file to write the non-error logs")

args = parser.parse_args()

########### Check arguments

# Set logfile; check permissions
if args.logfile:
    log_file = open(args.logfile, "a")

### args for monitoring specific elections

# Build list of uuids
uuids = [ ]
if args.uuid:
    uuids = [ args.uuid ]
elif args.uuidfile:
    with open(args.uuidfile, "r") as file:
        for line in file:
            uuids.append(line.rstrip())

# check that wdir exists and is r/w (if uuids given)
if uuids:
    if not args.wdir:
        print("--wdir is mandatory for monitoring elections")
        sys.exit(1)
    if not os.path.isdir(args.wdir) or not os.access(args.wdir, os.W_OK | os.R_OK):
        print("The wdir {} should read/write accessible".format(args.wdir))
        sys.exit(1)

### args for monitoring static files

# check that a ref file is given if checkhash is set
if args.checkhash == True:
    if not args.hashref:
        print("If --checkhash is set, a --hashref file should be given")
        sys.exit(1)

# check that a keyring is given if a signature url is set
if args.sighashref:
    if not args.keyring or not args.checkhash:
        print("If --sighashref is given, --checkhash must be set and a --keyring file should be given")
        sys.exit(1)


########### Monitor static files

if args.checkhash == True:
    logme("[{}] Starting monitoring static files.".format(datetime.datetime.now()))

    # Compare hashref and what is served by the server
    url = args.url.strip("/")
    hashfile_changed = False
    with open(args.hashref) as f:
        tmp_reference = json.load(f)
    reference = {}
    for f, descr in tmp_reference.items():
        if f == "/static/locales/admin/*.json":
            langs = [x[0:-3] for x in os.listdir(args.beleniospath + "/po/admin") if x[-3:] == ".po"]
            langs.sort()
            for x in langs:
                reference["/static/locales/admin/{}.json".format(x)] = None
        elif f == "/static/locales/voter/*.json":
            langs = [x[0:-3] for x in os.listdir(args.beleniospath + "/po/voter") if x[-3:] == ".po"]
            langs.sort()
            for x in langs:
                reference["/static/locales/voter/{}.json".format(x)] = None
        elif f == "/static/frontend/translations/*.json":
            langs = [x for x in os.listdir(args.beleniospath + "/frontend/translations") if x[-5:] == ".json"]
            langs.sort()
            for x in langs:
                reference["/static/frontend/translations/{}".format(x)] = None
        elif "*" in f:
            print("Wildcard not supported in {}".format(f))
            sys.exit(1)
        else:
            reference[f] = descr
    new_reference = {}
    for f, descr in shuffle(reference.items()):
        if type(descr) == dict:
            new_reference[f] = {}
            if len(descr) == 0:
                if len(langs) == 0:
                    langs = get_all_available_languages(args.beleniospath)
                for lang in langs:
                    descr[lang] = None
            for lang, descr in shuffle(descr.items()):
                h = hash_votefile(url + f, lang)
                new_reference[f][lang] = h
                if h != descr:
                    hashfile_changed = True
                    print("Different hash of {} in {}: got {} but expected {}".format(f, lang, h, descr))
        else:
            h = hash_file(url + f)
            new_reference[f] = h
            if h != descr:
                hashfile_changed = True
                print("Different hash of static file {}: got {} but expected {}".format(f, h, descr))

    if hashfile_changed:
        logme("Hash of static files have changed")
    else:
        logme("Successfully checked hash of static files")

    if hashfile_changed and args.outputref:
        print("Writing new reference file")
        with open(args.outputref, mode="w") as f:
            json.dump(new_reference, f, sort_keys=True)

    # If we can check signature, do it
    if args.sighashref:
        resp = urllib.request.urlopen(args.sighashref)
        sig = resp.read()
        gpgrun = subprocess.run(["gpg", "--keyring", args.keyring, "--decrypt"],
                input=sig,
                capture_output=True)
        if gpgrun.returncode != 0:
            print("GPG signature verification failed")
            print(gpgrun.stderr)
            sys.exit(1)
        else:
            signed_ref = json.loads(gpgrun.stdout)
            if signed_ref != new_reference:
                print("Signed reference does not correspond to downloaded files")
                sys.exit(1)
        logme("Successfully checked signature of hash of static files")

########### Monitor elections

if uuids:
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
        # create the hash_voterlist file, with the value read from index.html
        # or check that its value is consistent
        p = os.path.join(args.wdir, uuid, 'hash_voterlist')
        if os.path.exists(p):
            with open(p, "rb") as file:
                oldhash = file.read()
            if (oldhash != data['hash_voterlist']):
                status.merge(Status(True,
                    "Error: hash of the voter list changed for election {}".format(uuid).encode()))
        else:
            with open(p, "wb") as file:
                file.write(data['hash_voterlist'])

        # create the all_ballot_hashs file, or update it from the new
        # ballot files. Check that an old ballot was not replayed.
        # Note: the list of new ballot hashs is created earlier, during
        # write_and_verify_new_data(), because it must compare the old
        # and new ballot box.
        p = os.path.join(args.wdir, uuid, 'all_ballot_hashs')
        if os.path.exists(p):
            stat = check_noreplay(uuid, p, data['new_ballots'])
            status.merge(stat)
        else:
            with open(p, "wb") as file:
                file.write(data['new_ballots'])

    # commit
    if status.msg != b'':
        Elogme("Commit log for election {} is {}".format(uuid,
            status.msg.decode()))
    commit(args.wdir, uuid, status.msg)

if args.logfile:
    log_file.close()
