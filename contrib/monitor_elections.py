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
#   ./monitor_elections.py --uuid aTGmQNj1SXA5JG --url https://belenios.loria.fr/ --wdir /tmp/wdir --checkhash yes --hashref $HOME/hashref --outputref  $HOME/hashref --sighashref https://belenios.loria.fr/monitoring-reference/reference.json.gpg --keyring $HOME/.gnupg/pubring.gpg


# External dependencies (must be in path):
# - belenios-tool
# - git
# - (optional) gpg


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
# We also put here a fake filename, for the hash of the voterlist.
audit_files=['election.json', 'public_creds.txt', 'trustees.json',
        'ballots', 'index.html']
optional_audit_files=['ballots.jsons','result.json','shuffles.jsons','hash_voterlist']

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
    for l in data['ballots.jsons'].splitlines():
        m = hashlib.sha256()
        m.update(l)
        h = base64.b64encode(m.digest()).decode().strip('=')
        list_hash2.append(h)
        if has_weight:
            jsn = json.loads(l)
            cred = jsn['credential']
            pat = re.compile(cred + r',(\d+)')
            m=pat.search(data['public_creds.txt'].decode())
            if m:
                list_weights2.append(m.group(1))
            else: # if absent, default weight is 1
                list_weights2.append('1')
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
    
    # if weights, check the total/min/max vs content of public_creds.txt
    node = [ x.firstChild for x in dom.getElementsByTagName("div")
            if x.firstChild != None and
            x.firstChild.nodeType == xml.dom.minidom.Node.TEXT_NODE and
            re.search("The total weight is",x.firstChild.data) != None ]
    if (len(node) == 1):
        fail_weights = False
        pat = re.compile(r'The total weight is (\d+) \(min: (\d+), max: (\d+)\)')
        mat = pat.match(node[0].data)
        w_tot = int(mat.group(1))
        w_min = int(mat.group(2))
        w_max = int(mat.group(3))
        c_tot = 0
        c_min = 10000000000
        c_max = 0
        for l in data['public_creds.txt'].splitlines():
            wt = l.decode().split(',')
            if len(wt) == 2:
                x = int(l.decode().split(',')[1])
            else: # if absent, default weight is 1.
                assert len(wt) == 1;
                x = 1
            c_tot += x
            if (x < c_min):
                c_min = x
            if (x > c_max):
                c_max = x
        if (c_tot != w_tot) or (c_min != w_min) or (c_max != w_max):
            msg = msg + "Error: Wrong stats of weights in election {}\n".format(uuid).encode()
            logme(" " + str(c_tot) + " " + str(c_min) + " " + str(c_max))
            fail = True
        else:
            logme("  stats of weights ok")

    # check that the printed number of voters is consistent with number
    # of credentials
    node = [ x.firstChild for x in dom.getElementsByTagName("div")
            if x.firstChild != None and
            x.firstChild.nodeType == xml.dom.minidom.Node.TEXT_NODE and
            re.search("The voter list has",x.firstChild.data) != None ]
    assert len(node) == 1
    pat = re.compile(r'The voter list has (\d+) voter\(s\) and fingerprint ([\w/+]+).')
    mat = pat.match(node[0].data)
    nb_voters = int(mat.group(1))
    data['hash_voterlist'] = mat.group(2).encode() # for further checks
    nb_creds = data['public_creds.txt'].decode().count('\n')
    if (nb_voters != nb_creds):
        msg = msg + "Error: Number of voters different from number of credentials for election {}\n".format(uuid).encode()
        logme(" " + str(nb_voters) + " " + str(nb_creds))
        fail = True
    else:
        logme("  number of voters ok")

    def hash_pub_key(s):
        m = hashlib.sha256()
        m.update(b'\"')
        m.update(s.encode())
        m.update(b'\"')
        return base64.b64encode(m.digest()).decode().strip('=')

    # trustees fingerprint vs trustees.json
    jsn = json.loads(data['trustees.json'])
    names = []
    pks = []
    certs = []

    for trustee in jsn:
        if trustee[0] == 'Single':
            if 'name' in trustee[1]:
                names.append(trustee[1]['name'])
            else:
                names.append("N/A")
            pks.append(hash_pub_key(trustee[1]['public_key']))
            certs.append(None)
        else:
            assert trustee[0] == 'Pedersen'
            for tru in trustee[1]['verification_keys']:
                if 'name' in tru:
                    names.append(tru['name'])
                else:
                    names.append("N/A")
                pks.append(hash_pub_key(tru['public_key']))
            for tru in trustee[1]['certs']:
                m = hashlib.sha256()
                m.update(tru['message'].encode())
                certs.append(base64.b64encode(m.digest()).decode().strip('='))

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

    # encrypted tally vs result.json (if present)
    # (and extract shuffles if present)
    shuf_array = None
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
            shuf_array=jsn['shuffles']
    elif data['shuffles.jsons'] != b'':
        # not exactly the same as in result.json: one json per line...
        shuf_array = []
        for line in data['shuffles.jsons'].decode().splitlines():
            shuf_array.append(json.loads(line))
    
    # shuffles fingerprint vs result.json or shuffles.jsons (if present)
    if shuf_array != None:
        hashs = []
        hashs2 = []
        for s in shuf_array:
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


##################################
## Helper functions for monitoring static files

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
            langs = [x[0:-3] for x in os.listdir("po/admin") if x[-3:] == ".po"]
            langs.sort()
            for x in langs:
                reference["/static/locales/admin/{}.json".format(x)] = None
        elif f == "/static/locales/voter/*.json":
            langs = [x[0:-3] for x in os.listdir("po/voter") if x[-3:] == ".po"]
            langs.sort()
            for x in langs:
                reference["/static/locales/voter/{}.json".format(x)] = None
        elif f == "/static/frontend/translations/*.json":
            langs = [x for x in os.listdir("frontend/translations") if x[-5:] == ".json"]
            langs.sort()
            for x in langs:
                reference["/static/frontend/translations/{}".format(x)] = None
        elif "*" in f:
            print("Wildcard not supported in {}".format(f))
            sys.exit(1)
        else:
            reference[f] = descr
    new_reference = {}
    for f, descr in reference.items():
        if type(descr) == dict:
            new_reference[f] = {}
            for lang, descr in descr.items():
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
            json.dump(new_reference, f)

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

    # commit 
    if status.msg != b'':
        Elogme("Commit log for election {} is {}".format(uuid,
            status.msg.decode()))
    commit(args.wdir, uuid, status.msg)
    
if args.logfile:
    log_file.close()
