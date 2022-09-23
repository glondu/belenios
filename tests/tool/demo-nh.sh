#!/bin/bash

set -e

export BELENIOS_USE_URANDOM=1

BELENIOS=${BELENIOS:-$(dirname $(dirname $PWD))}

belenios-tool () {
    $BELENIOS/_run/tool-debug/bin/belenios-tool "$@"
}

header () {
    echo
    echo "=-=-= $1 =-=-="
    echo
}

header "Setup election"

UUID=`belenios-tool setup generate-token`
echo "UUID of the election is $UUID"

DIR=$BELENIOS/tests/tool/data/$UUID
mkdir $DIR
cd $DIR

# Common options
uuid="--uuid $UUID"
group="--group RFC-3526-2048"

# Generate credentials
belenios-tool setup generate-credentials $uuid $group --count 60
mv *.pubcreds public_creds.json
mv *.privcreds private_creds.txt

# Generate trustee keys
belenios-tool setup generate-trustee-key $group
belenios-tool setup generate-trustee-key $group
belenios-tool setup generate-trustee-key $group
cat *.pubkey > public_keys.jsons

# Generate trustee parameters
belenios-tool setup make-trustees
rm public_keys.jsons

# Generate election parameters
belenios-tool setup make-election $uuid $group --template $BELENIOS/tests/tool/templates/questions-nh.json

header "Simulate votes"

cat > votes.txt <<EOF
[[1,0],[1,0,0],[1,2,3]]
[[1,0],[1,0,0],[1,2,3]]
[[1,0],[1,0,0],[1,2,3]]
[[1,0],[1,0,0],[1,2,3]]
[[1,0],[1,0,0],[1,2,3]]
[[1,0],[1,0,0],[1,2,3]]
[[1,0],[1,0,0],[1,2,3]]
[[1,0],[1,0,0],[1,2,3]]
[[1,0],[1,0,0],[1,2,3]]
[[1,0],[1,0,0],[1,2,3]]
[[1,0],[1,0,0],[1,2,3]]
[[1,0],[1,0,0],[1,2,3]]
[[1,0],[1,0,0],[1,2,3]]
[[1,0],[1,0,0],[1,2,3]]
[[1,0],[1,0,0],[1,2,3]]
[[1,0],[1,0,0],[1,2,3]]
[[1,0],[1,0,0],[1,2,3]]
[[1,0],[1,0,0],[1,2,3]]
[[1,0],[1,0,0],[1,2,3]]
[[1,0],[1,0,0],[1,2,3]]
[[1,0],[1,0,0],[1,2,3]]
[[1,0],[1,0,0],[1,2,3]]
[[1,0],[1,0,0],[1,2,3]]
[[1,0],[0,1,0],[2,1,3]]
[[1,0],[0,1,0],[2,1,3]]
[[0,1],[0,0,1],[3,1,2]]
[[0,1],[0,0,1],[3,1,2]]
[[0,1],[0,0,1],[3,1,2]]
[[0,1],[0,0,1],[3,1,2]]
[[0,1],[0,0,1],[3,1,2]]
[[0,1],[0,0,1],[3,1,2]]
[[0,1],[0,0,1],[3,1,2]]
[[0,1],[0,0,1],[3,1,2]]
[[0,1],[0,0,1],[3,1,2]]
[[0,1],[0,0,1],[3,1,2]]
[[0,1],[0,0,1],[3,1,2]]
[[0,1],[0,0,1],[3,1,2]]
[[0,1],[0,0,1],[3,1,2]]
[[0,1],[0,0,1],[3,1,2]]
[[0,1],[0,0,1],[3,1,2]]
[[0,1],[0,0,1],[3,1,2]]
[[0,1],[0,0,1],[3,1,2]]
[[1,0],[1,0,0],[2,3,1]]
[[1,0],[1,0,0],[2,3,1]]
[[1,0],[1,0,0],[2,3,1]]
[[1,0],[1,0,0],[2,3,1]]
[[1,0],[1,0,0],[2,3,1]]
[[1,0],[1,0,0],[2,3,1]]
[[1,0],[1,0,0],[2,3,1]]
[[1,0],[1,0,0],[2,3,1]]
[[1,0],[1,0,0],[2,3,1]]
[[1,0],[1,0,0],[2,3,1]]
[[0,0],[0,1,0],[3,2,1]]
[[0,0],[0,1,0],[3,2,1]]
[[0,0],[0,1,0],[3,2,1]]
[[0,0],[0,1,0],[3,2,1]]
[[0,0],[0,1,0],[3,2,1]]
[[0,0],[0,1,0],[3,2,1]]
[[0,0],[0,1,0],[3,2,1]]
[[0,0],[0,1,0],[3,2,1]]
EOF

paste private_creds.txt votes.txt | while read id cred vote; do
    belenios-tool election generate-ballot --privcred <(echo "$cred") --ballot <(echo "$vote")
    echo "Voter $id voted" >&2
    echo >&2
done > ballots.tmp
mv ballots.tmp ballots.jsons

header "Perform verification"

belenios-tool election verify

header "Simulate and verify update"

tdir="$(mktemp -d)"
cp election.json public_creds.json trustees.json "$tdir"
head -n3 ballots.jsons > "$tdir/ballots.jsons"
belenios-tool election verify-diff --dir1="$tdir" --dir2=.
rm -rf "$tdir"

header "Shuffle ciphertexts"

belenios-tool election shuffle > shuffles.jsons
echo >&2
belenios-tool election shuffle >> shuffles.jsons

header "Perform decryption"

for u in *.privkey; do
    belenios-tool election decrypt --privkey $u
    echo >&2
done > partial_decryptions.tmp
mv partial_decryptions.tmp partial_decryptions.jsons

header "Finalize tally"

belenios-tool election compute-result
rm -f shuffles.jsons

header "Perform final verification"

belenios-tool election verify

header "Apply Schulze method"

cat > schulze.reference <<EOF
{"valid":60,"blank":0,"raw":[[0,33,25],[27,0,42],[35,18,0]],"beatpaths":[[[33,27],[33,27],[33,27]],[[35,25],[33,27],[42,18]],[[35,25],[33,27],[33,27]]],"winners":[[1],[2],[0]]}
EOF

if command -v jq > /dev/null; then
    if diff -u schulze.reference <(jq --compact-output '.result[2]' < result.json | belenios-tool method schulze --blank-allowed true); then
        echo "Schulze output is identical!"
    else
        echo "Differences in Schulze output!"
        exit 1
    fi
else
    echo "Could not find jq command, test skipped!"
fi

echo
echo "=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-="
echo
echo "The simulated election was successful! Its result can be seen in"
echo "  $DIR/result.json"
echo
echo "=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-="
echo
