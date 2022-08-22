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
belenios-tool setup generate-credentials $uuid $group --count 102
mv *.pubcreds public_creds.txt
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
belenios-tool setup make-election $uuid $group --template $BELENIOS/tests/tool/templates/questions-mj.json

header "Simulate votes"

cat > votes.txt <<EOF
[[1,1,1,1]]
[[1,1,1,1]]
[[1,1,1,1]]
[[1,1,1,1]]
[[1,1,1,1]]
[[1,1,1,1]]
[[1,1,2,1]]
[[1,1,2,1]]
[[1,1,2,1]]
[[1,1,2,1]]
[[1,1,2,2]]
[[1,1,2,2]]
[[1,1,2,2]]
[[1,1,2,2]]
[[1,1,2,2]]
[[1,1,2,2]]
[[1,1,2,2]]
[[1,1,2,2]]
[[1,1,2,2]]
[[1,1,2,2]]
[[1,1,2,2]]
[[1,1,2,2]]
[[1,1,2,2]]
[[1,1,2,2]]
[[1,1,2,2]]
[[1,2,2,3]]
[[1,2,2,3]]
[[1,2,2,3]]
[[1,2,2,3]]
[[1,2,2,3]]
[[2,2,3,3]]
[[2,2,3,3]]
[[2,2,3,3]]
[[2,2,3,3]]
[[2,2,3,3]]
[[2,2,3,3]]
[[2,2,3,3]]
[[2,2,3,3]]
[[2,2,3,3]]
[[2,2,3,3]]
[[2,2,3,3]]
[[2,2,3,3]]
[[2,2,3,3]]
[[2,2,3,3]]
[[2,2,3,3]]
[[2,2,3,4]]
[[2,2,3,4]]
[[2,2,3,4]]
[[2,2,3,4]]
[[2,2,3,4]]
[[2,2,3,4]]
[[2,2,3,4]]
[[2,2,3,4]]
[[2,2,3,4]]
[[2,2,3,4]]
[[3,3,3,4]]
[[3,3,3,4]]
[[3,3,3,4]]
[[3,3,3,4]]
[[3,3,3,4]]
[[3,3,3,4]]
[[3,3,3,4]]
[[3,3,3,4]]
[[3,3,3,4]]
[[3,3,3,4]]
[[3,3,3,4]]
[[3,3,3,4]]
[[3,3,3,4]]
[[3,3,3,4]]
[[3,3,3,4]]
[[3,3,4,5]]
[[3,3,4,5]]
[[3,3,4,5]]
[[3,3,4,5]]
[[3,3,4,5]]
[[4,4,4,5]]
[[4,4,4,5]]
[[4,4,4,5]]
[[4,4,4,5]]
[[4,4,4,5]]
[[4,4,4,5]]
[[4,4,4,5]]
[[4,4,4,5]]
[[4,4,4,5]]
[[4,4,4,5]]
[[4,4,4,5]]
[[4,4,4,5]]
[[4,4,4,5]]
[[4,4,4,5]]
[[4,5,4,5]]
[[5,5,4,5]]
[[5,5,4,5]]
[[5,5,4,5]]
[[5,5,4,5]]
[[5,5,5,5]]
[[5,5,5,5]]
[[5,5,5,5]]
[[5,5,5,5]]
[[5,5,5,5]]
[[5,5,5,5]]
[[5,0,5,5]]
[[0,0,0,0]]
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
cp election.json public_creds.txt trustees.json "$tdir"
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

belenios-tool election validate
rm -f shuffles.jsons

header "Perform final verification"

belenios-tool election verify

header "Apply Majority Judgment method"

cat > mj.reference <<EOF
{"raw":[[30,25,20,15,10],[25,30,20,14,11],[6,24,40,24,6],[10,15,20,25,30]],"valid":100,"blank":1,"invalid":[[5,0,5,5]],"winners":[[0],[1],[2],[3]]}
EOF

if command -v jq > /dev/null; then
    if diff -u mj.reference <(jq --compact-output '.result[0]' < result.json | belenios-tool method majority-judgment --ngrades 5 --blank-allowed true); then
        echo "Majority Judgment output is identical!"
    else
        echo "Differences in Majority Judgment output!"
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
