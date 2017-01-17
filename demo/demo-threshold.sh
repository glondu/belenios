#!/bin/bash

set -e

export BELENIOS_USE_URANDOM=1

BELENIOS=${BELENIOS:-$PWD}

belenios-tool () {
    $BELENIOS/_build/belenios-tool "$@"
}

header () {
    echo
    echo "=-=-= $1 =-=-="
    echo
}

header "Setup election"

UUID=`uuidgen`
echo "UUID of the election is $UUID"

DIR=$BELENIOS/demo/data/$UUID
mkdir $DIR
cd $DIR

# Common options
uuid="--uuid $UUID"
group="--group $BELENIOS/demo/groups/default.json"

# Generate credentials
belenios-tool credgen $uuid $group --count 5
mv *.pubcreds public_creds.txt
mv *.privcreds private_creds.txt

# Generate trustee keys
ttkeygen () {
    belenios-tool threshold-trustee-keygen $group "$@"
}
ttkeygen --step 1
ttkeygen --step 1
ttkeygen --step 1
cat *.cert > certs.jsons
ttkeygen --certs certs.jsons --step 2
for u in *.key; do
    ttkeygen --certs certs.jsons --key $u --step 3 --threshold 2
done > polynomials.jsons
ttkeygen --certs certs.jsons --step 4 --polynomials polynomials.jsons
for u in *.key; do
    b=${u%.key}
    ttkeygen --certs certs.jsons --key $u --step 5 < $b.vinput > $b.voutput
done
cat *.voutput | ttkeygen --certs certs.jsons --step 6 --polynomials polynomials.jsons > threshold.json

# Generate election parameters
belenios-tool mkelection $uuid $group --template $BELENIOS/demo/templates/questions.json

header "Simulate votes"

cat > votes.txt <<EOF
[[1,0],[1,0,0]]
[[1,0],[0,1,0]]
[[0,1],[0,0,1]]
[[1,0],[1,0,0]]
[[0,0],[0,1,0]]
EOF

paste private_creds.txt votes.txt | while read id cred vote; do
    belenios-tool vote --privcred <(echo "$cred") --ballot <(echo "$vote")
    echo "Voter $id voted" >&2
    echo >&2
done > ballots.tmp
mv ballots.tmp ballots.jsons

header "Perform verification"

belenios-tool verify

header "Simulate and verify update"

tdir="$(mktemp -d)"
cp election.json threshold.json public_creds.txt "$tdir"
head -n3 ballots.jsons > "$tdir/ballots.jsons"
belenios-tool verify-diff --dir1="$tdir" --dir2=.
rm -rf "$tdir"

header "Perform decryption"

for u in *.key; do
    belenios-tool threshold-decrypt --key $u --decryption-key ${u%.key}.dkey
    echo >&2
done > partial_decryptions.tmp
head -n2 partial_decryptions.tmp > partial_decryptions.jsons

header "Finalize tally"

belenios-tool finalize

header "Perform final verification"

belenios-tool verify

echo
echo "=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-="
echo
echo "The simulated election was successful! Its result can be seen in"
echo "  $DIR/result.json"
echo
echo "=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-="
echo
