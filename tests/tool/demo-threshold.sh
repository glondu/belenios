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
group="--group BELENIOS-2048"

# Generate credentials
belenios-tool setup generate-credentials $uuid $group --count 5
mv *.pubcreds public_creds.json
mv *.privcreds private_creds.txt

# Generate trustee keys
ttkeygen () {
    belenios-tool setup generate-trustee-key-threshold $group "$@"
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

# Generate mandatory (server) key
belenios-tool setup generate-trustee-key $group
cat *.pubkey > public_keys.jsons

# Generate trustee parameters
belenios-tool setup make-trustees
rm threshold.json

# Generate election parameters
belenios-tool setup make-election $uuid $group --template $BELENIOS/tests/tool/templates/questions.json

header "Simulate votes"

cat > votes.txt <<EOF
[[1,0],[1,0,0]]
[[1,0],[0,1,0]]
[[0,1],[0,0,1]]
[[1,0],[1,0,0]]
[[0,0],[0,1,0]]
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
cp election.json trustees.json public_creds.json "$tdir"
head -n3 ballots.jsons > "$tdir/ballots.jsons"
belenios-tool election verify-diff --dir1="$tdir" --dir2=.
rm -rf "$tdir"

header "Perform decryption (threshold)"

for u in *.key; do
    belenios-tool election decrypt-threshold --key $u --decryption-key ${u%.key}.dkey
    echo >&2
done > partial_decryptions.tmp
head -n2 partial_decryptions.tmp > partial_decryptions.jsons

header "Perform decryption (mandatory)"

for u in *.privkey; do
    belenios-tool election decrypt --privkey $u
    echo >&2
done >> partial_decryptions.jsons

header "Finalize tally"

belenios-tool election compute-result

header "Perform final verification"

belenios-tool election verify

echo
echo "=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-="
echo
echo "The simulated election was successful! Its result can be seen in"
echo "  $DIR/result.json"
echo
echo "=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-="
echo
