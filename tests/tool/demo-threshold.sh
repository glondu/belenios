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
belenios-tool setup generate-credentials $uuid $group --count 5 | tee generate-credentials.out
mv *.pubcreds public_creds.json
mv *.privcreds private_creds.json
paste <(jq --raw-output 'keys_unsorted[]' < private_creds.json) <(jq --raw-output '.[]' < private_creds.json) > private_creds.txt

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

# Initialize events
belenios-tool archive init
rm -f election.json trustees.json public_creds.json

# Check public credential fingerprint
EXPECTED_PUBLIC_CREDENTIAL_FINGERPRINT="$(tail -n1 generate-credentials.out| awk '{print $(NF)}')"
ACTUAL_PUBLIC_CREDENTIAL_FINGERPRINT="$(tar -xOf $UUID.bel $(tar -tf $UUID.bel | head -n4 | tail -n1) | belenios-tool sha256-b64)"
if [ "$EXPECTED_PUBLIC_CREDENTIAL_FINGERPRINT" != "$ACTUAL_PUBLIC_CREDENTIAL_FINGERPRINT" ]; then
    echo "Discrepancy in public credential fingerprint"
    exit 2
fi
rm -f generate-credentials.out

header "Simulate votes"

cat > votes.txt <<EOF
[[1,0],[1,0,0]]
[[1,0],[0,1,0]]
[[0,1],[0,0,1]]
[[1,0],[1,0,0]]
[[0,0],[0,1,0]]
EOF

paste private_creds.txt votes.txt | while read id cred vote; do
    belenios-tool election generate-ballot --privcred <(echo "$cred") --ballot <(echo "$vote") | belenios-tool archive add-event --type=Ballot
    echo "Voter $id voted" >&2
    echo >&2
done

header "Perform verification"

belenios-tool election verify

header "Simulate revotes and verify diff"

tdir="$(mktemp -d)"
cp $UUID.bel "$tdir"
paste <(head -n 3 private_creds.txt) <(head -n 3 votes.txt) | while read id cred vote; do
    belenios-tool election generate-ballot --privcred <(echo "$cred") --ballot <(echo "$vote") | belenios-tool archive add-event --type=Ballot
    echo "Voter $id voted" >&2
    echo >&2
done
belenios-tool election verify-diff --dir1="$tdir" --dir2=.
rm -rf "$tdir"

header "End voting phase"

belenios-tool archive add-event --type=EndBallots < /dev/null
belenios-tool election compute-encrypted-tally | belenios-tool archive add-event --type=EncryptedTally
belenios-tool election verify

header "Perform decryption (threshold)"

trustee_id=2
for u in *.key; do
    belenios-tool election decrypt-threshold --key $u --decryption-key ${u%.key}.dkey --trustee-id $trustee_id | belenios-tool archive add-event --type=PartialDecryption
    echo >&2
    : $((trustee_id++))
done

header "Perform decryption (mandatory)"

for u in *.privkey; do
    belenios-tool election decrypt --privkey $u --trustee-id 1 | belenios-tool archive add-event --type=PartialDecryption
    echo >&2
done

header "Finalize tally"

belenios-tool election compute-result | belenios-tool archive add-event --type=Result

header "Perform final verification"

belenios-tool election verify

echo
echo "=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-="
echo
echo "The simulated election was successful! Its result can be seen in"
echo "  $DIR"
echo
echo "=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-="
echo
