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
belenios-tool setup generate-credentials $uuid $group --count 60 | tee generate-credentials.out
mv *.pubcreds public_creds.json
mv *.privcreds private_creds.json
paste <(jq --raw-output 'keys_unsorted[]' < private_creds.json) <(jq --raw-output '.[]' < private_creds.json) > private_creds.txt

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
    belenios-tool election generate-ballot --privcred <(echo "$cred") --ballot <(echo "$vote") | belenios-tool archive add-event --type=Ballot
    echo "Voter $id voted" >&2
    echo >&2
done

header "Perform verification"

belenios-tool election verify

header "End voting phase"

belenios-tool archive add-event --type=EndBallots < /dev/null
belenios-tool election compute-encrypted-tally | belenios-tool archive add-event --type=EncryptedTally
belenios-tool election verify

header "Shuffle ciphertexts"

belenios-tool election shuffle --trustee-id=1 | belenios-tool archive add-event --type=Shuffle
echo >&2
belenios-tool election shuffle --trustee-id=2 | belenios-tool archive add-event --type=Shuffle
belenios-tool archive add-event --type=EndShuffles < /dev/null

header "Perform decryption"

trustee_id=1
for u in *.privkey; do
    belenios-tool election decrypt --privkey $u --trustee-id $trustee_id | belenios-tool archive add-event --type=PartialDecryption
    echo >&2
    : $((trustee_id++))
done

header "Finalize tally"

belenios-tool election compute-result | belenios-tool archive add-event --type=Result

header "Perform final verification"

belenios-tool election verify

header "Apply Schulze method"

cat > schulze.reference <<EOF
{"valid":60,"blank":0,"raw":[[0,33,25],[27,0,42],[35,18,0]],"beatpaths":[[[33,27],[33,27],[33,27]],[[35,25],[33,27],[42,18]],[[35,25],[33,27],[33,27]]],"winners":[[1],[2],[0]]}
EOF

RESULT=$(tar -tf $UUID.bel | tail -n2 | head -n1)
if command -v jq > /dev/null; then
    if diff -u schulze.reference <(tar -xOf $UUID.bel $RESULT | jq --compact-output '.result[2]' | belenios-tool method schulze --blank-allowed true); then
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
echo "  $DIR"
echo
echo "=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-="
echo
