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
group="--group Ed25519"

# Generate credentials
belenios-tool setup generate-credentials $uuid $group --count 102 | tee generate-credentials.out
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
belenios-tool setup make-election $uuid $group --template $BELENIOS/tests/tool/templates/questions-mj.json

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
    belenios-tool election generate-ballot --privcred <(echo "$cred") --choice <(echo "$vote") | belenios-tool archive add-event --type=Ballot
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

header "Apply Majority Judgment method"

cat > mj.reference <<EOF
{"raw":[[30,25,20,15,10],[25,30,20,14,11],[6,24,40,24,6],[10,15,20,25,30]],"valid":100,"blank":1,"invalid":[[5,0,5,5]],"winners":[[0],[1],[2],[3]]}
EOF

RESULT=$(tar -tf $UUID.bel | tail -n2 | head -n1)
if command -v jq > /dev/null; then
    if diff -u mj.reference <(tar -xOf $UUID.bel $RESULT | jq --compact-output '.result[0]' | belenios-tool method majority-judgment --ngrades 5 --blank-allowed true); then
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
echo "  $DIR"
echo
echo "=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-="
echo
