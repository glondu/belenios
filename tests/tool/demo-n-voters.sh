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

num_voters="10" # modify the number of voters here

header "Setup election"

UUID=`belenios-tool setup generate-token`
echo "UUID of the election is $UUID"

DIR=$BELENIOS/tests/tool/data/$UUID
mkdir -p $DIR
cd $DIR

uuid="--uuid $UUID"
group="--group Ed25519"


belenios-tool setup generate-credentials $uuid $group --count $num_voters | tee generate-credentials.out
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

yes "[[1,0],[0,1,0]]" | head -n $num_voters | cat > votes.txt

paste private_creds.txt votes.txt | while read id cred vote; do
    BALLOT="$(belenios-tool election generate-ballot --privcred <(echo "$cred") --choice <(echo "$vote") | sed -n 1p)"
    belenios-tool election verify-ballot --ballot <(echo "$BALLOT")
    HASH="$(printf "%s" "$BALLOT" | belenios-tool sha256-b64)"
    echo "$BALLOT" | belenios-tool archive add-event --type=Ballot
    echo "Voter $id voted with $HASH" >&2
    belenios-tool election verify --skip-ballot-check true
    echo >&2
done

header "Perform verification"

belenios-tool election verify

header "End voting phase"

belenios-tool archive add-event --type=EndBallots < /dev/null
belenios-tool election compute-encrypted-tally | belenios-tool archive add-event --type=EncryptedTally
belenios-tool election verify

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

header "Check result"

cat > result.reference <<EOF
[[${num_voters},0],[0,${num_voters},0]]
EOF

RESULT=$(tar -tf $UUID.bel | tail -n2 | head -n1)
if command -v jq > /dev/null; then
    if diff -u result.reference <(tar -xOf $UUID.bel $RESULT | jq --compact-output '.result'); then
        echo "Result is correct!"
    else
        echo "Result is incorrect!"
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
