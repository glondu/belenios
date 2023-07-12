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
mkdir -p $DIR
cd $DIR

# Common options
uuid="--uuid $UUID"
group="--group Ed25519"

# Generate credentials
cat > voters.txt <<EOF
voter1@example.com,voter1,1000000000
voter2@example.com,voter2,2000000000
voter3@example.com,voter3,3000000000
voter4@example.com,voter4,4000000000
voter5@example.com,voter5,90000000000
EOF
belenios-tool setup generate-credentials $uuid $group --file voters.txt | tee generate-credentials.out
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

cat > votes.txt <<EOF
[[1,0],[1,0,0]]
[[1,0],[0,1,0]]
[[0,1],[0,0,1]]
[[1,0],[1,0,0]]
[[0,0],[0,1,0]]
EOF

paste private_creds.txt votes.txt | while read id cred vote; do
    BALLOT="$(belenios-tool election generate-ballot --privcred <(echo "$cred") --choice <(echo "$vote"))"
    belenios-tool election verify-ballot --ballot <(echo "$BALLOT")
    HASH="$(printf "%s" "$BALLOT" | belenios-tool sha256-b64)"
    echo "$BALLOT" | belenios-tool archive add-event --type=Ballot
    echo "Voter $id voted with $HASH" >&2
    echo >&2
done

header "Perform verification (skip-ballot-check)"

belenios-tool election verify --skip-ballot-check

header "Perform verification"

belenios-tool election verify

header "Simulate revotes and verify diff"

tdir="$(mktemp -d)"
cp $UUID.bel "$tdir"
paste <(head -n 3 private_creds.txt) <(head -n 3 votes.txt) | while read id cred vote; do
    BALLOT="$(belenios-tool election generate-ballot --privcred <(echo "$cred") --choice <(echo "$vote"))"
    HASH="$(printf "%s" "$BALLOT" | belenios-tool sha256-b64)"
    echo "$BALLOT" | belenios-tool archive add-event --type=Ballot
    echo "Voter $id voted with $HASH" >&2
    echo >&2
done
belenios-tool election verify-diff --dir1="$tdir" --dir2=.
rm -rf "$tdir"

header "End voting phase"

belenios-tool archive add-event --type=EndBallots < /dev/null
belenios-tool election compute-encrypted-tally | belenios-tool archive add-event --type=EncryptedTally
belenios-tool election verify

header "Check voters"

NUMBER_OF_VOTERS="$(belenios-tool election compute-voters --privcreds private_creds.json | wc -l)"
if [ "$NUMBER_OF_VOTERS" -eq "5" ]; then
    echo "Number of voters is correct"
else
    echo "Number of voters does not match!"
    exit 1
fi

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
[["7000000000","3000000000"],["5000000000","92000000000","3000000000"]]
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
