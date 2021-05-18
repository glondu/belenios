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

UUID=`belenios-tool generate-token`
echo "UUID of the election is $UUID"

DIR=$BELENIOS/tests/tool/data/$UUID
mkdir $DIR
cd $DIR

# Common options
uuid="--uuid $UUID"
group="--group $BELENIOS/files/groups/default.json"

# Generate credentials
cat > voters.txt <<EOF
voter1@example.com,voter1,1000
voter2@example.com,voter2,2000
voter3@example.com,voter3,3000
voter4@example.com,voter4,4000
voter5@example.com,voter5,90000
EOF
belenios-tool credgen $uuid $group --file voters.txt
mv *.pubcreds public_creds.txt
mv *.privcreds private_creds.txt

# Generate trustee keys
belenios-tool trustee-keygen $group
belenios-tool trustee-keygen $group
belenios-tool trustee-keygen $group
cat *.pubkey > public_keys.jsons

# Generate trustee parameters
belenios-tool mktrustees
rm public_keys.jsons

# Generate election parameters
belenios-tool mkelection $uuid $group --template $BELENIOS/tests/tool/templates/questions.json

header "Simulate votes"

cat > votes.txt <<EOF
[[1,0],[1,0,0]]
[[1,0],[0,1,0]]
[[0,1],[0,0,1]]
[[1,0],[1,0,0]]
[[0,0],[0,1,0]]
EOF

paste private_creds.txt votes.txt | while read id cred vote; do
    BALLOT="$(belenios-tool vote --privcred <(echo "$cred") --ballot <(echo "$vote"))"
    HASH="$(printf "%s" "$BALLOT" | belenios-tool sha256-b64)"
    echo "$BALLOT"
    echo "Voter $id voted with $HASH" >&2
    echo >&2
done > ballots.tmp
mv ballots.tmp ballots.jsons

header "Perform verification"

belenios-tool verify

header "Simulate and verify update"

tdir="$(mktemp -d)"
cp election.json public_creds.txt trustees.json "$tdir"
head -n3 ballots.jsons > "$tdir/ballots.jsons"
belenios-tool verify-diff --dir1="$tdir" --dir2=.
rm -rf "$tdir"

header "Perform decryption"

for u in *.privkey; do
    belenios-tool decrypt --privkey $u
    echo >&2
done > partial_decryptions.tmp
mv partial_decryptions.tmp partial_decryptions.jsons

header "Finalize tally"

belenios-tool validate

header "Perform final verification"

belenios-tool verify

header "Check result"

cat > result.reference <<EOF
[[7000,3000],[5000,92000,3000]]
EOF

if command -v jq > /dev/null; then
    if diff -u result.reference <(jq --compact-output '.result' < result.json); then
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
echo "  $DIR/result.json"
echo
echo "=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-="
echo
