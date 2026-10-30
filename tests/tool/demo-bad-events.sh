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

UNEXPECTED_EVENT="unexpected .* event at height"

# Create a new election with 3 voters and 1 trustee, using template $1,
# in a fresh directory that becomes the current one
setup_election () {
    UUID=`belenios-tool setup generate-token`
    echo "UUID of the election is $UUID"

    DIR=$BELENIOS/tests/tool/data/$UUID
    mkdir -p $DIR
    cd $DIR

    uuid="--uuid $UUID"
    group="--group Ed25519"

    cat > voters.json <<EOF
[{"login":"voter1","address":"voter1@example.com"},{"login":"voter2","address":"voter2@example.com"},{"login":"voter3","address":"voter3@example.com"}]
EOF

    belenios-tool setup generate-credentials $uuid $group --file voters.json > /dev/null
    mv *.pubcreds public_creds.json
    mv *.privcreds private_creds.json

    belenios-tool setup generate-trustee-key $group --name "Trustee 1"
    cat *.pubkey > public_keys.jsons
    belenios-tool setup make-trustees
    rm public_keys.jsons

    belenios-tool setup make-election $uuid $group --template $1

    belenios-tool archive init
    rm -f election.json trustees.json public_creds.json

    BEL=$UUID.bel
}

# Cast ballot $2 on behalf of voter $1
vote () {
    cred="$(jq --raw-output .[\""$1"\"] private_creds.json)"
    belenios-tool election generate-ballot --privcred <(echo "$cred") --choice <(echo "$2") \
        | belenios-tool archive add-event --type=Ballot
}

# Save the current archive as the starting point of expect_rejected
snapshot () {
    rm -rf pristine
    mkdir pristine
    cp $BEL private_creds.json pristine/
}

# Run $2 on a copy of the archive saved by snapshot, and check that the
# resulting archive is rejected by verify with an error matching $3
# (error messages are wrapped, so line breaks are turned into spaces)
expect_rejected () {
    header "Check rejection: $1"
    rm -rf bad
    cp -r pristine bad
    ( cd bad && eval "$2" )
    if belenios-tool election verify --dir bad > bad.out 2>&1; then
        cat bad.out
        echo "verify accepted an archive with $1!"
        exit 1
    elif tr -s " \n" "  " < bad.out | grep -q "$3"; then
        echo "verify rejected it as expected"
    else
        cat bad.out
        echo "verify rejected it, but not for the expected reason!"
        exit 1
    fi
}

header "Setup homomorphic election"

setup_election $BELENIOS/tests/tool/templates/questions.json
H_VOTE="[[1,0],[1,0,0]]"

header "Simulate votes"

vote voter1 "$H_VOTE"
vote voter2 "$H_VOTE"
belenios-tool election verify
snapshot

expect_rejected "a ballot after EndBallots" '
    belenios-tool archive add-event --type=EndBallots < /dev/null
    vote voter3 "$H_VOTE"
' "$UNEXPECTED_EVENT"

expect_rejected "a second Setup event" '
    for i in 2 3 4 5; do tar -xOf $BEL $(tar -tf $BEL | sed -n ${i}p); echo; done \
        | belenios-tool archive add-event --type=Setup
' "$UNEXPECTED_EVENT"

expect_rejected "a partial decryption without encrypted tally" '
    echo "{}" | belenios-tool archive add-event --type=PartialDecryption
' "$UNEXPECTED_EVENT"

expect_rejected "EndShuffles without Shuffle" '
    belenios-tool archive add-event --type=EndBallots < /dev/null
    belenios-tool election compute-encrypted-tally | belenios-tool archive add-event --type=EncryptedTally
    belenios-tool archive add-event --type=EndShuffles < /dev/null
' "$UNEXPECTED_EVENT"

expect_rejected "a shuffle in an election without non-homomorphic questions" '
    belenios-tool archive add-event --type=EndBallots < /dev/null
    belenios-tool election compute-encrypted-tally | belenios-tool archive add-event --type=EncryptedTally
    belenios-tool election shuffle --key ../*.privkey --trustee-id 1 | belenios-tool archive add-event --type=Shuffle
    belenios-tool archive add-event --type=EndShuffles < /dev/null
' "no non-homomorphic questions and shuffles were found"

header "Finish the election"

belenios-tool archive add-event --type=EndBallots < /dev/null
belenios-tool election compute-encrypted-tally | belenios-tool archive add-event --type=EncryptedTally
belenios-tool election decrypt --privkey *.privkey --trustee-id 1 | belenios-tool archive add-event --type=PartialDecryption
belenios-tool election compute-result | belenios-tool archive add-event --type=Result
belenios-tool election verify
snapshot

expect_rejected "an event after Result" '
    vote voter3 "$H_VOTE"
' "$UNEXPECTED_EVENT"

header "Setup non-homomorphic election"

setup_election $BELENIOS/tests/tool/templates/questions-nh.json
NH_VOTE="[[1,0],[1,0,0],[1,2,3]]"

header "Simulate votes"

vote voter1 "$NH_VOTE"
vote voter2 "$NH_VOTE"
belenios-tool archive add-event --type=EndBallots < /dev/null
belenios-tool election compute-encrypted-tally | belenios-tool archive add-event --type=EncryptedTally
belenios-tool election verify
snapshot

# The trustee refuses to decrypt without shuffles, so forge an owned
# partial decryption: its contents do not matter, the absence of
# shuffles must be detected first
expect_rejected "a partial decryption without shuffles" '
    PD="{}"
    printf "%s\n{\"owner\":1,\"payload\":\"%s\"}\n" "$PD" "$(printf "%s" "$PD" | sha256sum | cut -d" " -f1)" \
        | belenios-tool archive add-event --type=PartialDecryption
' "partial decryptions were found without shuffles"

expect_rejected "a partial decryption after Shuffle without EndShuffles" '
    belenios-tool election shuffle --key ../*.privkey --trustee-id 1 | belenios-tool archive add-event --type=Shuffle
    belenios-tool election decrypt --privkey ../*.privkey --trustee-id 1 | belenios-tool archive add-event --type=PartialDecryption
' "$UNEXPECTED_EVENT"

header "Finish the election"

belenios-tool election shuffle --key *.privkey --trustee-id 1 | belenios-tool archive add-event --type=Shuffle
belenios-tool archive add-event --type=EndShuffles < /dev/null
belenios-tool election decrypt --privkey *.privkey --trustee-id 1 | belenios-tool archive add-event --type=PartialDecryption
belenios-tool election compute-result | belenios-tool archive add-event --type=Result
belenios-tool election verify

echo
echo "=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-="
echo
echo "All ill-formed archives were rejected!"
echo
echo "=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-="
echo
