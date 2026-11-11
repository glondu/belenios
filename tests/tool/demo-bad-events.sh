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
    mkdir setup
    mv election.json trustees.json public_creds.json setup/

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

# Recreate the archive with public credentials modified by jq filter $1
init_with_public_creds () {
    rm -f $BEL
    cp ../setup/election.json ../setup/trustees.json .
    jq --compact-output "$1" ../setup/public_creds.json > public_creds.json
    belenios-tool archive init
}

# Recreate the archive with an extra public credential $1 (with the
# right key)
init_with_credential () {
    init_with_public_creds ". + {\"$(printf "%s" "$1" | sha256sum | cut -d" " -f1)\": {credential: \"$1\", id: \"intruder\"}}"
}

expect_rejected "the identity element as credential" '
    init_with_credential 0000000000000000000000000000000000000000000000000000000000000001
' "invalid credential:"

expect_rejected "a credential of order 2" '
    init_with_credential 7fffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffec
' "invalid credential:"

expect_rejected "a credential with a wrong key" '
    init_with_public_creds "to_entries | .[0].key = \"0000000000000000000000000000000000000000000000000000000000000000\" | from_entries"
' "invalid key for credential"

# Store $1 in extracted/ as a $2 (data or event) file, and print its hash
add_object () {
    printf "%s" "$1" > object.tmp
    h=$(sha256sum object.tmp | cut -d" " -f1)
    mv object.tmp extracted/$h.$2.json
    echo $h
}

# Recreate the archive with a credentials certificate for election $1,
# with $2 credentials and public credentials hash $3 (right values when
# empty), signed with a bogus signature (the tool cannot sign one)
init_with_certificate () {
    rm -f $BEL
    cp ../setup/election.json ../setup/trustees.json ../setup/public_creds.json .
    belenios-tool archive init
    mkdir extracted
    tar -xf $BEL -C extracted
    SETUP=$(tar -tf $BEL | sed -n 5p)
    CREDS=$(jq --raw-output .credentials extracted/$SETUP).data.json
    KEY=$(jq --raw-output "[.[]][0].credential" extracted/$CREDS)
    CERT=$(jq --null-input --compact-output --arg uuid "${1:-$UUID}" --argjson n "${2:-3}" \
        --arg h "${3:-$(sha256sum < extracted/$CREDS | cut -d" " -f1)}" --arg k "$KEY" \
        "{message: {uuid: \$uuid, voter_list_length: \$n, public_creds_hash: \$h, verification_key: \$k, encryption_key: \$k}, signature: {challenge: \"1\", response: \"1\"}}")
    CERT_H=$(add_object "$CERT" data)
    SETUP_H=$(add_object "$(jq --compact-output --arg h $CERT_H ". + {credentials_certificate: \$h}" extracted/$SETUP)" data)
    rm extracted/*.event.json
    add_object "{\"height\":0,\"type\":[\"Setup\"],\"payload\":\"$SETUP_H\"}" event > /dev/null
    belenios-tool archive make --dir extracted > $BEL
}

expect_rejected "a credentials certificate for another election" '
    init_with_certificate $(belenios-tool setup generate-token)
' "invalid credentials certificate: wrong uuid"

expect_rejected "a credentials certificate with a wrong number of credentials" '
    init_with_certificate "" 2
' "invalid credentials certificate: wrong number of credentials"

expect_rejected "a credentials certificate for other credentials" '
    init_with_certificate "" "" 0000000000000000000000000000000000000000000000000000000000000000
' "invalid credentials certificate: wrong public credentials"

expect_rejected "a credentials certificate with a bad signature" '
    init_with_certificate
' "invalid credentials certificate: bad signature"

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

# Add an EncryptedTally event whose sized_encrypted_tally (last line
# output by compute-encrypted-tally) is modified by jq filter $1
add_modified_tally () {
    belenios-tool archive add-event --type=EndBallots < /dev/null
    belenios-tool election compute-encrypted-tally \
        | { IFS= read -r et; echo "$et"; jq --compact-output "$1"; } \
        | belenios-tool archive add-event --type=EncryptedTally
}

expect_rejected "a wrong number of tallied ballots" '
    add_modified_tally ".num_tallied = 3"
' "wrong number of tallied ballots"

expect_rejected "a wrong total weight of tallied ballots" '
    add_modified_tally ".total_weight = 3"
' "wrong total weight of tallied ballots"

header "Finish the election"

belenios-tool archive add-event --type=EndBallots < /dev/null
belenios-tool election compute-encrypted-tally | belenios-tool archive add-event --type=EncryptedTally
belenios-tool election verify
snapshot

decrypt () {
    belenios-tool election decrypt --privkey ../*.privkey --trustee-id 1
}

# Add a partial decryption of trustee 1, with the partial decryption
# (first line output by decrypt) modified by jq filter $1, and the
# owned partial decryption (second line) by jq filter $2
add_modified_pd () {
    decrypt | {
        IFS= read -r pd
        pd="$(echo "$pd" | jq --compact-output "$1")"
        echo "$pd"
        jq --compact-output --arg h "$(printf "%s" "$pd" | sha256sum | cut -d" " -f1)" ".payload = \$h | $2"
    } | belenios-tool archive add-event --type=PartialDecryption
}

expect_rejected "an invalid partial decryption" '
    add_modified_pd ".signature.response = \"1\"" "."
' "invalid partial decryption from trustee 1"

expect_rejected "a partial decryption from an unknown trustee" '
    add_modified_pd "." ".owner = 2"
' "partial decryption from unknown trustee 2"

expect_rejected "two partial decryptions from the same trustee" '
    decrypt | belenios-tool archive add-event --type=PartialDecryption
    decrypt | belenios-tool archive add-event --type=PartialDecryption
' "duplicate partial decryption from trustee 1"

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

expect_rejected "a shuffle from an unknown trustee" '
    belenios-tool election shuffle --key ../*.privkey --trustee-id 2 | belenios-tool archive add-event --type=Shuffle
' "shuffle from unknown trustee 2"

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
