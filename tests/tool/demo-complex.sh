#!/bin/bash

set -e

export BELENIOS_USE_URANDOM=1

: ${N:=10}
BELENIOS=${BELENIOS:-$(dirname $(dirname $PWD))}
export PATH=$BELENIOS/_run/tool-debug/bin:$PATH

if [ -x /usr/bin/time ]; then
    TIME=/usr/bin/time
fi

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
for i in $(seq 1 $N); do
    printf "voter%05d@example.com,voter%05d\n" $i $i >> voters.txt
done
belenios-tool setup generate-credentials $uuid $group --file voters.txt | tee generate-credentials.out
mv *.pubcreds public_creds.json
mv *.privcreds private_creds.json
paste <(jq --raw-output 'keys_unsorted[]' < private_creds.json) <(jq --raw-output '.[]' < private_creds.json) > private_creds.txt

# Generate trustee keys
belenios-tool setup generate-trustee-key $group
cat *.pubkey > public_keys.jsons

# Generate trustee parameters
belenios-tool setup make-trustees
rm public_keys.jsons

# Generate election parameters
belenios-tool setup make-election $uuid $group --template $BELENIOS/tests/tool/templates/questions-complex.json

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

header "Generate ballots"

printf "[[1,0,0,0,0,0,0,0],[1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],[1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0]]" > choice.json

mkdir ballots

VOTE_SCRIPT='
echo $1 | {
  read id cred
  belenios-tool election generate-ballot --privcred <(echo "$cred") --choice choice.json > ballots/$id.json 2>/dev/null
  HASH="$(belenios-tool sha256-b64 < ballots/$id.json)"
  echo "Voter $id voted with $HASH" >&2
}'

if command -v parallel >/dev/null; then
    xargs -d '\n' parallel -j $(nproc) bash -c "$VOTE_SCRIPT" vote -- < private_creds.txt
else
    while read a; do bash -c "$VOTE_SCRIPT" vote "$a"; done < private_creds.txt
fi

header "Cast ballots"

$TIME sh -c '
for u in ballots/*.json; do
    belenios-tool archive add-event --type=Ballot < $u
done'

rm -rf ballots

header "Compute encrypted tally"

belenios-tool archive add-event --type=EndBallots < /dev/null
$TIME belenios-tool election compute-encrypted-tally | belenios-tool archive add-event --type=EncryptedTally

header "Perform decryption"

trustee_id=1
for u in *.privkey; do
    $TIME belenios-tool election decrypt --privkey $u --trustee-id $trustee_id | belenios-tool archive add-event --type=PartialDecryption
    echo >&2
    : $((trustee_id++))
done

header "Finalize tally"

$TIME belenios-tool election compute-result | belenios-tool archive add-event --type=Result

header "Perform final verification"

$TIME belenios-tool election verify

echo
echo "=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-="
echo
echo "The simulated election was successful! Its result can be seen in"
echo "  $DIR"
echo
echo "=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-="
echo
