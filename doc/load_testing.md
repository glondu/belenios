# Load testing

To set up a load test that will make fake users submit encrypted ballot files, you need to generate these ballot files in advance. A script which does this automatically is available at `tests/load_testing_set_up.py`. Here is an example of usage:

In a local terminal, start your Belenios server: `BELENIOS_SENDMAIL=/path/to/tests/tools/sendmail_fake_to_static.sh demo/run-server.sh`

In another local terminal, execute the script: `USE_HEADLESS_BROWSER=0 SENT_EMAILS_TEXT_FILE_ABSOLUTE_PATH=/path/to/_build/src/static/mail.txt FAKE_SENDMAIL_EXECUTABLE_FILE_PATH_RELATIVE_TO_GIT_REPOSITORY=tests/tools/sendmail_fake_to_static.sh FAKE_SENT_EMAILS_FILE_RELATIVE_URL=static/mail.txt python ./tests/load_testing_set_up.py`

This creates files `voter_row_{i}_crypted_ballot.json`, `voter_row_{i}_privcred.txt`, `voter_row_{i}_uncrypted_ballot.json` for each voter `{i}`.

You can now tell your load test to upload the encrypted ballot file `voter_row_{i}_crypted_ballot.json`.
