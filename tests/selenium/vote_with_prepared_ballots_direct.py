#!/usr/bin/python
# coding: utf-8
import unittest
import os
import csv
import json
import re
import urllib.request
import base64
from util.election_testing import strtobool, console_log
from util.execution import ConsoleLogDuration, try_several_times
import settings


class BeleniosVoteWithPreparedBallots(unittest.TestCase):

    def __init__(self, *args, **kw):
        super().__init__(*args, **kw)


    def cast_all_votes_from_csv(self):
        generated_files_destination_folder = settings.GENERATED_FILES_DESTINATION_FOLDER
        csv_file_path = os.path.join(generated_files_destination_folder, 'all_votes.csv')
        pat = re.compile(r'^(.+)/election#([^/]+)$')
        with open(csv_file_path, 'r', newline='') as csvfile:
            csvreader = csv.DictReader(csvfile, delimiter=',', quotechar='|')
            current_row = 0
            for row in csvreader:
                current_row += 1
                if current_row <= settings.SKIP_ROWS_IN_CSV_FILE:
                    continue

                voter_email_address = row['voter_email_address']
                auth_dict = {
                    "username": voter_email_address,
                    "password": row['voter_password']
                }
                auth_json = json.dumps(auth_dict)
                auth_token = base64.b64encode(auth_json.encode())
                headers = {
                    "Content-Type": b"application/json",
                    "Authorization": b"Bearer " + auth_token
                }

                voter_encrypted_ballot_file_name = row['voter_encrypted_ballot_file_name']
                with open(os.path.join(generated_files_destination_folder, voter_encrypted_ballot_file_name), "r") as f:
                    data = f.read().strip().encode()

                election_page_url = row['election_page_url']
                mat = pat.match(election_page_url)
                prefix = mat.group(1)
                uuid = mat.group(2)

                with ConsoleLogDuration(f"Row {current_row} (voter {voter_email_address})"):
                    req = urllib.request.urlopen(urllib.request.Request(prefix + "/api/elections/" + uuid + "/ballots", data=data, headers=headers))
                    if req.status != 200:
                        console_log("failure")


    def test_vote_with_prepared_ballots(self):
        # Generate ballot for each voter
        console_log("### Starting step: cast_all_votes_from_csv")
        self.cast_all_votes_from_csv()
        console_log("### Step complete: cast_all_votes_from_csv")


if __name__ == "__main__":
    settings.GENERATED_FILES_DESTINATION_FOLDER = os.getenv('GENERATED_FILES_DESTINATION_FOLDER', settings.GENERATED_FILES_DESTINATION_FOLDER)
    settings.SKIP_ROWS_IN_CSV_FILE = int(os.getenv('SKIP_ROWS_IN_CSV_FILE', 0))

    console_log("GENERATED_FILES_DESTINATION_FOLDER:", settings.GENERATED_FILES_DESTINATION_FOLDER)
    console_log("SKIP_ROWS_IN_CSV_FILE:", settings.SKIP_ROWS_IN_CSV_FILE)

    unittest.main()
