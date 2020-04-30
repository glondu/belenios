#!/usr/bin/python
# coding: utf-8
import unittest
import random
import os
import sys
from distutils.util import strtobool
from util.election_testing import verify_election_consistency, create_election_data_snapshot, delete_election_data_snapshot, election_id_to_election_home_page_url, populate_credential_and_password_for_voters_from_sent_emails, populate_random_votes_for_voters
from util.execution import console_log, ConsoleLogDuration
from test_scenario_2 import BeleniosTestElectionScenario2Base, initialize_browser_for_scenario_2
from test_smart_monkey import smart_monkey_votes
import settings


class BeleniosTestElectionScenario2WithMonkeys(BeleniosTestElectionScenario2Base):

    def test_scenario_2_manual_vote_with_monkeys(self):
        console_log("### Running test method BeleniosTestElectionScenario2WithMonkeys::test_scenario_2_manual_vote_with_monkeys()")
        with ConsoleLogDuration("### administrator_starts_creation_of_manual_election"):
            self.administrator_starts_creation_of_manual_election()

        with ConsoleLogDuration("### credential_authority_sends_credentials_to_voters"):
            self.credential_authority_sends_credentials_to_voters()

        with ConsoleLogDuration("### administrator_invites_trustees"):
            self.administrator_invites_trustees()

        with ConsoleLogDuration("### trustees_generate_election_private_keys"):
            self.trustees_generate_election_private_keys()

        with ConsoleLogDuration("### administrator_completes_creation_of_election"):
            self.administrator_completes_creation_of_election()

        with ConsoleLogDuration("### verify_election_consistency using `belenios_tool verify` (#0)"):
            verify_election_consistency(self.election_id)

        voters_who_will_vote = random.sample(self.voters_email_addresses, settings.NUMBER_OF_VOTING_VOTERS)
        start_index_of_voters_who_vote_in_first_part = 0
        end_index_of_voters_who_vote_in_first_part = settings.NUMBER_OF_VOTING_VOTERS_IN_FIRST_PART
        console_log(f"number of (normal) voters who will vote in first part: {end_index_of_voters_who_vote_in_first_part} (indexes {start_index_of_voters_who_vote_in_first_part} included to {end_index_of_voters_who_vote_in_first_part} excluded)")
        start_index_of_voters_who_vote_in_second_part = end_index_of_voters_who_vote_in_first_part
        end_index_of_voters_who_vote_in_second_part = end_index_of_voters_who_vote_in_first_part + settings.NUMBER_OF_MONKEY_VOTING_VOTERS
        console_log(f"number of (smart monkey) voters who will vote in second part: {end_index_of_voters_who_vote_in_second_part - start_index_of_voters_who_vote_in_second_part} (indexes {start_index_of_voters_who_vote_in_second_part} included to {end_index_of_voters_who_vote_in_second_part} excluded)")
        start_index_of_voters_who_vote_in_third_part = end_index_of_voters_who_vote_in_second_part
        end_index_of_voters_who_vote_in_third_part = settings.NUMBER_OF_VOTING_VOTERS
        console_log(f"number of (normal) voters who will vote in third part: {end_index_of_voters_who_vote_in_third_part - start_index_of_voters_who_vote_in_third_part} (indexes {start_index_of_voters_who_vote_in_third_part} included to {end_index_of_voters_who_vote_in_third_part} excluded)")
        verify_every_x_votes = 5

        with ConsoleLogDuration("### some_voters_vote_in_sequences (first part)"):
            self.some_voters_vote_in_sequences(voters_who_will_vote, start_index=start_index_of_voters_who_vote_in_first_part, end_index=end_index_of_voters_who_vote_in_first_part, verify_every_x_votes=verify_every_x_votes)

        with ConsoleLogDuration("### smart monkeys vote (second part)"):
            smart_monkey_voters_who_will_vote_now = voters_who_will_vote[start_index_of_voters_who_vote_in_second_part:end_index_of_voters_who_vote_in_second_part]
            timeout = settings.EXPLICIT_WAIT_TIMEOUT
            voters_who_will_vote_now_data = populate_credential_and_password_for_voters_from_sent_emails(self.fake_sent_emails_manager, smart_monkey_voters_who_will_vote_now, settings.ELECTION_TITLE)
            voters_who_will_vote_now_data = populate_random_votes_for_voters(voters_who_will_vote_now_data)
            self.update_voters_data(voters_who_will_vote_now_data)
            for idx, voter in enumerate(voters_who_will_vote_now_data):
                console_log(f"#### Voting as smart monkey {idx+1} of {settings.NUMBER_OF_MONKEY_VOTING_VOTERS}")
                voter_email_address = voter["email_address"]
                voter_username = voter["username"]
                voter_password = voter["password"]
                voter_credential = voter["credential"]
                voter_decided_vote = voter["votes"]
                election_url = voter["election_page_url"] # this is the same as `election_id_to_election_home_page_url(self.election_id)`
                smart_ballot_tracker = smart_monkey_votes(self.browser, timeout, election_url, voter_username, voter_password, voter_credential, voter_decided_vote)
                if smart_ballot_tracker:
                    voter["smart_ballot_tracker"] = smart_ballot_tracker
                else:
                    raise Exception("Monkey voter did not complete its vote properly")
                self.voters_email_addresses_who_have_voted[voter_email_address] = True
                self.browser.quit()
                self.browser = initialize_browser_for_scenario_2()

        with ConsoleLogDuration("### some_voters_vote_in_sequences (third part)"):
            self.some_voters_vote_in_sequences(voters_who_will_vote, start_index=start_index_of_voters_who_vote_in_third_part, end_index=end_index_of_voters_who_vote_in_third_part, verify_every_x_votes=verify_every_x_votes)

        with ConsoleLogDuration("### verify_election_consistency using `belenios_tool verify` (#1)"):
            verify_election_consistency(self.election_id)

        with ConsoleLogDuration("### Starting step: create_election_data_snapshot (#0)"):
            snapshot_folder = create_election_data_snapshot(self.election_id)
            console_log("snapshot_folder: ", snapshot_folder)

        try:
            with ConsoleLogDuration("### some_voters_revote"):
                self.some_voters_revote()

            with ConsoleLogDuration("### verify_election_consistency using `belenios_tool verify-diff` (#2)"):
                verify_election_consistency(self.election_id, snapshot_folder)
        finally:
            with ConsoleLogDuration("### delete_election_data_snapshot"):
                delete_election_data_snapshot(snapshot_folder)

        with ConsoleLogDuration("### verify_election_consistency using `belenios_tool verify` (#3)"):
            verify_election_consistency(self.election_id)

        with ConsoleLogDuration("### administrator_starts_tallying_of_election"):
            self.administrator_starts_tallying_of_election()

        with ConsoleLogDuration("### trustees_do_partial_decryption"):
            self.trustees_do_partial_decryption()

        with ConsoleLogDuration("### administrator_finishes_tallying_of_election"):
            self.administrator_finishes_tallying_of_election()

        with ConsoleLogDuration("### verify_election_consistency using `belenios_tool verify` (#4)"):
            verify_election_consistency(self.election_id)


if __name__ == "__main__":
    random_seed = os.getenv('RANDOM_SEED', None)
    if not random_seed:
        random_seed = random.randrange(sys.maxsize)
    console_log("Python random seed being used:", random_seed)
    random.seed(random_seed)

    if os.getenv('USE_HEADLESS_BROWSER', None):
        settings.USE_HEADLESS_BROWSER = bool(strtobool(os.getenv('USE_HEADLESS_BROWSER')))

    settings.SENT_EMAILS_TEXT_FILE_ABSOLUTE_PATH = os.getenv('SENT_EMAILS_TEXT_FILE_ABSOLUTE_PATH', settings.SENT_EMAILS_TEXT_FILE_ABSOLUTE_PATH)
    settings.WAIT_TIME_BETWEEN_EACH_STEP = float(os.getenv('WAIT_TIME_BETWEEN_EACH_STEP', settings.WAIT_TIME_BETWEEN_EACH_STEP))

    settings.EXPLICIT_WAIT_TIMEOUT = int(os.getenv('EXPLICIT_WAIT_TIMEOUT', settings.EXPLICIT_WAIT_TIMEOUT))
    if os.getenv('CLEAN_UP_POLICY', None):
        input_clean_up_policy = os.getenv('CLEAN_UP_POLICY')
        if hasattr(settings.CLEAN_UP_POLICIES, input_clean_up_policy):
            settings.CLEAN_UP_POLICY = getattr(settings.CLEAN_UP_POLICIES, input_clean_up_policy)
        else:
            raise Exception("Error: Unknown value for CLEAN_UP_POLICY:", input_clean_up_policy)

    settings.NUMBER_OF_INVITED_VOTERS = int(os.getenv('NUMBER_OF_INVITED_VOTERS', settings.NUMBER_OF_INVITED_VOTERS))
    settings.NUMBER_OF_VOTING_VOTERS = int(os.getenv('NUMBER_OF_VOTING_VOTERS', settings.NUMBER_OF_VOTING_VOTERS))
    settings.NUMBER_OF_MONKEY_VOTING_VOTERS = int(os.getenv('NUMBER_OF_MONKEY_VOTING_VOTERS', settings.NUMBER_OF_MONKEY_VOTING_VOTERS))
    settings.NUMBER_OF_VOTING_VOTERS_IN_FIRST_PART = int(os.getenv('NUMBER_OF_VOTING_VOTERS_IN_FIRST_PART', settings.NUMBER_OF_VOTING_VOTERS_IN_FIRST_PART))
    settings.NUMBER_OF_REVOTING_VOTERS = int(os.getenv('NUMBER_OF_REVOTING_VOTERS', settings.NUMBER_OF_REVOTING_VOTERS))
    settings.NUMBER_OF_REGENERATED_PASSWORD_VOTERS = int(os.getenv('NUMBER_OF_REGENERATED_PASSWORD_VOTERS', settings.NUMBER_OF_REGENERATED_PASSWORD_VOTERS))
    settings.ADMINISTRATOR_USERNAME = os.getenv('ADMINISTRATOR_USERNAME', settings.ADMINISTRATOR_USERNAME)
    settings.ADMINISTRATOR_PASSWORD = os.getenv('ADMINISTRATOR_PASSWORD', settings.ADMINISTRATOR_PASSWORD)
    settings.ELECTION_TITLE = os.getenv('ELECTION_TITLE', settings.ELECTION_TITLE)
    settings.ELECTION_DESCRIPTION = os.getenv('ELECTION_DESCRIPTION', settings.ELECTION_DESCRIPTION)
    settings.INITIATOR_CONTACT = os.getenv('INITIATOR_CONTACT', settings.INITIATOR_CONTACT)
    settings.BROWSER_DOWNLOAD_FOLDER = os.getenv('BROWSER_DOWNLOAD_FOLDER', settings.BROWSER_DOWNLOAD_FOLDER)
    settings.ADMINISTRATOR_EMAIL_ADDRESS = os.getenv('ADMINISTRATOR_EMAIL_ADDRESS', settings.ADMINISTRATOR_EMAIL_ADDRESS)
    settings.CREDENTIAL_AUTHORITY_EMAIL_ADDRESS = os.getenv('CREDENTIAL_AUTHORITY_EMAIL_ADDRESS', settings.CREDENTIAL_AUTHORITY_EMAIL_ADDRESS)
    # TODO: settings.TRUSTEES_EMAIL_ADDRESSES (it cannot be manipulated the same way because it is an array)

    console_log("USE_HEADLESS_BROWSER:", settings.USE_HEADLESS_BROWSER)
    console_log("SENT_EMAILS_TEXT_FILE_ABSOLUTE_PATH:", settings.SENT_EMAILS_TEXT_FILE_ABSOLUTE_PATH)
    console_log("WAIT_TIME_BETWEEN_EACH_STEP:", settings.WAIT_TIME_BETWEEN_EACH_STEP)
    console_log("EXPLICIT_WAIT_TIMEOUT:", settings.EXPLICIT_WAIT_TIMEOUT)
    console_log("CLEAN_UP_POLICY:", settings.CLEAN_UP_POLICY)

    console_log("NUMBER_OF_INVITED_VOTERS:", settings.NUMBER_OF_INVITED_VOTERS)
    console_log("NUMBER_OF_VOTING_VOTERS:", settings.NUMBER_OF_VOTING_VOTERS)
    console_log("NUMBER_OF_MONKEY_VOTING_VOTERS:", settings.NUMBER_OF_MONKEY_VOTING_VOTERS)
    console_log("NUMBER_OF_VOTING_VOTERS_IN_FIRST_PART:", settings.NUMBER_OF_VOTING_VOTERS_IN_FIRST_PART)
    console_log("NUMBER_OF_REVOTING_VOTERS:", settings.NUMBER_OF_REVOTING_VOTERS)
    console_log("NUMBER_OF_REGENERATED_PASSWORD_VOTERS:", settings.NUMBER_OF_REGENERATED_PASSWORD_VOTERS)
    console_log("ELECTION_TITLE:", settings.ELECTION_TITLE)
    console_log("ELECTION_DESCRIPTION:", settings.ELECTION_DESCRIPTION)
    console_log("INITIATOR_CONTACT:", settings.INITIATOR_CONTACT)
    console_log("BROWSER_DOWNLOAD_FOLDER:", settings.BROWSER_DOWNLOAD_FOLDER)
    console_log("ADMINISTRATOR_EMAIL_ADDRESS:", settings.ADMINISTRATOR_EMAIL_ADDRESS)
    console_log("CREDENTIAL_AUTHORITY_EMAIL_ADDRESS:", settings.CREDENTIAL_AUTHORITY_EMAIL_ADDRESS)
    console_log("TRUSTEES_EMAIL_ADDRESSES:", settings.TRUSTEES_EMAIL_ADDRESSES)

    unittest.main()
