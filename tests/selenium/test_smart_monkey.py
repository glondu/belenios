#!/usr/bin/python
# coding: utf-8
import unittest
import random
import os
import sys
import time
from distutils.util import strtobool

from util.election_testing import election_id_to_election_home_page_url
from util.page_objects import BallotBoxPage
from util.state_machine import ElectionHomePageState, NormalVoteStep6PageState
from util.monkeys import SmartMonkeyWithMemoryAndKnownStateMachine
from util.execution import console_log
from test_fuzz_vote import BeleniosTestElectionWithCreationBase
import settings


def smart_monkey_votes(browser, timeout, election_url, voter_username, voter_password, voter_credential, voter_decided_vote=None):
    console_log("## Going to election page:", election_url)
    browser.get(election_url)

    election_home_page_state = ElectionHomePageState(browser, timeout)
    election_home_page_state.page.click_on_language_link("en")
    in_memory = {
        "voter_username": voter_username,
        "voter_password": voter_password,
        "voter_credential": voter_credential,
    }
    if voter_decided_vote:
        in_memory["voter_decided_vote"] = voter_decided_vote
    smart_monkey = SmartMonkeyWithMemoryAndKnownStateMachine(election_home_page_state, in_memory=in_memory)
    console_log(f"smart_monkey.current_state: {smart_monkey.current_state}")
    current_iteration = 1
    while not isinstance(smart_monkey.current_state, NormalVoteStep6PageState):
        smart_monkey.verify_page()
        current_iteration += 1
        console_log(f"executing action number {current_iteration}")
        try:
            executed_action = smart_monkey.execute_a_random_action()
            console_log(f"executed action was: {executed_action}")
        except Exception as e:
            console_log(f"Exception while executing `smart_monkey.execute_a_random_action()`. Page state was {smart_monkey.current_state} and exception was: {repr(e)}")
            time.sleep(10)
            raise Exception("Exception while executing `smart_monkey.execute_a_random_action()`") from e
        console_log(f"smart_monkey.current_state: {smart_monkey.current_state}")

    if isinstance(smart_monkey.current_state, NormalVoteStep6PageState):
        console_log("Ending monkey behaviour here because we have completed the vote")
        smart_monkey.verify_page()
        console_log("Clicking on the ballot box link and verifying presence of voter's smart ballot tracker")
        smart_monkey.current_state.page.click_on_ballot_box_link()
        ballot_box_page = BallotBoxPage(browser, timeout)
        voter_validated_smart_ballot_tracker = smart_monkey.get_memory_element("voter_validated_smart_ballot_tracker")
        ballot_box_page.verify_page(voter_validated_smart_ballot_tracker)
        return voter_validated_smart_ballot_tracker


class BeleniosMonkeyTestClicker(BeleniosTestElectionWithCreationBase):
    def test_very_smart_monkey_votes(self):
        console_log("# test_very_smart_monkey_votes()")
        browser = self.browser
        timeout = settings.EXPLICIT_WAIT_TIMEOUT
        election_url = election_id_to_election_home_page_url(self.election_id)

        smart_monkey_votes(browser, timeout, election_url, settings.VOTER_USERNAME, settings.VOTER_PASSWORD, settings.VOTER_CREDENTIAL)


if __name__ == "__main__":
    random_seed = os.getenv('RANDOM_SEED', None)
    if not random_seed:
        random_seed = random.randrange(sys.maxsize)
    console_log("Python random seed being used:", random_seed)
    random.seed(random_seed)

    settings.SERVER_URL = os.getenv('SERVER_URL', settings.SERVER_URL)
    if os.getenv('START_SERVER', None):
        settings.START_SERVER = bool(strtobool(os.getenv('START_SERVER')))

    if os.getenv('USE_HEADLESS_BROWSER', None):
        settings.USE_HEADLESS_BROWSER = bool(strtobool(os.getenv('USE_HEADLESS_BROWSER')))

    settings.ELECTION_ID = os.getenv('ELECTION_ID', None) or None
    settings.VOTER_USERNAME = os.getenv('VOTER_USERNAME', None) or None
    settings.VOTER_PASSWORD = os.getenv('VOTER_PASSWORD', None) or None
    settings.VOTER_CREDENTIAL = os.getenv('VOTER_CREDENTIAL', None) or None
    settings.FAKE_SENT_EMAILS_FILE_RELATIVE_URL = os.getenv('FAKE_SENT_EMAILS_FILE_RELATIVE_URL', "static/mail.txt")

    settings.SENT_EMAILS_TEXT_FILE_ABSOLUTE_PATH = os.getenv('SENT_EMAILS_TEXT_FILE_ABSOLUTE_PATH', settings.SENT_EMAILS_TEXT_FILE_ABSOLUTE_PATH)
    settings.WAIT_TIME_BETWEEN_EACH_STEP = float(os.getenv('WAIT_TIME_BETWEEN_EACH_STEP', settings.WAIT_TIME_BETWEEN_EACH_STEP)) # Do not set a value below 0.02 seconds, otherwise hypothesis test becomes flaky.
    settings.EXPLICIT_WAIT_TIMEOUT = int(os.getenv('EXPLICIT_WAIT_TIMEOUT', settings.EXPLICIT_WAIT_TIMEOUT))
    if os.getenv('CLEAN_UP_POLICY', None):
        input_clean_up_policy = os.getenv('CLEAN_UP_POLICY')
        if hasattr(settings.CLEAN_UP_POLICIES, input_clean_up_policy):
            settings.CLEAN_UP_POLICY = getattr(settings.CLEAN_UP_POLICIES, input_clean_up_policy)
        else:
            raise Exception("Error: Unknown value for CLEAN_UP_POLICY:", input_clean_up_policy)

    settings.NUMBER_OF_INVITED_VOTERS = int(os.getenv('NUMBER_OF_INVITED_VOTERS', settings.NUMBER_OF_INVITED_VOTERS))
    settings.NUMBER_OF_VOTING_VOTERS = int(os.getenv('NUMBER_OF_VOTING_VOTERS', settings.NUMBER_OF_VOTING_VOTERS))
    settings.NUMBER_OF_REVOTING_VOTERS = int(os.getenv('NUMBER_OF_REVOTING_VOTERS', settings.NUMBER_OF_REVOTING_VOTERS))
    settings.NUMBER_OF_REGENERATED_PASSWORD_VOTERS = int(os.getenv('NUMBER_OF_REGENERATED_PASSWORD_VOTERS', settings.NUMBER_OF_REGENERATED_PASSWORD_VOTERS))
    settings.LOGIN_MODE = os.getenv('LOGIN_MODE', settings.LOGIN_MODE)
    settings.ADMINISTRATOR_USERNAME = os.getenv('ADMINISTRATOR_USERNAME', settings.ADMINISTRATOR_USERNAME)
    settings.ADMINISTRATOR_PASSWORD = os.getenv('ADMINISTRATOR_PASSWORD', settings.ADMINISTRATOR_PASSWORD)
    settings.ELECTION_TITLE = os.getenv('ELECTION_TITLE', settings.ELECTION_TITLE)
    settings.ELECTION_DESCRIPTION = os.getenv('ELECTION_DESCRIPTION', settings.ELECTION_DESCRIPTION)

    console_log("SERVER_URL:", settings.SERVER_URL)
    console_log("START_SERVER:", settings.START_SERVER)
    console_log("USE_HEADLESS_BROWSER:", settings.USE_HEADLESS_BROWSER)
    console_log("SENT_EMAILS_TEXT_FILE_ABSOLUTE_PATH:", settings.SENT_EMAILS_TEXT_FILE_ABSOLUTE_PATH)
    console_log("WAIT_TIME_BETWEEN_EACH_STEP:", settings.WAIT_TIME_BETWEEN_EACH_STEP)
    console_log("EXPLICIT_WAIT_TIMEOUT:", settings.EXPLICIT_WAIT_TIMEOUT)
    console_log("NUMBER_OF_INVITED_VOTERS:", settings.NUMBER_OF_INVITED_VOTERS)
    console_log("NUMBER_OF_VOTING_VOTERS:", settings.NUMBER_OF_VOTING_VOTERS)
    console_log("NUMBER_OF_REVOTING_VOTERS:", settings.NUMBER_OF_REVOTING_VOTERS)
    console_log("NUMBER_OF_REGENERATED_PASSWORD_VOTERS:", settings.NUMBER_OF_REGENERATED_PASSWORD_VOTERS)
    console_log("LOGIN_MODE:", settings.LOGIN_MODE)
    console_log("ELECTION_TITLE:", settings.ELECTION_TITLE)
    console_log("ELECTION_DESCRIPTION:", settings.ELECTION_DESCRIPTION)

    unittest.main()
