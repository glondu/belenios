#!/usr/bin/python
# -*- coding: utf-8 -*
import unittest
import time
import string
import random
import os
import shutil
import subprocess
import re
import sys
from distutils.util import strtobool
from selenium import webdriver
from selenium.webdriver.common.keys import Keys
from selenium.webdriver.common.alert import Alert
from util.fake_sent_emails_manager import FakeSentEmailsManager
from util.selenium_tools import wait_for_element_exists, wait_for_elements_exist, wait_for_element_exists_and_contains_expected_text, wait_for_element_exists_and_has_non_empty_content, wait_for_an_element_with_partial_link_text_exists
from test_scenario_1 import console_log, remove_database_folder, initialize_server, initialize_browser, log_in_as_administrator, log_out, wait_a_bit
import settings

settings.USE_HEADLESS_BROWSER = False


class BeleniosTestElectionScenario2(unittest.TestCase):
    """
    Properties:
    - server
    - browser
    - voters_email_addresses
    - voters_email_addresses_who_have_lost_their_password
    - voters_data
    - election_page_url
    - election_id
    """

    def setUp(self):
        self.fake_sent_emails_manager = FakeSentEmailsManager(settings.SENT_EMAILS_TEXT_FILE_ABSOLUTE_PATH)
        self.fake_sent_emails_manager.install_fake_sendmail_log_file()

        remove_database_folder()

        self.server = initialize_server()

        self.browser = initialize_browser()

        self.voters_email_addresses = []
        self.voters_email_addresses_who_have_lost_their_password = []
        self.voters_data = dict()
        self.election_page_url = None
        self.election_id = None


    def tearDown(self):
        self.browser.quit()

        self.server.kill()

        remove_database_folder()

        self.fake_sent_emails_manager.uninstall_fake_sendmail_log_file()


    def administrator_starts_creation_of_manual_election(self):
        # # Setting up a new election (action of the administrator)

        browser = self.browser

        # Edith has been given administrator rights on an online voting app called Belenios. She goes
        # to check out its homepage and logs in
        log_in_as_administrator(browser)

        # She clicks on the "Prepare a new election" link
        create_election_link_expected_content = "Prepare a new election"
        links_css_selector = "#main a"
        create_election_link_element = wait_for_element_exists_and_contains_expected_text(browser, links_css_selector, create_election_link_expected_content, settings.EXPLICIT_WAIT_TIMEOUT)
        create_election_link_element.click()

        wait_a_bit()

        # TODO: continue creation of election


    def test_scenario_2_manual_vote(self):
        console_log("### Starting step: administrator_starts_creation_of_manual_election")
        self.administrator_starts_creation_of_manual_election()
        console_log("### Step complete: administrator_starts_creation_of_manual_election")


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
    settings.NUMBER_OF_INVITED_VOTERS = int(os.getenv('NUMBER_OF_INVITED_VOTERS', settings.NUMBER_OF_INVITED_VOTERS))
    settings.NUMBER_OF_VOTING_VOTERS = int(os.getenv('NUMBER_OF_VOTING_VOTERS', settings.NUMBER_OF_VOTING_VOTERS))
    settings.NUMBER_OF_REVOTING_VOTERS = int(os.getenv('NUMBER_OF_REVOTING_VOTERS', settings.NUMBER_OF_REVOTING_VOTERS))
    settings.NUMBER_OF_REGENERATED_PASSWORD_VOTERS = int(os.getenv('NUMBER_OF_REGENERATED_PASSWORD_VOTERS', settings.NUMBER_OF_REGENERATED_PASSWORD_VOTERS))
    settings.ADMINISTRATOR_USERNAME = os.getenv('ADMINISTRATOR_USERNAME', settings.ADMINISTRATOR_USERNAME)
    settings.ADMINISTRATOR_PASSWORD = os.getenv('ADMINISTRATOR_PASSWORD', settings.ADMINISTRATOR_PASSWORD)

    console_log("USE_HEADLESS_BROWSER:", settings.USE_HEADLESS_BROWSER)
    console_log("SENT_EMAILS_TEXT_FILE_ABSOLUTE_PATH:", settings.SENT_EMAILS_TEXT_FILE_ABSOLUTE_PATH)
    console_log("WAIT_TIME_BETWEEN_EACH_STEP:", settings.WAIT_TIME_BETWEEN_EACH_STEP)
    console_log("EXPLICIT_WAIT_TIMEOUT:", settings.EXPLICIT_WAIT_TIMEOUT)
    console_log("NUMBER_OF_INVITED_VOTERS:", settings.NUMBER_OF_INVITED_VOTERS)
    console_log("NUMBER_OF_VOTING_VOTERS:", settings.NUMBER_OF_VOTING_VOTERS)
    console_log("NUMBER_OF_REVOTING_VOTERS:", settings.NUMBER_OF_REVOTING_VOTERS)
    console_log("NUMBER_OF_REGENERATED_PASSWORD_VOTERS:", settings.NUMBER_OF_REGENERATED_PASSWORD_VOTERS)

    unittest.main()
