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
from test_scenario_1 import console_log, remove_database_folder, initialize_server, initialize_browser, log_in_as_administrator, log_out, wait_a_bit, verify_element_label, random_email_addresses_generator, administrator_starts_creation_of_election, administrator_edits_election_questions, administrator_sets_election_voters, build_css_selector_to_find_buttons_in_page_content_by_value
import settings


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

        # Alice has been given administrator rights on an online voting app called Belenios. She goes
        # to check out its homepage and logs in
        log_in_as_administrator(browser)

        # She starts creation of the election:
        # - She clicks on the "Prepare a new election" link
        # - She picks the Credential management method: manual
        # (- She keeps default value for Authentication method: it is Password, not CAS)
        # - She clicks on the "Proceed" button (this redirects to the "Preparation of election" page)
        # - She changes values of fields name and description of the election
        # - She clicks on the "Save changes button" (the one that is next to the election description field)
        administrator_starts_creation_of_election(browser, True)

        # She edits election's questions:
        # - She clicks on the "Edit questions" link, to write her own questions
        # - She arrives on the Questions page. She checks that the page title is correct
        # - She removes answer 3
        # - She clicks on the "Save changes" button (this redirects to the "Preparation of election" page)
        administrator_edits_election_questions(browser)

        # She sets election's voters:
        # - She clicks on the "Edit voters" link, to then type the list of voters
        # - She types N e-mail addresses (the list of invited voters)
        # - She clicks on the "Add" button to submit changes
        # - She clicks on "Return to draft page" link
        self.voters_email_addresses = random_email_addresses_generator(settings.NUMBER_OF_INVITED_VOTERS)
        administrator_sets_election_voters(browser, self.voters_email_addresses)

        # In "Authentication" section, she clicks on the "Generate and mail missing passwords" button
        generate_and_mail_missing_passwords_button_label = "Generate and mail missing passwords"
        generate_and_mail_missing_passwords_button_element = wait_for_element_exists(browser, build_css_selector_to_find_buttons_in_page_content_by_value(generate_and_mail_missing_passwords_button_label), settings.EXPLICIT_WAIT_TIMEOUT)
        generate_and_mail_missing_passwords_button_element.click()

        wait_a_bit()

        # She checks that the page contains expected confirmation text, instead of an error (TODO: explain in which case an error can happen, and check that it does not show)
        confirmation_sentence_expected_text = "Passwords have been generated and mailed!"
        confirmation_sentence_css_selector = "#main p"
        wait_for_element_exists_and_contains_expected_text(browser, confirmation_sentence_css_selector, confirmation_sentence_expected_text, settings.EXPLICIT_WAIT_TIMEOUT)

        # She clicks on the "Proceed" link (this redirects to the "Preparation of election" page)
        proceed_link_expected_label = "Proceed"
        proceed_link_css_selector = "#main a"
        proceed_link_element = wait_for_element_exists_and_contains_expected_text(browser, proceed_link_css_selector, proceed_link_expected_label, settings.EXPLICIT_WAIT_TIMEOUT)
        proceed_link_element.click()

        wait_a_bit()

        # In "Credentials" section, she clicks on "Credential management" link
        credential_management_expected_label = "Credential management"
        credential_management_link_element = wait_for_an_element_with_partial_link_text_exists(browser, credential_management_expected_label)
        credential_management_link_element.click()

        # She remembers the link displayed
        link_for_credential_authority_css_selector = "#main a"
        link_for_credential_authority_element = wait_for_element_exists_and_has_non_empty_content(browser, link_for_credential_authority_css_selector)
        link_label = link_for_credential_authority_element.get_attribute('innerText').strip()
        self.certificate_authority_link = link_label

        # She sends the remembered link to the certificate authority by email (actually we don't need to send anything because we will act as the certificate authority)

        # She closes the browser. Cecily, the Certificate Authority, receives the email sent by Alice, and opens the link in it.
        browser.quit()
        self.browser = initialize_browser()
        browser = self.browser
        browser.get(self.certificate_authority_link)

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
    settings.ELECTION_TITLE = os.getenv('ELECTION_TITLE', settings.ELECTION_TITLE)
    settings.ELECTION_DESCRIPTION = os.getenv('ELECTION_DESCRIPTION', settings.ELECTION_DESCRIPTION)

    console_log("USE_HEADLESS_BROWSER:", settings.USE_HEADLESS_BROWSER)
    console_log("SENT_EMAILS_TEXT_FILE_ABSOLUTE_PATH:", settings.SENT_EMAILS_TEXT_FILE_ABSOLUTE_PATH)
    console_log("WAIT_TIME_BETWEEN_EACH_STEP:", settings.WAIT_TIME_BETWEEN_EACH_STEP)
    console_log("EXPLICIT_WAIT_TIMEOUT:", settings.EXPLICIT_WAIT_TIMEOUT)
    console_log("NUMBER_OF_INVITED_VOTERS:", settings.NUMBER_OF_INVITED_VOTERS)
    console_log("NUMBER_OF_VOTING_VOTERS:", settings.NUMBER_OF_VOTING_VOTERS)
    console_log("NUMBER_OF_REVOTING_VOTERS:", settings.NUMBER_OF_REVOTING_VOTERS)
    console_log("NUMBER_OF_REGENERATED_PASSWORD_VOTERS:", settings.NUMBER_OF_REGENERATED_PASSWORD_VOTERS)
    console_log("ELECTION_TITLE:", settings.ELECTION_TITLE)
    console_log("ELECTION_DESCRIPTION:", settings.ELECTION_DESCRIPTION)

    unittest.main()
