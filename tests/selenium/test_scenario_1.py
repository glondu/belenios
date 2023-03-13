#!/usr/bin/python
# coding: utf-8
import unittest
import random
import os
import sys
from selenium.webdriver.support.select import Select
from util.fake_sent_emails_manager import FakeSentEmailsManager
from util.selenium_tools import wait_for_element_exists, wait_for_an_element_with_partial_link_text_exists, wait_for_an_element_with_link_text_exists
from util.election_testing import strtobool, remove_database_folder, remove_election_from_database, wait_a_bit, build_css_selector_to_find_buttons_in_page_content_by_value, initialize_server, initialize_browser, verify_election_consistency, create_election_data_snapshot, delete_election_data_snapshot, log_in_as_administrator
from util.election_test_base import BeleniosElectionTestBase
from util.execution import console_log, ConsoleLogDuration
import settings


class BeleniosTestElectionScenario1(BeleniosElectionTestBase):
    def setUp(self):
        self.fake_sent_emails_manager = FakeSentEmailsManager(settings.SENT_EMAILS_TEXT_FILE_ABSOLUTE_PATH)
        self.fake_sent_emails_manager.install_fake_sendmail_log_file()

        if settings.CLEAN_UP_POLICY == settings.CLEAN_UP_POLICIES.REMOVE_DATABASE:
            remove_database_folder()
        elif settings.CLEAN_UP_POLICY == settings.CLEAN_UP_POLICIES.REMOVE_ELECTION:
            pass

        self.server = initialize_server()

        self.browser = initialize_browser()


    def tearDown(self):
        self.browser.quit()

        self.server.kill()

        if settings.CLEAN_UP_POLICY == settings.CLEAN_UP_POLICIES.REMOVE_DATABASE:
            remove_database_folder()
        elif settings.CLEAN_UP_POLICY == settings.CLEAN_UP_POLICIES.REMOVE_ELECTION:
            if self.election_id:
                remove_election_from_database(self.election_id)

        self.fake_sent_emails_manager.uninstall_fake_sendmail_log_file()


    def administrator_does_tallying_of_election(self):
        browser = self.browser

        # Alice goes to the election page
        election_url = self.election_page_url # Could also be obtained with self.voters_data[self.voters_email_addresses[0]]["election_page_url"]
        browser.get(election_url)

        wait_a_bit()

        # She clicks on "en" language
        select = Select(wait_for_element_exists(browser, ".lang_box select", settings.EXPLICIT_WAIT_TIMEOUT))
        select.select_by_value("en")

        wait_a_bit()

        # She clicks on the "Administer this election" link
        administration_link_label = "Administer this election"
        administration_link_element = wait_for_an_element_with_partial_link_text_exists(browser, administration_link_label, settings.EXPLICIT_WAIT_TIMEOUT)
        administration_link_element.click()

        # She logs in as administrator
        log_in_as_administrator(browser, from_a_login_page=True)

        wait_a_bit()

        # She clicks on the "Close election" button
        close_election_button_label = "Close election"
        close_election_button_css_selector = build_css_selector_to_find_buttons_in_page_content_by_value(close_election_button_label)
        close_election_button_element = wait_for_element_exists(browser, close_election_button_css_selector, settings.EXPLICIT_WAIT_TIMEOUT)
        close_election_button_element.click()

        wait_a_bit()

        # She clicks on the "Proceed to vote counting" button
        proceed_button_label = "Proceed to vote counting"
        proceed_button_css_selector = build_css_selector_to_find_buttons_in_page_content_by_value(proceed_button_label)
        proceed_button_element = wait_for_element_exists(browser, proceed_button_css_selector, settings.EXPLICIT_WAIT_TIMEOUT)
        proceed_button_element.click()

        wait_a_bit()

        # She clicks on "Election home"
        election_home_element = wait_for_an_element_with_link_text_exists(browser, "Election home", settings.EXPLICIT_WAIT_TIMEOUT)
        election_home_element.click()

        wait_a_bit()

        self.administrator_verifies_vote_results()


    def test_scenario_1_simple_vote(self):
        with ConsoleLogDuration("### administrator_creates_election"):
            self.administrator_creates_election()

        with ConsoleLogDuration("### administrator_regenerates_passwords_for_some_voters"):
            self.administrator_regenerates_passwords_for_some_voters()

        with ConsoleLogDuration("### verify_election_consistency using `belenios_tool verify` (#0)"):
            verify_election_consistency(self.election_id)

        with ConsoleLogDuration("### all_voters_vote_in_sequences"):
            self.all_voters_vote_in_sequences()

        with ConsoleLogDuration("### verify_election_consistency using `belenios_tool verify` (#1)"):
            verify_election_consistency(self.election_id)

        with ConsoleLogDuration("### create_election_data_snapshot (#0)"):
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

        with ConsoleLogDuration("### administrator_does_tallying_of_election"):
            self.administrator_does_tallying_of_election()

        with ConsoleLogDuration("### verify_election_consistency using `belenios_tool verify` (#4)"):
            verify_election_consistency(self.election_id)

        with ConsoleLogDuration("### one_voter_revotes_after_the_election_is_closed"):
            self.one_voter_revotes_after_the_election_is_closed()



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
    console_log("CLEAN_UP_POLICY:", settings.CLEAN_UP_POLICY)

    console_log("NUMBER_OF_INVITED_VOTERS:", settings.NUMBER_OF_INVITED_VOTERS)
    console_log("NUMBER_OF_VOTING_VOTERS:", settings.NUMBER_OF_VOTING_VOTERS)
    console_log("NUMBER_OF_REVOTING_VOTERS:", settings.NUMBER_OF_REVOTING_VOTERS)
    console_log("NUMBER_OF_REGENERATED_PASSWORD_VOTERS:", settings.NUMBER_OF_REGENERATED_PASSWORD_VOTERS)
    console_log("ELECTION_TITLE:", settings.ELECTION_TITLE)
    console_log("ELECTION_DESCRIPTION:", settings.ELECTION_DESCRIPTION)

    unittest.main()
